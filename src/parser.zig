const ast = @import("ast.zig");
const token = @import("token.zig");
const Token = token.Token;

const std = @import("std");
const ArrayList = std.ArrayList;

pub const ParseError = error {
    ExpectedTokenOrEOF,
    ExpectedPClose,
    ExpectedPrint,
    ExpectedLineBreak,
    ExpectedIf,
    ExpectedWhile,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedStatement,
};

pub fn expectedTokenError(expected: Token) ParseError {
    return switch (expected) {
        .RPAREN => ParseError.ExpectedPClose,
        .PRINT => ParseError.ExpectedPrint,
        .LB => ParseError.ExpectedLineBreak,
        .IF => ParseError.ExpectedIf,
        .WHILE => ParseError.ExpectedWhile,
        .IDENT => ParseError.ExpectedIdentifier,
        else => unreachable
    };
}

pub const Parser = struct {

    const Self: type = @This();

    tokens: ArrayList(Token),
    allocator: std.mem.Allocator,

    pub fn new(tokens: ArrayList(Token), allocator: std.mem.Allocator) Self {

        // NOTE: Reversing token list to allow
        // using tokens.pop and tokens.getLast
        std.mem.reverse(Token, tokens.items);
        return .{
            .tokens = tokens,
            .allocator = allocator,
        };
    }

    fn nextToken(self: *Self) ParseError!Token {
        return if (self.tokens.popOrNull()) |tok| tok
        else ParseError.ExpectedTokenOrEOF;
    }

    fn peekToken(self: *Self) ?Token {
        return self.tokens.getLastOrNull();
    }

    fn expectToken(self: *Self, tok: Token) ParseError!void {
        const nt: Token = try self.nextToken();
        return if (
            !std.meta.eql(nt, tok)
        ) expectedTokenError(tok);
    }

    fn expectTokenOrEOF(self: *Self, tok: Token) ParseError!void {
        const nt: Token = try self.nextToken();
        return if (
            !std.meta.eql(nt, tok) and !std.meta.eql(nt, Token {.EOF = {}})
        ) expectedTokenError(tok);
    }

    fn parseExprBp(self: *Self, min_bp: u8) ParseError!*ast.Expr {

        // Parse left-hand side of expression
        var lhs: *ast.Expr = undefined;
        const next_token: Token = try self.nextToken();
        switch(next_token) {

            // Skip errors
            .ERROR, .ILLEGAL => {
                lhs = try self.parseExprBp(min_bp);
            },

            // Literals that form the start of a binary expression
            .INTLIT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lit = ast.Lit {.Int = val} };
            },
            .BOOLLIT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lit = ast.Lit {.Bool = val}};
            },
            .IDENT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lval = ast.Lval {.Var = val}};
            },

            // Unary operators
            .MINUS, .NOT => {


                // Get precedence of token
                const bp: u8 = next_token.getPrefixPrecedence().?;

                // Work on right-hand side
                const rhs: *ast.Expr = try self.parseExprBp(bp);

                // Properly set left-hand side
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr {.UnOpExpr = ast.UnOpExpr {
                    .op = ast.UnOp.from_token(next_token).?,
                    .rhs = rhs,
                }};
            },

            // Parentheses
            .LPAREN => {
                const ptr: *ast.Expr = try self.parseExprBp(0);
                lhs = ptr;
                errdefer lhs.destroyAll(self.allocator);
                try self.expectToken(Token {.RPAREN = {}});
            },

            // BUG: If we go here we do not destroy lhs
            .EOF => return ParseError.ExpectedExpression,
            else => {
                return ParseError.ExpectedExpression;
            }
        }


        while (true) {

            // LHS has already been created, so need errdefer here
            errdefer lhs.destroyAll(self.allocator);

            // Ensure next is operator with improved binding power
            // Make sure that there is a token
            const op_token: Token = if (self.peekToken()) |tok| tok else return ParseError.ExpectedTokenOrEOF;

            // If postfix operator, we treat as postfix
            if (op_token.getPostfixPrecedence()) |op_prec| {

                // Make sure that we are binding stronger
                if (op_prec < min_bp) break
                else _ = self.nextToken() catch unreachable;

                // Treat the next token (can only be parenthesis)
                const r: ast.Expr = sw: switch (op_token) {
                    .LPAREN => {

                        // Parse expressions while possible
                        var acc: ArrayList(*const ast.Expr) = .init(self.allocator);
                        errdefer acc.deinit();
                        errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                        // Keep parsing expressions while comma until rparen
                        while (!std.meta.eql(self.peekToken(), Token {.RPAREN = {}})) {

                            // Parse expression
                            acc.append(try self.parseExprBp(0)) catch unreachable;

                            // Peek the next token
                            if (self.peekToken()) |tok| switch (tok) {
                                .COMMA => _ = self.nextToken() catch unreachable,
                                else => break
                            } else break;
                        }

                        // Make sure call expression is closed
                        try self.expectToken(Token {.RPAREN = {}});

                        // Produce call expression
                        break :sw ast.Expr {.CallExpr = ast.CallExpr {
                            .id = lhs,
                            .args = acc.toOwnedSlice() catch unreachable
                        }};

                    },
                    // TODO: Allow other kinds of post-fix operators
                    // like obj.property and arr[idx]
                    else => unreachable,
                };

                // Allocate and assign expression
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = r;

                // Continue for next token
                continue;
            }

            // If infix operator, we treat it as an infix
            if (op_token.getInfixPrecedence()) |op_prec| {

                // Decide if we should recurse for rhs
                const l_bp: u8 = op_prec.left;
                const r_bp: u8 = op_prec.right;

                // Break or skip the already peeked token
                if (l_bp < min_bp) break
                else _ = self.nextToken() catch unreachable;

                // Handle right-hand side of operator/expression
                const rhs: *ast.Expr = try self.parseExprBp(r_bp);

                // Update left-hand side to root of ast
                const r: ast.Expr = switch(op_token) {
                    .EQ => ast.Expr {.AssignExpr = ast.AssignExpr {
                        .lhs = lhs,
                        .rhs = rhs
                    }},
                    else => ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = lhs,
                        .op = ast.BinOp.from_token(op_token).?,
                        .rhs = rhs
                    }},
                };

                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = r;

                continue;
            }

            // Explicit continue statements must be found in cases
            break;

        }

        return lhs;
    }

    pub fn parseExpr(self: *Self) ParseError!*ast.Expr {
        return try self.parseExprBp(0);
    }

    pub fn parseStmt(self: *Self) ParseError!ast.Stmt {
        // Recursive descent parsing

        // Match on first token
        return if (self.peekToken()) |tok| blk: switch (tok) {

            // Skip all error tokens
            .ERROR, .ILLEGAL => {
                _ = try self.nextToken();
                break :blk self.parseStmt();
            },

            // Statements that start with specific keywords
            .PRINT => {
                try self.expectToken(Token {.PRINT = {}});

                var acc: ArrayList(*ast.Expr) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                acc.append(try self.parseExpr()) catch unreachable;
                while (std.meta.eql(self.peekToken(), Token {.COMMA = {}})) {
                    try self.expectToken(Token {.COMMA = {}});
                    acc.append(try self.parseExpr()) catch unreachable;
                }

                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.PrintStmt = acc.toOwnedSlice() catch unreachable};
            },
            .DECLARE => {
                try self.expectToken(Token {.DECLARE = {}});

                var acc: ArrayList(*ast.Expr) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                acc.append(try self.parseExpr()) catch unreachable;
                while (std.meta.eql(self.peekToken(), Token {.COMMA = {}})) {
                    try self.expectToken(Token {.COMMA = {}});
                    acc.append(try self.parseExpr()) catch unreachable;
                }

                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.DeclareStmt = acc.toOwnedSlice() catch unreachable};
            },
            .IF => {
                try self.expectToken(Token {.IF = {}});
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);

                try self.expectToken(Token {.LB = {}});

                const ifStmt: ast.Stmt = try self.parseStmt();
                errdefer ifStmt.destroyAll(self.allocator);

                // Look for an else branch
                const elseStmt: ?ast.Stmt = if (self.peekToken()) |tk| bk: switch (tk) {
                    .ELSE => {
                        try self.expectToken(Token {.ELSE = {}});
                        try self.expectToken(Token {.LB = {}});
                        const stmt = try self.parseStmt();
                        // TODO: Figure out if expect LB here...
                        // try self.expectToken(Token {.LB = {}});
                        break :bk stmt;
                    },
                    else => null
                } else null;

                // Allocate if else statement
                const ifElseStmt: *ast.IfElseStmt = self.allocator.create(ast.IfElseStmt) catch unreachable;
                ifElseStmt.* = ast.IfElseStmt {
                    .cond = exp,
                    .ifStmt = ifStmt,
                    .elseStmt = elseStmt,
                };

                break :blk ast.Stmt {.IfElseStmt = ifElseStmt};
            },
            .WHILE => {
                try self.expectToken(Token {.WHILE = {}});
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);

                try self.expectToken(Token {.LB = {}});

                const stmt: ast.Stmt = try self.parseStmt();

                const whileStmt: *ast.WhileStmt = self.allocator.create(ast.WhileStmt) catch unreachable;
                whileStmt.* = ast.WhileStmt {
                    .cond = exp,
                    .body = stmt
                };

                break :blk ast.Stmt {.WhileStmt = whileStmt};
            },
            .BREAK => {
                try self.expectToken(Token {.BREAK = {}});
                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.BreakStmt = {}};
            },
            .CONTINUE => {
                try self.expectToken(Token {.CONTINUE = {}});
                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.ContinueStmt = {}};
            },
            .RETURN => {
                try self.expectToken(Token {.RETURN = {}});

                // Try to parse return expression
                const maybe_expr: ?*ast.Expr = if (self.peekToken()) |tk| switch (tk) {
                    .LB, .EOF => null,
                    else => try self.parseExpr()
                } else null;
                try self.expectTokenOrEOF(Token {.LB = {}});

                // Create the actual return expression
                const expr: *ast.Expr = if (maybe_expr) |ex| ex else bk: {
                    const ptr: *ast.Expr = self.allocator.create(ast.Expr) catch unreachable;
                    ptr.* = ast.Expr {.Lit = ast.Lit {.Void = {}}};
                    break :bk ptr;
                };

                errdefer expr.destroyAll(self.allocator);

                // Return statement
                break :blk ast.Stmt {.ReturnStmt = expr};
            },
            .FUN => {
                try self.expectToken(Token {.FUN = {}});

                // Parse the function identifier
                const id: ast.Var = if (self.peekToken()) |tk| switch (tk) {
                    .IDENT => |id| id,
                    else => return expectedTokenError(Token {.IDENT = ""})
                } else return expectedTokenError(Token {.IDENT = ""});
                _ = try self.nextToken();

                // Parse function parameters
                var acc: ArrayList(ast.Var) = .init(self.allocator);
                errdefer acc.deinit();
                try self.expectToken(Token {.LPAREN = {}});

                while (!std.meta.eql(self.peekToken(), Token {.RPAREN = {}})) {

                    // Make sure next token is an identifier
                    switch(self.peekToken().?) {
                        .IDENT => |ident|{
                            _ = try self.nextToken();
                            acc.append(ident) catch unreachable;
                        },
                        else => return expectedTokenError(Token {.IDENT = ""})
                    }

                    // Continue if there is a COMMA, otherwise break loop
                    if (self.peekToken()) |tk| switch (tk) {
                        .COMMA => _ = self.nextToken() catch unreachable,
                        else => break
                    } else break;
                }

                try self.expectToken(Token {.RPAREN = {}});
                try self.expectToken(Token {.LB = {}});

                // Parse statement
                const body: ast.Stmt = try self.parseStmt();

                // NOTE: Since we end with a statement, we do not need to parse LB

                // Produce function definition
                const stmt: *ast.FunDefStmt = self.allocator.create(ast.FunDefStmt) catch unreachable;
                stmt.* = ast.FunDefStmt {
                    .id = id,
                    .body = body,
                    .params = acc.toOwnedSlice() catch unreachable
                };
                break :blk ast.Stmt {.FunDefStmt = stmt};

            },

            // Block Statements
            .LCURLY => {
                try self.expectToken(Token {.LCURLY = {}});
                try self.expectToken(Token {.LB = {}});

                // Create arraylist for this..
                var acc: ArrayList(ast.Stmt) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |stmt| stmt.destroyAll(self.allocator);

                // Parse statements while they are there...
                while (!std.meta.eql(self.peekToken(), Token {.RCURLY = {}}))
                    acc.append(try self.parseStmt()) catch unreachable;

                // Expect an ending
                try self.expectToken(Token {.RCURLY = {}});
                try self.expectTokenOrEOF(Token {.LB = {}});

                // Construct resulting statement
                break :blk ast.Stmt {.BlockStmt = acc.toOwnedSlice() catch unreachable};

            },

            .EOF => ParseError.ExpectedStatement,
            // Otherwise treat as an expression statement
            else => {
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);
                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.ExprStmt = exp};
            },

        } else ParseError.ExpectedTokenOrEOF;
    }

    pub fn parseProcedure(self: *Self) ParseError!ast.Proc {

        // Initialize buffer for statements
        var statements: ArrayList(ast.Stmt) = .init(self.allocator);
        errdefer statements.deinit();
        errdefer for (statements.items) |stmt| stmt.destroyAll(self.allocator);

        while (self.peekToken() != null) {
            switch (self.peekToken().?) {
                .EOF => break,
                else => {
                    // Parse statement
                    const stmt: ast.Stmt = try self.parseStmt();

                    // Append statement to list
                    statements.append(stmt) catch unreachable;
                }
            }
        }

        // Extract and return
        return ast.Proc {
            .stmts = statements.toOwnedSlice() catch unreachable
        };
    }

};

// TODO:
// 3. Add index expressions arr[x + i]
// 4. Add field expressions monkey.name
