const ast = @import("ast.zig");
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const std = @import("std");
const ArrayList = std.ArrayList;

pub const ParseError = error {
    ExpectedTokenOrEOF,
    ExpectedPClose,
    ExpectedAngleClose,
    ExpectedPrint,
    ExpectedLineBreak,
    ExpectedIf,
    ExpectedWhile,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedStatement,
};

pub fn expectedTokenError(expected: TokenType) ParseError {
    return switch (expected) {
        .RPAREN => ParseError.ExpectedPClose,
        .PRINT => ParseError.ExpectedPrint,
        .LB => ParseError.ExpectedLineBreak,
        .IF => ParseError.ExpectedIf,
        .WHILE => ParseError.ExpectedWhile,
        .IDENT => ParseError.ExpectedIdentifier,
        .GT => ParseError.ExpectedAngleClose,
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
        return if (self.tokens.pop()) |tok| tok
        else ParseError.ExpectedTokenOrEOF;
    }

    fn peekToken(self: *Self) ?Token {
        return self.tokens.getLastOrNull();
    }

    fn peekNthToken(self: *Self, n: usize) ?Token {
        if (self.tokens.items.len >= n) return self.tokens.items[self.tokens.items.len - n]
        else return null;
    }

    fn expectToken(self: *Self, tok: TokenType) ParseError!void {
        const nt: Token = try self.nextToken();
        return if (
            !std.meta.eql(nt.tokenType, tok)
        ) expectedTokenError(tok);
    }

    fn expectTokenOrEOF(self: *Self, tok: TokenType) ParseError!void {
        const nt: TokenType = (try self.nextToken()).tokenType;
        return if (
            !std.meta.eql(nt, tok) and !std.meta.eql(nt, TokenType {.EOF = {}})
        ) expectedTokenError(tok);
    }

    fn parseExprBp(self: *Self, min_bp: u8) ParseError!*ast.Expr {

        // Parse left-hand side of expression
        var lhs: *ast.Expr = undefined;
        const next_token: Token = try self.nextToken();
        switch(next_token.tokenType) {

            // Skip errors
            .ERROR, .ILLEGAL => {
                lhs = try self.parseExprBp(min_bp);
            },

            // Literals that form the start of a binary expression
            .INTLIT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lit = ast.Lit {.Int = val} };
            },
            .FLOATLIT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lit = ast.Lit {.Float = val} };
            },
            .BOOLLIT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lit = ast.Lit {.Bool = val}};
            },
            .IDENT => |val| {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr { .Lval = ast.Lval {.Var = val}};
            },
            .INT => {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr {.Lit = ast.Lit {.Type = .Int}};
            },
            .FLOAT => {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr {.Lit = ast.Lit {.Type = .Float}};
            },
            .BOOL => {
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr {.Lit = ast.Lit {.Type = .Bool}};
            },

            // Unary operators
            .MINUS, .NOT => {


                // Get precedence of token
                const bp: u8 = next_token.tokenType.getPrefixPrecedence().?;

                // Work on right-hand side
                const rhs: *ast.Expr = try self.parseExprBp(bp);

                // Properly set left-hand side
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = ast.Expr {.UnOpExpr = ast.UnOpExpr {
                    .op = ast.UnOp.from_token(next_token.tokenType).?,
                    .rhs = rhs,
                }};
            },

            // Immediate list literals
            .LT => {

                // Parse contents
                var acc: ArrayList(*const ast.Expr) = .init(self.allocator);
                errdefer acc.deinit();
                errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                while (!std.meta.eql(self.peekToken().?.tokenType, TokenType {.GT = {}})) {

                    // Parse expression
                    const bp: u8 = (TokenType {.GT = {}}).getInfixPrecedence().?.right;
                    acc.append(try self.parseExprBp(bp)) catch unreachable;

                    // Peek the next token
                    if (self.peekToken()) |tok| switch (tok.tokenType) {
                        .COMMA => _ = self.nextToken() catch unreachable,
                        else => break
                    } else break;
                }

                // Consume closing bracket
                try self.expectToken(TokenType {.GT = {}});

                // Produce list literal
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.*  = ast.Expr {.ListExpr = acc.toOwnedSlice() catch unreachable};

            },

            // Parentheses
            .LPAREN => {
                const ptr: *ast.Expr = try self.parseExprBp(0);
                lhs = ptr;
                errdefer lhs.destroyAll(self.allocator);
                try self.expectToken(TokenType {.RPAREN = {}});
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
            if (op_token.tokenType.getPostfixPrecedence()) |op_prec| {

                // Make sure that we are binding stronger
                if (op_prec < min_bp) break
                else _ = self.nextToken() catch unreachable;

                // Treat the next token (can only be parenthesis)
                const r: ast.Expr = sw: switch (op_token.tokenType) {
                    .LPAREN => {

                        // Parse expressions while possible
                        var acc: ArrayList(*const ast.Expr) = .init(self.allocator);
                        errdefer acc.deinit();
                        errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                        // Keep parsing expressions while comma until rparen
                        while (!std.meta.eql(self.peekToken().?.tokenType, TokenType {.RPAREN = {}})) {

                            // Parse expression
                            acc.append(try self.parseExprBp(0)) catch unreachable;

                            // Peek the next token
                            if (self.peekToken()) |tok| switch (tok.tokenType) {
                                .COMMA => _ = self.nextToken() catch unreachable,
                                else => break
                            } else break;
                        }

                        // Make sure call expression is closed
                        try self.expectToken(TokenType {.RPAREN = {}});

                        // Produce call expression
                        break :sw ast.Expr {.CallExpr = ast.CallExpr {
                            .id = lhs,
                            .args = acc.toOwnedSlice() catch unreachable
                        }};

                    },
                    .LBRACK => {

                        // Parse single expression
                        const exp = try self.parseExprBp(0);
                        try self.expectToken(TokenType {.RBRACK = {}});

                        // Produce list index expression
                        break :sw ast.Expr {.Lval = ast.Lval {
                            .ListIndex = ast.ListIndex {
                                .id = lhs,
                                .idx = exp
                            }
                        }};
                    },

                    else => unreachable,
                };

                // Allocate and assign expression
                lhs = self.allocator.create(ast.Expr) catch unreachable;
                lhs.* = r;

                // Continue for next token
                continue;
            }

            // If infix operator, we treat it as an infix
            if (op_token.tokenType.getInfixPrecedence()) |op_prec| {

                // Decide if we should recurse for rhs
                const l_bp: u8 = op_prec.left;
                const r_bp: u8 = op_prec.right;

                // Break or skip the already peeked token
                if (l_bp < min_bp) break;
                _ = self.nextToken() catch unreachable;

                // Handle right-hand side of operator/expression
                const rhs: *ast.Expr = try self.parseExprBp(r_bp);

                // Update left-hand side to root of ast
                const r: ast.Expr = switch(op_token.tokenType) {
                    .EQ => ast.Expr {.AssignExpr = ast.AssignExpr {
                        .lhs = lhs,
                        .rhs = rhs
                    }},
                    .DOT => ast.Expr {.Lval = ast.Lval {.PropertyAccess = ast.PropertyAccess {
                        .lhs = lhs,
                        .prop = rhs
                    }}},
                    .AS => ast.Expr {.AsExpr = ast.AsExpr {
                        .lhs = lhs,
                        .as = rhs
                    }},
                    else => ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = lhs,
                        .op = ast.BinOp.from_token(op_token.tokenType).?,
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
        return if (self.peekToken()) |tok| blk: switch (tok.tokenType) {

            // Skip all error tokens
            .ERROR, .ILLEGAL => {
                _ = try self.nextToken();
                break :blk self.parseStmt();
            },

            // Statements that start with specific keywords
            .PRINT => {
                try self.expectToken(TokenType {.PRINT = {}});

                var acc: ArrayList(*ast.Expr) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                acc.append(try self.parseExpr()) catch unreachable;
                while (std.meta.eql(self.peekToken().?.tokenType, TokenType {.COMMA = {}})) {
                    try self.expectToken(TokenType {.COMMA = {}});
                    acc.append(try self.parseExpr()) catch unreachable;
                }

                try self.expectTokenOrEOF(TokenType {.LB = {}});
                break :blk ast.Stmt {.PrintStmt = acc.toOwnedSlice() catch unreachable};
            },
            .DECLARE => {
                try self.expectToken(TokenType {.DECLARE = {}});

                var acc: ArrayList(*ast.Expr) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |expr| expr.destroyAll(self.allocator);

                acc.append(try self.parseExpr()) catch unreachable;
                while (std.meta.eql(self.peekToken().?.tokenType, TokenType {.COMMA = {}})) {
                    try self.expectToken(TokenType {.COMMA = {}});
                    acc.append(try self.parseExpr()) catch unreachable;
                }

                try self.expectTokenOrEOF(TokenType {.LB = {}});
                break :blk ast.Stmt {.DeclareStmt = acc.toOwnedSlice() catch unreachable};
            },
            .IF => {
                try self.expectToken(TokenType {.IF = {}});
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);

                try self.expectToken(TokenType {.LB = {}});

                const ifStmt: ast.Stmt = try self.parseStmt();
                errdefer ifStmt.destroyAll(self.allocator);

                // Look for an else branch
                const elseStmt: ?ast.Stmt = if (self.peekToken()) |tk| bk: switch (tk.tokenType) {
                    .ELSE => {
                        try self.expectToken(TokenType {.ELSE = {}});
                        try self.expectToken(TokenType {.LB = {}});
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
                try self.expectToken(TokenType {.WHILE = {}});
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);

                try self.expectToken(TokenType {.LB = {}});

                const stmt: ast.Stmt = try self.parseStmt();

                const whileStmt: *ast.WhileStmt = self.allocator.create(ast.WhileStmt) catch unreachable;
                whileStmt.* = ast.WhileStmt {
                    .cond = exp,
                    .body = stmt
                };

                break :blk ast.Stmt {.WhileStmt = whileStmt};
            },
            .BREAK => {
                try self.expectToken(TokenType {.BREAK = {}});
                try self.expectTokenOrEOF(TokenType {.LB = {}});
                break :blk ast.Stmt {.BreakStmt = {}};
            },
            .CONTINUE => {
                try self.expectToken(TokenType {.CONTINUE = {}});
                try self.expectTokenOrEOF(TokenType {.LB = {}});
                break :blk ast.Stmt {.ContinueStmt = {}};
            },
            .RETURN => {
                try self.expectToken(TokenType {.RETURN = {}});

                // Try to parse return expression
                const maybe_expr: ?*ast.Expr = if (self.peekToken()) |tk| switch (tk.tokenType) {
                    .LB, .EOF => null,
                    else => try self.parseExpr()
                } else null;
                try self.expectTokenOrEOF(TokenType {.LB = {}});

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
                try self.expectToken(TokenType {.FUN = {}});

                // Parse the function identifier
                const id: ast.Var = if (self.peekToken()) |tk| switch (tk.tokenType) {
                    .IDENT => |id| id,
                    else => return expectedTokenError(TokenType {.IDENT = ""})
                } else return expectedTokenError(TokenType {.IDENT = ""});
                _ = try self.nextToken();

                // Parse function parameters
                var acc: ArrayList(ast.Var) = .init(self.allocator);
                errdefer acc.deinit();
                try self.expectToken(TokenType {.LPAREN = {}});

                while (!std.meta.eql(self.peekToken().?.tokenType, TokenType {.RPAREN = {}})) {

                    // Make sure next token is an identifier
                    switch(self.peekToken().?.tokenType) {
                        .IDENT => |ident|{
                            _ = try self.nextToken();
                            acc.append(ident) catch unreachable;
                        },
                        else => return expectedTokenError(TokenType {.IDENT = ""})
                    }

                    // Continue if there is a COMMA, otherwise break loop
                    if (self.peekToken()) |tk| switch (tk.tokenType) {
                        .COMMA => _ = self.nextToken() catch unreachable,
                        else => break
                    } else break;
                }

                try self.expectToken(TokenType {.RPAREN = {}});
                try self.expectToken(TokenType {.LB = {}});

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
                try self.expectToken(TokenType {.LCURLY = {}});
                try self.expectToken(TokenType {.LB = {}});

                // Create arraylist for this..
                var acc: ArrayList(ast.Stmt) = .init(self.allocator);
                defer acc.deinit();
                errdefer for (acc.items) |stmt| stmt.destroyAll(self.allocator);

                // Parse statements while they are there...
                while (!std.meta.eql(self.peekToken().?.tokenType, TokenType {.RCURLY = {}}))
                    acc.append(try self.parseStmt()) catch unreachable;

                // Expect an ending
                try self.expectToken(TokenType {.RCURLY = {}});
                try self.expectTokenOrEOF(TokenType {.LB = {}});

                // Construct resulting statement
                break :blk ast.Stmt {.BlockStmt = acc.toOwnedSlice() catch unreachable};

            },

            .EOF => ParseError.ExpectedStatement,
            // Otherwise treat as an expression statement
            else => {
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);
                try self.expectTokenOrEOF(TokenType {.LB = {}});
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
            switch (self.peekToken().?.tokenType) {
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
