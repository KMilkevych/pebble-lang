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
};

pub fn expectedTokenError(expected: Token) ParseError {
    return switch (expected) {
        .RPAREN => ParseError.ExpectedPClose,
        .PRINT => ParseError.ExpectedPrint,
        .LB => ParseError.ExpectedLineBreak,
        .IF => ParseError.ExpectedIf,
        .WHILE => ParseError.ExpectedWhile,
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
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);
                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.PrintStmt = exp};
            },
            .DECLARE => {
                try self.expectToken(Token {.DECLARE = {}});
                const exp: *ast.Expr = try self.parseExpr();
                errdefer exp.destroyAll(self.allocator);
                try self.expectTokenOrEOF(Token {.LB = {}});
                break :blk ast.Stmt {.DeclareStmt = exp};
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
