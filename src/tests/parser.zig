const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const parser = @import("../parser.zig");
const token = @import("../token.zig");
const Token = token.Token;

const std = @import("std");

test "destroyAll" {

    // Make sure that destroyAll does not leak memory or crash

    const llrhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    llrhs.* = ast.Expr {.Lit = ast.Lit {.Int = 2}};

    const llhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    llhs.* = ast.Expr {.UnOpExpr = ast.UnOpExpr {
        .op = ast.UnOp.Neg,
        .rhs = llrhs
    }};

    const lrrhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    lrrhs.* = ast.Expr {.Lit = ast.Lit {.Int = 3}};

    const lrhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    lrhs.* = ast.Expr {.UnOpExpr = ast.UnOpExpr {
        .op = ast.UnOp.Neg,
        .rhs = lrrhs
    }};

    const lhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    lhs.* = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = llhs,
        .op = ast.BinOp.Mul,
        .rhs = lrhs
    }};

    const rrhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    rrhs.* = ast.Expr {.Lit = ast.Lit {.Bool = false}};

    const rhs: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    rhs.* = ast.Expr {.UnOpExpr = ast.UnOpExpr {
        .op = ast.UnOp.Neg,
        .rhs = rrhs
    }};

    const ptr: *ast.Expr = try std.testing.allocator.create(ast.Expr);
    ptr.* = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = lhs,
        .op = ast.BinOp.Add,
        .rhs = rhs
    }};

    ptr.destroyAll(std.testing.allocator);
}

test "single literal" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.Lit = ast.Lit { .Int = 13 }};
    try std.testing.expectEqualDeep(expect, result.*);
}

test "single binary add" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
        .op = ast.BinOp.Add,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};
    try std.testing.expectEqualDeep(expect, result.*);
}

test "double binary left-associative" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
            .op = ast.BinOp.Add,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        }},
        .op = ast.BinOp.Sub,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "precedence 1" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.MUL = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
        .op = ast.BinOp.Add,
        .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 7}},
            .op = ast.BinOp.Mul,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
        }},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "unary int" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.UnOpExpr = ast.UnOpExpr {
        .op = .Neg,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "unary int unary bool" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.MUL = {}});
    try tokens.append(Token {.NOT = {}});
    try tokens.append(Token {.BOOLLIT = true});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Neg,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }},
        .op = .Mul,
        .rhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Not,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
        }}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "int plus unary int" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Add,
        .rhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Neg,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}}
        }}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "unary int plus int" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Neg,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }},
        .op = .Add,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "unary int plus int parantheses" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.UnOpExpr = ast.UnOpExpr {
        .op = .Neg,
        .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
            .op = .Add,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
        }}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "many parentheses" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.INTLIT = 11});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.Lit = ast.Lit {.Int = 11}};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "precedence 2" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.MUL = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
            .op = ast.BinOp.Add,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        }},
        .op = ast.BinOp.Mul,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "precedence 2 div minus" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.DIV = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
            .op = ast.BinOp.Sub,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        }},
        .op = ast.BinOp.Div,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "boolean operations" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.NOT = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.BOOLLIT = false});
    try tokens.append(Token {.OR = {}});
    try tokens.append(Token {.BOOLLIT = true});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.AND = {}});
    try tokens.append(Token {.NOT = {}});
    try tokens.append(Token {.BOOLLIT = false});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Not,
            .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lit = ast.Lit {.Bool = false}},
                .op = ast.BinOp.Or,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }},
        .op = ast.BinOp.And,
        .rhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Not,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = false}}
        }},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "integer comparison and boolean operations precedence" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.DEQ = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.AND = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.DEQ = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}},
            .op = .Eq,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 5}}
        }},
        .op = ast.BinOp.And,
        .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
            .op = .Eq,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "single ident" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "abekat"});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.Lval = ast.Lval {.Var = "abekat"}};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "variable expression" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "abe"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.IDENT = "kat"});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "abe"}},
        .op = .Add,
        .rhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Neg,
            .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "kat"}}
        }}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "assignment expression" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "right associative assignment" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 47});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 47}}
        }},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}


test "left evaluated assignment" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .op = .Add,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "right associative assignment 2" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            .op = .Add,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
        }},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "x + y = 13 == (x + y) = 13" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .op = .Add,
            .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
        }},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "-x = 13 == (-x) = 13" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.UnOpExpr = ast.UnOpExpr {
            .op = .Neg,
            .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        }},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "unmatched parentheses" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 13});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: parser.ParseError!*ast.Expr = prs.parseExpr();

    const expect: parser.ParseError = parser.ParseError.ExpectedPClose;
    try std.testing.expectEqualDeep(expect, result);
}


test "unfinished expression" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: parser.ParseError!*ast.Expr = prs.parseExpr();

    const expect: parser.ParseError = parser.ParseError.ExpectedExpression;
    try std.testing.expectEqualDeep(expect, result);
}

test "expect token or EOF" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: parser.ParseError!*ast.Expr = prs.parseExpr();

    const expect: parser.ParseError = parser.ParseError.ExpectedTokenOrEOF;
    try std.testing.expectEqualDeep(expect, result);
}

test "expect token or EOF 2" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.RPAREN = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: parser.ParseError!*ast.Expr = prs.parseExpr();

    const expect: parser.ParseError = parser.ParseError.ExpectedTokenOrEOF;
    try std.testing.expectEqualDeep(expect, result);
}

test "print statement" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: ast.Stmt = try prs.parseStmt();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {
        .Lval = ast.Lval {.Var = "x"}
    }}};

    try std.testing.expectEqualDeep(expect, result);
}

test "declare statement" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: ast.Stmt = try prs.parseStmt();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
        .Lval = ast.Lval {.Var = "x"}
    }}};

    try std.testing.expectEqualDeep(expect, result);
}

test "expression statement" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: ast.Stmt = try prs.parseStmt();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.ExprStmt = &ast.Expr {
        .Lval = ast.Lval {.Var = "x"}
    }};

    try std.testing.expectEqualDeep(expect, result);
}


test "print statement sequence" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: ast.Stmt = try prs.parseStmt();
    defer result.destroyAll(std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {
        .Lval = ast.Lval {.Var = "x"}
    }}};

    try std.testing.expectEqualDeep(expect, result);

    const result2: ast.Stmt = try prs.parseStmt();
    defer result2.destroyAll(std.testing.allocator);

    const expect2: ast.Stmt = ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {
        .Lval = ast.Lval {.Var = "y"}
    }}};
    try std.testing.expectEqualDeep(expect2, result2);
}

test "expect line break between statements" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: parser.ParseError!ast.Stmt = prs.parseStmt();
    try std.testing.expectEqualDeep(parser.ParseError.ExpectedLineBreak, result);
}

test "empty block statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {}};
    defer expect.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, try prs.parseStmt());

}

test "block statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "nested block statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
        ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
        }}
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "if else statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.IF = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.ELSE = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
        .cond = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .ifStmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
        .elseStmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "y"}}}},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "if else block statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.IF = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.ELSE = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
        .cond = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
        }},
        .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "y"}}}}
        }},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "if else block statement 2" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.IF = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.ELSE = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
        .cond = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "y"}}}}
        }},
        .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "y"}}}},
            ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
        }},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "complicated if-else branch" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.IF = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.DEQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 20});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.ELSE = {}});

    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Eq,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}},
            }},
            .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 20}},
                }}},
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
            }},
            .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lit = ast.Lit {.Int = 1}}}}
            }}
        }},

        ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "simple while with condition" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.WHILE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.GT = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.RCURLY = {}});

    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr { &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}}
            }},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "more interesting while" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.WHILE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.GT = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.MINUS = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});

    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }}
                }}}
            }},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}


test "while with break statement" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.WHILE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.GT = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.BREAK = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});

    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
                ast.Stmt {.BreakStmt = {}},
            }},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "while with continue statement" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 10});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.WHILE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.GT = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.LCURLY = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});

    try tokens.append(Token {.CONTINUE = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RCURLY = {}});

    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}},
                ast.Stmt {.ContinueStmt = {}},
            }},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}


test "comma declaration" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);


    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 2});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "w"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
            }},
            &ast.Expr {.Lval = ast.Lval {.Var = "z"}},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "w"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "comma declaration double comma error" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);


    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 2});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "w"});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);
    const res: parser.ParseError!ast.Proc = prs.parseProcedure();

    try std.testing.expectEqualDeep(res, parser.ParseError.ExpectedExpression);
}

test "comma print" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);


    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.PrintStmt = &[_]*const ast.Expr {
            &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = .Add,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
            &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
        }},
    }};

    const actual: ast.Proc = try prs.parseProcedure();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "comma print multiple comma error" {
    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);

    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const actual: parser.ParseError!ast.Proc = prs.parseProcedure();

    try std.testing.expectEqualDeep(parser.ParseError.ExpectedExpression, actual);
}

test "call expression no args" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
        .args = &[_]*const ast.Expr {
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "call expression one arg" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "call expression two args" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "somevar"});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            &ast.Expr {.Lval = ast.Lval {.Var = "somevar"}},
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "call expression complex expressions as args" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "otherfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "otherfun"}},
                .args = &[_]*const ast.Expr {},
            }},
            &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .op = .Add,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "expression as function funcall" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.MUL = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .op = .Mul,
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }},
        .args = &[_]*const ast.Expr {}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "funcall funcall" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
            .args = &[_]*const ast.Expr {},
        }},
        .args = &[_]*const ast.Expr {},
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "funcall funcall funcall" {
    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "myfun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "myfun"}},
                .args = &[_]*const ast.Expr {},
            }},
            .args = &[_]*const ast.Expr {},
        }},
        .args = &[_]*const ast.Expr {}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "empty return statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Void = {}}}};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "return statement with expression" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .op = .Add,
        .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
    }}};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}


test "empty return statement at the end" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Void = {}}}};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "return statement with expression at the end" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .op = .Add,
        .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
    }}};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);

}

test "simple inline function definition" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.FUN = {}});
    try tokens.append(Token {.IDENT = "last"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
        .id = "last",
        .params = &[_]ast.Var {"x", "y", "z"},
        .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "z"}}}
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}



// test "function definition with trailing comma" {

//     var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
//     try tokens.append(Token {.FUN = {}});
//     try tokens.append(Token {.IDENT = "last"});
//     try tokens.append(Token {.LPAREN = {}});
//     try tokens.append(Token {.IDENT = "x"});
//     try tokens.append(Token {.COMMA = {}});
//     try tokens.append(Token {.IDENT = "y"});
//     try tokens.append(Token {.COMMA = {}});
//     try tokens.append(Token {.RPAREN = {}});
//     try tokens.append(Token {.LB = {}});
//     try tokens.append(Token {.RETURN = {}});
//     try tokens.append(Token {.IDENT = "z"});
//     try tokens.append(Token {.EOF = {}});
//     defer tokens.deinit();

//     var prs: parser.Parser = .new(tokens, std.testing.allocator);
//     const res: parser.ParseError!ast.Stmt = prs.parseStmt();

//     try std.testing.expectEqualDeep(parser.ParseError.ExpectedIdentifier, res);
// }


test "function definition no name" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.FUN = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.RETURN = {}});
    try tokens.append(Token {.IDENT = "z"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);
    const res: parser.ParseError!ast.Stmt = prs.parseStmt();

    try std.testing.expectEqualDeep(parser.ParseError.ExpectedIdentifier, res);
}


test "function definition no body" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.FUN = {}});
    try tokens.append(Token {.IDENT = "afun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);
    const res: parser.ParseError!ast.Stmt = prs.parseStmt();

    try std.testing.expectEqualDeep(parser.ParseError.ExpectedLineBreak, res);
}

test "function definition no body 2" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.FUN = {}});
    try tokens.append(Token {.IDENT = "afun"});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "x"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "y"});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.LB = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);
    const res: parser.ParseError!ast.Stmt = prs.parseStmt();

    try std.testing.expectEqualDeep(parser.ParseError.ExpectedStatement, res);
}

test "make statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "list"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }}},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "multi make statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "list"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "otherlist"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }}},
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "otherlist"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        }}},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "list index expression" {

    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.Lval = ast.Lval {
        .ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "complex list index expression" {

    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.MUL = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.PLUS = {}});
    try tokens.append(Token {.INTLIT = 4});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.Lval = ast.Lval {
        .ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
                .op = .Mul,
                .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    .op = .Add,
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
                }}
            }}
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "double list index expression" {

    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.Lval = ast.Lval {
        .ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "list index assignment" {

    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 3});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        }}},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
    }};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "list index in parentheses" {

    var tokens: std.ArrayList(Token) = .init(std.testing.allocator);
    defer tokens.deinit();

    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.LPAREN = {}});
    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.RPAREN = {}});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const result: *ast.Expr = try prs.parseExpr();
    defer result.destroyAll(std.testing.allocator);


    const expect: ast.Expr = ast.Expr { .Lval = ast.Lval {
        .ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }}}

    }}};

    try std.testing.expectEqualDeep(expect, result.*);
}

test "print list index statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "list"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.PrintStmt = &[_]*const ast.Expr {
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }}},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "print multi list index statement" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.PRINT = {}});
    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 7});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.PrintStmt = &[_]*const ast.Expr {
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }}},
        &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
            .idx = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        }}},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}

test "declare multi list with initialization" {

    var tokens: std.ArrayList(Token) = std.ArrayList(Token).init(std.testing.allocator);
    try tokens.append(Token {.DECLARE = {}});
    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 1});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.INTLIT = -1});
    try tokens.append(Token {.COMMA = {}});
    try tokens.append(Token {.IDENT = "list"});
    try tokens.append(Token {.LBRACK = {}});
    try tokens.append(Token {.INTLIT = 5});
    try tokens.append(Token {.RBRACK = {}});
    try tokens.append(Token {.EQ = {}});
    try tokens.append(Token {.IDENT = "lst"});
    try tokens.append(Token {.EOF = {}});
    defer tokens.deinit();

    var prs: parser.Parser = .new(tokens, std.testing.allocator);

    const expect: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = -1}}
        }},
        &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 5}}
            }}},
            .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}
        }},
    }};

    const actual: ast.Stmt = try prs.parseStmt();
    defer actual.destroyAll(std.testing.allocator);

    try std.testing.expectEqualDeep(expect, actual);
}
