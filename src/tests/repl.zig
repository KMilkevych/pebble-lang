const ast = @import("../ast.zig");
const lexer = @import("../lexer.zig");
const parser = @import("../parser.zig");
const token = @import("../token.zig");
const interpreter = @import("../interpreter.zig");
const venv = @import("../env.zig");

const std = @import("std");

test "intlit" {
    const input: []const u8 = "8191";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Int = 8191}, res);
}


test "complicated intlit expression" {
    const input: []const u8 = "(3 + 5) * 7 - (13 + 17)";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Int = 26}, res);
}


test "complicated boolean expression" {

    const input: []const u8 = "!(false || true) && !false";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Bool = false}, res);
}

test "variable eval with premade environment" {

    const input: []const u8 = "-3+some_variable";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();
    env.insert("some_variable", venv.ObjectVal {.Var = venv.Value {.Int = 2193}});

    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Int = 2190}, res);
}


test "assignment expression" {
    const input: []const u8 = "variable = 3";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    env.insert("variable", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    defer env.deinit();

    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Int = 3}, res);
}


test "double assignment" {
    const input: []const u8 = "x = y = 581";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const expr: *ast.Expr = try prsr.parseExpr();
    defer expr.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    env.insert("y", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    defer env.deinit();

    const res: ast.Lit = try interpreter.evalExpr(expr, &env);

    try std.testing.expectEqualDeep(ast.Lit {.Int = 581}, res);
}


test "declare statement undefined" {
    const input: []const u8 = "declare x";

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const stmt: ast.Stmt = try prsr.parseStmt();
    defer stmt.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    try interpreter.evalStmt(stmt, &env);

    // Assert that x has been added into environment as undefined
    const objval: ?venv.ObjectVal = env.lookup("x");
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = venv.Value {.Undefined = {}}}, objval);
}
