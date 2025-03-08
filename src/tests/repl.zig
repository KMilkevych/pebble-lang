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
    env.insert("some_variable", venv.ObjectVal {.Var = ast.Lit {.Int = 2193}});

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
    env.insert("variable", venv.ObjectVal {.Undefined = {}});
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
    env.insert("x", venv.ObjectVal {.Undefined = {}});
    env.insert("y", venv.ObjectVal {.Undefined = {}});
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

    _ = try interpreter.evalStmt(stmt, &env);

    // Assert that x has been added into environment as undefined
    const objval: ?venv.ObjectVal = env.lookup("x");
    try std.testing.expectEqualDeep(venv.ObjectVal {.Undefined = {}}, objval);
}


test "break statement" {
    const input: []const u8 =
        \\declare x = 10
        \\while x > 0 {
        \\x = x - 1
        \\if x == 5 break
        \\}
    ;

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    _ = try interpreter.evalProc(proc, &env);

    // Assert that x has been added into environment as undefined
    const objval: ?venv.ObjectVal = env.lookup("x");
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = ast.Lit {.Int = 5}}, objval);
}

test "continue statement" {
    const input: []const u8 =
        \\declare x = 10
        \\declare y = 0
        \\while x > 0 {
        \\x = x - 1
        \\continue
        \\y = y + 1
        \\}
        \\y = y + 1
    ;

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    _ = try interpreter.evalProc(proc, &env);

    // Assert that x has been added into environment as undefined
    const objval: ?venv.ObjectVal = env.lookup("y");
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = ast.Lit {.Int = 1}}, objval);
}

test "closure 1 test" {


    const input: []const u8 =
        \\declare x = 7
        \\function mul_by_x(y) return x * y
        \\declare y = mul_by_x(3)
        \\x = 11
        \\declare z = mul_by_x(5)
    ;

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    _ = try interpreter.evalProc(proc, &env);

    // Assert that x has been added into environment as undefined
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = ast.Lit {.Int = 21}}, env.lookup("y"));
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = ast.Lit {.Int = 55}}, env.lookup("z"));
}


test "first-class values test" {
    const input: []const u8 =
        \\function add(x, y) return x + y
        \\function apply(op, x, y) return op(x, y)
        \\declare x = apply(add, 3, 7)
    ;

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    _ = try interpreter.evalProc(proc, &env);

    // Assert that x has been added into environment as undefined
    try std.testing.expectEqualDeep(venv.ObjectVal {.Var = ast.Lit {.Int = 10}}, env.lookup("x"));
}


test "prohibit upcalling" {
    const input: []const u8 =
        \\function make_function() {
        \\  function voidfun() {}
        \\  return voidfun
        \\}
        \\declare f = make_function()
    ;

    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.testing.allocator);
    const tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    var prsr: parser.Parser = parser.Parser.new(tokens, std.testing.allocator);
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(std.testing.allocator);

    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    const r: interpreter.EvalError!void = interpreter.evalProc(proc, &env);

    // Assert that x has been added into environment as undefined
    try std.testing.expectEqualDeep(interpreter.EvalError.InvalidUpcall, r);
}
