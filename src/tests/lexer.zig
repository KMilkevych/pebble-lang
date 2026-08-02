const ast = @import("../ast.zig");
const interpreter = @import("../interpreter.zig");
const lexer = @import("../lexer.zig");
const token = @import("../token.zig");
const loc = @import("../location.zig");
const Token = token.Token;
const TokenType = token.TokenType;
const Location = loc.Location;
const LocationRange = loc.LocationRange;
const Log = @import("../logger.zig");

const std = @import("std");
const expect = std.testing.expect;

fn tokens_eql(tokens1: []const Token, tokens2: []const Token) bool {
    if (tokens1.len != tokens2.len) return false;
    for (tokens1, tokens2) |tk1, tk2| if (!std.meta.eql(tk1, tk2)) return false;
    return true;
}

fn printTokens(tokens: std.ArrayList(Token)) void {
    for (tokens.items) |tok| std.debug.print("{f}\n", .{tok});
}

test "tokens_eql" {
    const tokens1: []const Token = &[_]Token {
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    const tokens2: []const Token = &[_]Token {
        Token {.tokenType = TokenType {.INTLIT = 4}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    const tokens3: []const Token = &[_]Token {
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.MINUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    const tokens4: []const Token = &[_]Token {
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.MINUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    const tokens5: []const Token = &[_]Token {
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(!tokens_eql(tokens1, tokens2));
    try expect(!tokens_eql(tokens1, tokens3));
    try expect(!tokens_eql(tokens1, tokens4));
    try expect(tokens_eql(tokens1, tokens5));
}

test "tokens_eql 2" {
    try expect(std.meta.eql(Token {.tokenType = TokenType {.IDENT = "identifier"}}, Token {.tokenType = TokenType {.IDENT = "identifier"}}));
}

test "tokens_eql 3" {
    const slice = "id";
    const tk1: Token = Token {.tokenType = TokenType {.IDENT = "id"}};
    const tk2: Token = Token {.tokenType = TokenType {.IDENT = slice}};
    try expect(std.meta.eql(tk1, tk2));
}

test "intlits" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "12+2+891 + 21823 + 2";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.INTLIT = 12}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 2}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 891}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 21823}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 2}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}


test "whitespace" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "++ + \t+\t\t+ \t\r\t+ \r+\r\t+  \t \t \t +";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    // try expect(tokens_eql(tokens.items, expected));
    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "operators" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "(1+3)*\t( 8912 || 123 )&&  4/5==12";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.INTLIT = 1}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.MUL = {}}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.INTLIT = 8912}},
        Token {.tokenType = TokenType {.OR = {}}},
        Token {.tokenType = TokenType {.INTLIT = 123}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.INTLIT = 4}},
        Token {.tokenType = TokenType {.DIV = {}}},
        Token {.tokenType = TokenType {.INTLIT = 5}},
        Token {.tokenType = TokenType {.DEQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 12}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "booleans" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "!(true&&false)||!true==false";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.NOT = {}}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.OR = {}}},
        Token {.tokenType = TokenType {.NOT = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.DEQ = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "illegal" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "1232 ^23@3";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.INTLIT = 1232}},
        Token {.tokenType = TokenType {.ILLEGAL = '^'}},
        Token {.tokenType = TokenType {.INTLIT = 23}},
        Token {.tokenType = TokenType {.ILLEGAL = '@'}},
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "bool literals" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "true && false||true*false";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.OR = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.MUL = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "keyword" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();

    const input = "declare && declare4declare";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.IDENT = "declare4declare"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "keyword 2" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "print x declare y = 3";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "identifiers" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "declare (bob == alice)||eve*b0b";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.IDENT = "bob"}},
        Token {.tokenType = TokenType {.DEQ = {}}},
        Token {.tokenType = TokenType {.IDENT = "alice"}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.OR = {}}},
        Token {.tokenType = TokenType {.IDENT = "eve"}},
        Token {.tokenType = TokenType {.MUL = {}}},
        Token {.tokenType = TokenType {.IDENT = "b0b"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "assignment" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "declare eeve=bob&&alice";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "eeve"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.IDENT = "bob"}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.IDENT = "alice"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "brackets" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "()[(]{[}";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    // NOTE: Expecting a LB to be inserted before RCURLY and LCURLY
    // This is because { and } are special statement-related tokens
    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.LBRACK = {}}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.RBRACK = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LBRACK = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "smart line breaks" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "declare x=x+1\n\n\ndeclare";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.INTLIT = 1}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "no line break before first statement" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\nprint x";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "no line break before first statement 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\n\n \n\n \nprint x";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "line break mixup" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\n\n \n\n \nprint x\n \n\n\r   \nprint x";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line statement" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "print x";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line statement 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\nprint x\n";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line statement 3" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\n\n \nprint x\n \n";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line two statements" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "print x print y";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line two statements 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "print x \n \n print y";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line two statements 3" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\n print x \n \n print y";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line with block statement" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "\n print x \n \n {print y}";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline input" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline input 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "multiline input with blocks" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{
        \\  print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline input with inline block" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{print y}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "multiline input with inline block two statements" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{print y print x}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "multiline input with inline block two statements 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{print y
        \\print x}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };


    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline input with block two statements" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{
        \\print y
        \\print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "multiline input with empty block" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\print y
        \\{
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "empty block" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "empty block inline" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "nested blocks" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{
        \\{
        \\}
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "nested blocks inline" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{{}}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "nested blocks semi-inline" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{{
        \\}}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "nested blocks semi-inline 2" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{
        \\{}}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "nested blocks semi-inline 3" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{{}
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "advanced sequence with nested blocks" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\{{}
        \\}
        \\declare x = 10
        \\{declare x}{declare x
        \\print x}
        \\print x
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 10}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline if else" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if x {
        \\} else {}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IF = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.ELSE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "inline if else" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if x {} else {}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IF = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.ELSE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "if else without curly braces" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if x print x else print y
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IF = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.ELSE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "if else with mixed curly braces" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if x {print x} else print y
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IF = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.ELSE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline if else with mixed curly braces" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if x {
        \\x = 10
        \\}
        \\else print y
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IF = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 10}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.ELSE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline while" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\while x {
        \\ print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);

}

test "inline while" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "while x print x";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "less than" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "x < y";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "x > y";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.GT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "less than equal" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "x <= y";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LTE = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than equal" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "x >= y";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.GTE = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "expression statement after other statements" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\print x
        \\x = x
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "break" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "break";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.BREAK = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "continue" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "continue";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.CONTINUE = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "block break" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\while true {
        \\break
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.BREAK = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "block continue" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\while true {
        \\continue
        \\}
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.CONTINUE = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "inline block break" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\while true break
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.BREAK = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "newline block break" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\while true
        \\break
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.WHILE = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.BREAK = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "comma declaration" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare x = 1, y = 2, z, w = 5
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 1}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 2}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "z"}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "w"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 5}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "inline simple function definition with return" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare x = 0
        \\function last(x, y, z) return z
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 0}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.FUN = {}}},
        Token {.tokenType = TokenType {.IDENT = "last"}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "z"}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RETURN = {}}},
        Token {.tokenType = TokenType {.IDENT = "z"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single make" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare list[10]
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "list"}},
        Token {.tokenType = TokenType {.LBRACK = {}}},
        Token {.tokenType = TokenType {.INTLIT = 10}},
        Token {.tokenType = TokenType {.RBRACK = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline multi make" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare list[10], anotherlist[5]
        \\print 10
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "list"}},
        Token {.tokenType = TokenType {.LBRACK = {}}},
        Token {.tokenType = TokenType {.INTLIT = 10}},
        Token {.tokenType = TokenType {.RBRACK = {}}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.IDENT = "anotherlist"}},
        Token {.tokenType = TokenType {.LBRACK = {}}},
        Token {.tokenType = TokenType {.INTLIT = 5}},
        Token {.tokenType = TokenType {.RBRACK = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.INTLIT = 10}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "property access" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\mylist.size
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "mylist"}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "size"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multi property access" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\obj.prop1.prop2
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "obj"}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "prop1"}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "prop2"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multiline property access not allowed" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\obj
        \\  .prop1
        \\  .prop2
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "obj"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "prop1"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "prop2"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "line comment" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\ # this is a comment
        \\ ; this is another comment
        \\ declare x = 0
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 0}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "after line comment" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\ declare x = 0 ; this is a descriptive comment
        \\ print x # this is another descriptive comment
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EQ = {}}},
        Token {.tokenType = TokenType {.INTLIT = 0}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "inline comment" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\ declare my_;variable = 0 <-- inline comment
        \\ print my_#variable print 0
    ;

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.IDENT = "my_"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "my_"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "float literal" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "13.4";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.4}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "short form float literal" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "13.";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.0}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "float expression" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "13.4+1.-1.3434";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.4}},
        Token {.tokenType = TokenType {.PLUS = {}}},
        Token {.tokenType = TokenType {.FLOATLIT = 1.0}},
        Token {.tokenType = TokenType {.MINUS = {}}},
        Token {.tokenType = TokenType {.FLOATLIT = 1.3434}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "mixed float" {
    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "13.,42";

    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.0}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.INTLIT = 42}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "booleans alternative" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "!(true and false)or true==false";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.NOT = {}}},
        Token {.tokenType = TokenType {.LPAREN = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.RPAREN = {}}},
        Token {.tokenType = TokenType {.OR = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.DEQ = {}}},
        Token {.tokenType = TokenType {.BOOLLIT = false}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "as and type keywords" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input = "true as Int as Float as Bool";
    var lx = lexer.Lexer.new(input, "", std.testing.allocator, &logger);
    lx.storeLocation = false;

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.BOOLLIT = true}},
        Token {.tokenType = TokenType {.AS = {}}},
        Token {.tokenType = TokenType {.INT = {}}},
        Token {.tokenType = TokenType {.AS = {}}},
        Token {.tokenType = TokenType {.FLOAT = {}}},
        Token {.tokenType = TokenType {.AS = {}}},
        Token {.tokenType = TokenType {.BOOL = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "location one line" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();
    const input = "myvar + 3";
    var lx = lexer.Lexer.new(input, "test.peb", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.IDENT = "myvar"},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 0, .line = 0},
                .to = Location {.file = "test.peb", .column = 4, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.PLUS = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 6, .line = 0},
                .to = Location {.file = "test.peb", .column = 6, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 3},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 8, .line = 0},
                .to = Location {.file = "test.peb", .column = 8, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 9, .line = 0},
                .to = Location {.file = "test.peb", .column = 9, .line = 0},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "location two lines auto line break" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare myvar = 1
        \\return 0.23
    ;

    var lx = lexer.Lexer.new(input, "tf", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.DECLARE = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 0},
                .to = Location {.file = "tf", .column = 6, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "myvar"},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 8, .line = 0},
                .to = Location {.file = "tf", .column = 12, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.EQ  = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 14, .line = 0},
                .to = Location {.file = "tf", .column = 14, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 1},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 16, .line = 0},
                .to = Location {.file = "tf", .column = 16, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LB = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 17, .line = 0},
                .to = Location {.file = "tf", .column = 17, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.RETURN = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 1},
                .to = Location {.file = "tf", .column = 5, .line = 1},
            }
        },
        Token {
            .tokenType = TokenType {.FLOATLIT = 0.23},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 7, .line = 1},
                .to = Location {.file = "tf", .column = 10, .line = 1},
            }
        },
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 11, .line = 1},
                .to = Location {.file = "tf", .column = 11, .line = 1},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "location extra line break" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\declare myvar = 1
        \\
        \\return 0.23
    ;

    var lx = lexer.Lexer.new(input, "tf", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.DECLARE = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 0},
                .to = Location {.file = "tf", .column = 6, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "myvar"},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 8, .line = 0},
                .to = Location {.file = "tf", .column = 12, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.EQ  = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 14, .line = 0},
                .to = Location {.file = "tf", .column = 14, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 1},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 16, .line = 0},
                .to = Location {.file = "tf", .column = 16, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LB = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 17, .line = 0},
                .to = Location {.file = "tf", .column = 17, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.RETURN = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 2},
                .to = Location {.file = "tf", .column = 5, .line = 2},
            }
        },
        Token {
            .tokenType = TokenType {.FLOATLIT = 0.23},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 7, .line = 2},
                .to = Location {.file = "tf", .column = 10, .line = 2},
            }
        },
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 11, .line = 2},
                .to = Location {.file = "tf", .column = 11, .line = 2},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "location if statement" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\if (1 == 0)
        \\  return false
    ;

    var lx = lexer.Lexer.new(input, "tf", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.IF = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 0},
                .to = Location {.file = "tf", .column = 1, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LPAREN = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 3, .line = 0},
                .to = Location {.file = "tf", .column = 3, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 1},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 4, .line = 0},
                .to = Location {.file = "tf", .column = 4, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.DEQ  = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 6, .line = 0},
                .to = Location {.file = "tf", .column = 7, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 0},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 9, .line = 0},
                .to = Location {.file = "tf", .column = 9, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.RPAREN = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 10, .line = 0},
                .to = Location {.file = "tf", .column = 10, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LB = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 11, .line = 0},
                .to = Location {.file = "tf", .column = 11, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.RETURN = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 2, .line = 1},
                .to = Location {.file = "tf", .column = 7, .line = 1},
            }
        },
        Token {
            .tokenType = TokenType {.BOOLLIT = false},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 9, .line = 1},
                .to = Location {.file = "tf", .column = 13, .line = 1},
            }
        },
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 14, .line = 1},
                .to = Location {.file = "tf", .column = 14, .line = 1},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "location early eof" {

    var logger: Log.Logger = .new(std.testing.allocator);
defer logger.destroyAll();
const input =
        \\
    ;

    var lx = lexer.Lexer.new(input, "tf", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "tf", .column = 0, .line = 0},
                .to = Location {.file = "tf", .column = 0, .line = 0},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "location with property access" {

    var logger: Log.Logger = .new(std.testing.allocator);
    defer logger.destroyAll();
    const input = "while (i = i + 2) < x.sizes";
    var lx = lexer.Lexer.new(input, "test.peb", std.testing.allocator, &logger);

    var tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit(std.testing.allocator);

    const expected: []const Token = &[_]Token{
        Token {
            .tokenType = TokenType {.WHILE = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 0, .line = 0},
                .to = Location {.file = "test.peb", .column = 4, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LPAREN = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 6, .line = 0},
                .to = Location {.file = "test.peb", .column = 6, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "i"},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 7, .line = 0},
                .to = Location {.file = "test.peb", .column = 7, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.EQ = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 9, .line = 0},
                .to = Location {.file = "test.peb", .column = 9, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "i"},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 11, .line = 0},
                .to = Location {.file = "test.peb", .column = 11, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.PLUS = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 13, .line = 0},
                .to = Location {.file = "test.peb", .column = 13, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.INTLIT = 2},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 15, .line = 0},
                .to = Location {.file = "test.peb", .column = 15, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.RPAREN = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 16, .line = 0},
                .to = Location {.file = "test.peb", .column = 16, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.LT = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 18, .line = 0},
                .to = Location {.file = "test.peb", .column = 18, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "x"},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 20, .line = 0},
                .to = Location {.file = "test.peb", .column = 20, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.DOT = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 21, .line = 0},
                .to = Location {.file = "test.peb", .column = 21, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.IDENT = "sizes"},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 22, .line = 0},
                .to = Location {.file = "test.peb", .column = 26, .line = 0},
            }
        },
        Token {
            .tokenType = TokenType {.EOF = {}},
            .location = LocationRange {
                .from = Location {.file = "test.peb", .column = 27, .line = 0},
                .to = Location {.file = "test.peb", .column = 27, .line = 0},
            }
        },
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}
