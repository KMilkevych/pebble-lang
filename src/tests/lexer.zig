const ast = @import("../ast.zig");
const interpreter = @import("../interpreter.zig");
const lexer = @import("../lexer.zig");
const token = @import("../token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const std = @import("std");
const expect = std.testing.expect;

fn tokens_eql(tokens1: []const Token, tokens2: []const Token) bool {
    if (tokens1.len != tokens2.len) return false;
    for (tokens1, tokens2) |tk1, tk2| if (!std.meta.eql(tk1, tk2)) return false;
    return true;
}

fn printTokens(tokens: std.ArrayList(Token)) void {
    for (tokens.items) |tok| std.debug.print("{}\n", .{tok});
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

    const input = "12+2+891 + 21823 + 2";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "++ + \t+\t\t+ \t\r\t+ \r+\r\t+  \t \t \t +";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    try expect(tokens_eql(tokens.items, expected));
}


test "operators" {

    const input = "(1+3)*\t( 8912 || 123 )&&  4/5==12";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "!(true&&false)||!true==false";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "12'32 ^23@3";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.INTLIT = 12}},
        Token {.tokenType = TokenType {.ILLEGAL = '\''}},
        Token {.tokenType = TokenType {.INTLIT = 32}},
        Token {.tokenType = TokenType {.ILLEGAL = '^'}},
        Token {.tokenType = TokenType {.INTLIT = 23}},
        Token {.tokenType = TokenType {.ILLEGAL = '@'}},
        Token {.tokenType = TokenType {.INTLIT = 3}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "bool literals" {

    const input = "true && false||true*false";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "declare && declare4declare";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.DECLARE = {}}},
        Token {.tokenType = TokenType {.AND = {}}},
        Token {.tokenType = TokenType {.IDENT = "declare4declare"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "keyword 2" {

    const input = "print x declare y = 3";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "declare (bob == alice)||eve*b0b";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "declare eeve=bob&&alice";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "()[(]{[}";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "declare x=x+1\n\n\ndeclare";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "\nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "no line break before first statement 2" {
    const input = "\n\n \n\n \nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "line break mixup" {
    const input = "\n\n \n\n \nprint x\n \n\n\r   \nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "print x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line statement 2" {
    const input = "\nprint x\n";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line statement 3" {
    const input = "\n\n \nprint x\n \n";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.PRINT = {}}},
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line two statements" {
    const input = "print x print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "print x \n \n print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "\n print x \n \n print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "\n print x \n \n {print y}";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{
        \\  print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{print y}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{print y print x}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{print y
        \\print x}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{
        \\print y
        \\print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\print x
        \\print y
        \\{
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "empty block inline" {
    const input =
        \\{}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.LCURLY = {}}},
        Token {.tokenType = TokenType {.LB = {}}},
        Token {.tokenType = TokenType {.RCURLY = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "nested blocks" {
    const input =
        \\{
        \\{
        \\}
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{{}}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{{
        \\}}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{
        \\{}}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{{}
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\{{}
        \\}
        \\declare x = 10
        \\{declare x}{declare x
        \\print x}
        \\print x
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\if x {
        \\} else {}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\if x {} else {}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\if x print x else print y
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\if x {print x} else print y
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\if x {
        \\x = 10
        \\}
        \\else print y
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\while x {
        \\ print x
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "while x print x";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "x < y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than" {
    const input = "x > y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.GT = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "less than equal" {
    const input = "x <= y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.LTE = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than equal" {
    const input = "x >= y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "x"}},
        Token {.tokenType = TokenType {.GTE = {}}},
        Token {.tokenType = TokenType {.IDENT = "y"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "expression statement after other statements" {
    const input =
        \\print x
        \\x = x
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "break";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.BREAK = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "continue" {
    const input = "continue";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.CONTINUE = {}}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "block break" {
    const input =
        \\while true {
        \\break
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\while true {
        \\continue
        \\}
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\while true break
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\while true
        \\break
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\declare x = 1, y = 2, z, w = 5
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\declare x = 0
        \\function last(x, y, z) return z
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\declare list[10]
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\declare list[10], anotherlist[5]
        \\print 10
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\mylist.size
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.IDENT = "mylist"}},
        Token {.tokenType = TokenType {.DOT = {}}},
        Token {.tokenType = TokenType {.IDENT = "size"}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "multi property access" {
    const input =
        \\obj.prop1.prop2
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\obj
        \\  .prop1
        \\  .prop2
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\ # this is a comment
        \\ ; this is another comment
        \\ declare x = 0
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\ declare x = 0 ; this is a descriptive comment
        \\ print x # this is another descriptive comment
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input =
        \\ declare my_;variable = 0 <-- inline comment
        \\ print my_#variable print 0
    ;

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "13.4";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.4}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "short form float literal" {
    const input = "13.";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.0}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "float expression" {
    const input = "13.4+1.-1.3434";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
    const input = "13.,42";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.tokenType = TokenType {.FLOATLIT = 13.0}},
        Token {.tokenType = TokenType {.COMMA = {}}},
        Token {.tokenType = TokenType {.INTLIT = 42}},
        Token {.tokenType = TokenType {.EOF = {}}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "booleans alternative" {

    const input = "!(true and false)or true==false";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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

    const input = "true as Int as Float as Bool";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

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
