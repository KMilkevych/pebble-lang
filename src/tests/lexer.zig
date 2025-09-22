const ast = @import("../ast.zig");
const interpreter = @import("../interpreter.zig");
const lexer = @import("../lexer.zig");
const token = @import("../token.zig");
const Token = token.Token;

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
        Token {.INTLIT = 3},
        Token {.PLUS = {}},
        Token {.EOF = {}}
    };

    const tokens2: []const Token = &[_]Token {
        Token {.INTLIT = 4},
        Token {.PLUS = {}},
        Token {.EOF = {}}
    };

    const tokens3: []const Token = &[_]Token {
        Token {.INTLIT = 3},
        Token {.MINUS = {}},
        Token {.EOF = {}}
    };

    const tokens4: []const Token = &[_]Token {
        Token {.INTLIT = 3},
        Token {.MINUS = {}},
        Token {.EOF = {}},
        Token {.EOF = {}}
    };

    const tokens5: []const Token = &[_]Token {
        Token {.INTLIT = 3},
        Token {.PLUS = {}},
        Token {.EOF = {}}
    };

    try expect(!tokens_eql(tokens1, tokens2));
    try expect(!tokens_eql(tokens1, tokens3));
    try expect(!tokens_eql(tokens1, tokens4));
    try expect(tokens_eql(tokens1, tokens5));
}

test "tokens_eql 2" {
    try expect(std.meta.eql(Token {.IDENT = "identifier"}, Token {.IDENT = "identifier"}));
}

test "tokens_eql 3" {
    const slice = "id";
    const tk1: Token = Token {.IDENT = "id"};
    const tk2: Token = Token {.IDENT = slice};
    try expect(std.meta.eql(tk1, tk2));
}

test "intlits" {

    const input = "12+2+891 + 21823 + 2";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.INTLIT = 12},
        Token {.PLUS = {}},
        Token {.INTLIT = 2},
        Token {.PLUS = {}},
        Token {.INTLIT = 891},
        Token {.PLUS = {}},
        Token {.INTLIT = 21823},
        Token {.PLUS = {}},
        Token {.INTLIT = 2},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}


test "whitespace" {

    const input = "++ + \t+\t\t+ \t\r\t+ \r+\r\t+  \t \t \t +";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.PLUS = {}},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}


test "operators" {

    const input = "(1+3)*\t( 8912 || 123 )&&  4/5==12";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.LPAREN = {}},
        Token {.INTLIT = 1},
        Token {.PLUS = {}},
        Token {.INTLIT = 3},
        Token {.RPAREN = {}},
        Token {.MUL = {}},
        Token {.LPAREN = {}},
        Token {.INTLIT = 8912},
        Token {.OR = {}},
        Token {.INTLIT = 123},
        Token {.RPAREN = {}},
        Token {.AND = {}},
        Token {.INTLIT = 4},
        Token {.DIV = {}},
        Token {.INTLIT = 5},
        Token {.DEQ = {}},
        Token {.INTLIT = 12},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "booleans" {

    const input = "!(true&&false)||!true==false";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.NOT = {}},
        Token {.LPAREN = {}},
        Token {.BOOLLIT = true},
        Token {.AND = {}},
        Token {.BOOLLIT = false},
        Token {.RPAREN = {}},
        Token {.OR = {}},
        Token {.NOT = {}},
        Token {.BOOLLIT = true},
        Token {.DEQ = {}},
        Token {.BOOLLIT = false},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "illegal" {

    const input = "12'32 ^23@3";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.INTLIT = 12},
        Token {.ILLEGAL = '\''},
        Token {.INTLIT = 32},
        Token {.ILLEGAL = '^'},
        Token {.INTLIT = 23},
        Token {.ILLEGAL = '@'},
        Token {.INTLIT = 3},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "bool literals" {

    const input = "true && false||true*false";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.BOOLLIT = true},
        Token {.AND = {}},
        Token {.BOOLLIT = false},
        Token {.OR = {}},
        Token {.BOOLLIT = true},
        Token {.MUL = {}},
        Token {.BOOLLIT = false},
        Token {.EOF = {}}
    };

    try expect(tokens_eql(tokens.items, expected));
}

test "keyword" {

    const input = "declare && declare4declare";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.DECLARE = {}},
        Token {.AND = {}},
        Token {.IDENT = "declare4declare"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "keyword 2" {

    const input = "print x declare y = 3";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.DECLARE = {}},
        Token {.IDENT = "y"},
        Token {.EQ = {}},
        Token {.INTLIT = 3},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "identifiers" {

    const input = "declare (bob == alice)||eve*b0b";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.DECLARE = {}},
        Token {.LPAREN = {}},
        Token {.IDENT = "bob"},
        Token {.DEQ = {}},
        Token {.IDENT = "alice"},
        Token {.RPAREN = {}},
        Token {.OR = {}},
        Token {.IDENT = "eve"},
        Token {.MUL = {}},
        Token {.IDENT = "b0b"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "assignment" {

    const input = "declare eeve=bob&&alice";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.DECLARE = {}},
        Token {.IDENT = "eeve"},
        Token {.EQ = {}},
        Token {.IDENT = "bob"},
        Token {.AND = {}},
        Token {.IDENT = "alice"},
        Token {.EOF = {}}
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
        Token {.LPAREN = {}},
        Token {.RPAREN = {}},
        Token {.LBRACK = {}},
        Token {.LPAREN = {}},
        Token {.RBRACK = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LBRACK = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "smart line breaks" {
    const input = "declare x=x+1\n\n\ndeclare";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.IDENT = "x"},
        Token {.PLUS = {}},
        Token {.INTLIT = 1},
        Token {.LB = {}},
        Token {.DECLARE = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "no line break before first statement" {
    const input = "\nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "no line break before first statement 2" {
    const input = "\n\n \n\n \nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "line break mixup" {
    const input = "\n\n \n\n \nprint x\n \n\n\r   \nprint x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line statement" {
    const input = "print x";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line statement 2" {
    const input = "\nprint x\n";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line statement 3" {
    const input = "\n\n \nprint x\n \n";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "single line two statements" {
    const input = "print x print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line two statements 2" {
    const input = "print x \n \n print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line two statements 3" {
    const input = "\n print x \n \n print y";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "single line with block statement" {
    const input = "\n print x \n \n {print y}";
    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 10},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
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
        Token {.IF = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.ELSE = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.IF = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.ELSE = {}},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.IF = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.ELSE = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
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
        Token {.IF = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.ELSE = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
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
        Token {.IF = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 10},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.LB = {}},
        Token {.ELSE = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
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
        Token {.WHILE = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);

}

test "inline while" {
    const input = "while x print x";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.WHILE = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "less than" {
    const input = "x < y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.IDENT = "x"},
        Token {.LT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than" {
    const input = "x > y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.IDENT = "x"},
        Token {.GT = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "less than equal" {
    const input = "x <= y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.IDENT = "x"},
        Token {.LTE = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}


test "greater than equal" {
    const input = "x >= y";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.IDENT = "x"},
        Token {.GTE = {}},
        Token {.IDENT = "y"},
        Token {.EOF = {}}
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
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.LB = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "break" {
    const input = "break";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.BREAK = {}},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "continue" {
    const input = "continue";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.CONTINUE = {}},
        Token {.EOF = {}}
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
        Token {.WHILE = {}},
        Token {.BOOLLIT = true},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.BREAK = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.WHILE = {}},
        Token {.BOOLLIT = true},
        Token {.LB = {}},
        Token {.LCURLY = {}},
        Token {.LB = {}},
        Token {.CONTINUE = {}},
        Token {.LB = {}},
        Token {.RCURLY = {}},
        Token {.EOF = {}}
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
        Token {.WHILE = {}},
        Token {.BOOLLIT = true},
        Token {.LB = {}},
        Token {.BREAK = {}},
        Token {.EOF = {}}
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
        Token {.WHILE = {}},
        Token {.BOOLLIT = true},
        Token {.LB = {}},
        Token {.BREAK = {}},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 1},
        Token {.COMMA = {}},
        Token {.IDENT = "y"},
        Token {.EQ = {}},
        Token {.INTLIT = 2},
        Token {.COMMA = {}},
        Token {.IDENT = "z"},
        Token {.COMMA = {}},
        Token {.IDENT = "w"},
        Token {.EQ = {}},
        Token {.INTLIT = 5},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 0},
        Token {.LB = {}},
        Token {.FUN = {}},
        Token {.IDENT = "last"},
        Token {.LPAREN = {}},
        Token {.IDENT = "x"},
        Token {.COMMA = {}},
        Token {.IDENT = "y"},
        Token {.COMMA = {}},
        Token {.IDENT = "z"},
        Token {.RPAREN = {}},
        Token {.LB = {}},
        Token {.RETURN = {}},
        Token {.IDENT = "z"},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "list"},
        Token {.LBRACK = {}},
        Token {.INTLIT = 10},
        Token {.RBRACK = {}},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "list"},
        Token {.LBRACK = {}},
        Token {.INTLIT = 10},
        Token {.RBRACK = {}},
        Token {.COMMA = {}},
        Token {.IDENT = "anotherlist"},
        Token {.LBRACK = {}},
        Token {.INTLIT = 5},
        Token {.RBRACK = {}},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.INTLIT = 10},
        Token {.EOF = {}}
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
        Token {.IDENT = "mylist"},
        Token {.DOT = {}},
        Token {.IDENT = "size"},
        Token {.EOF = {}}
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
        Token {.IDENT = "obj"},
        Token {.DOT = {}},
        Token {.IDENT = "prop1"},
        Token {.DOT = {}},
        Token {.IDENT = "prop2"},
        Token {.EOF = {}}
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
        Token {.IDENT = "obj"},
        Token {.LB = {}},
        Token {.DOT = {}},
        Token {.IDENT = "prop1"},
        Token {.LB = {}},
        Token {.DOT = {}},
        Token {.IDENT = "prop2"},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 0},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "x"},
        Token {.EQ = {}},
        Token {.INTLIT = 0},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "x"},
        Token {.EOF = {}}
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
        Token {.DECLARE = {}},
        Token {.IDENT = "my_"},
        Token {.LB = {}},
        Token {.PRINT = {}},
        Token {.IDENT = "my_"},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "float literal" {
    const input = "13.4";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.FLOATLIT = 13.4},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "short form float literal" {
    const input = "13.";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.FLOATLIT = 13.0},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "float expression" {
    const input = "13.4+1.-1.3434";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.FLOATLIT = 13.4},
        Token {.PLUS = {}},
        Token {.FLOATLIT = 1.0},
        Token {.MINUS = {}},
        Token {.FLOATLIT = 1.3434},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}

test "mixed float" {
    const input = "13.,42";

    var lx = lexer.Lexer.new(input, std.testing.allocator);

    const tokens: std.ArrayList(Token) = lx.lex();
    defer tokens.deinit();

    const expected: []const Token = &[_]Token{
        Token {.FLOATLIT = 13.0},
        Token {.COMMA = {}},
        Token {.INTLIT = 42},
        Token {.EOF = {}}
    };

    try std.testing.expectEqualDeep(expected, tokens.items);
}
