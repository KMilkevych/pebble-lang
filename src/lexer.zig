const std = @import("std");
const ArrayList = std.ArrayList;

const token = @import("token.zig");

// TODO: Make these comptime
fn createLU(comptime chars: []const u8) [256]bool {
    var arr: [256]bool = undefined;
    inline for (0..256) |i| arr[i] = false;
    inline for (chars) |c| arr[c] = true;
    return arr;
}

const is_numeric_digit: [256]bool = createLU("0123456789");
const is_real_digit: [256]bool = createLU(".0123456789");
const is_whitespace_digit: [256]bool = createLU(" \t\r");
const is_operator_digit: [256]bool = createLU("+-/*|&()[]{}=!<>\n,");
const is_ident_digit: [256]bool = createLU(
    "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
);

fn contains(arr: []const []const u8, item: []const u8) bool {
    for (arr) |v| if (std.mem.eql(u8, v, item)) return true;
    return false;
}

fn is_operator(chars: []const u8) bool {
    return contains(
        &[_][]const u8 { "\n", "+", "-", "*", "/", "!", "||", "&&", "==", "=", "(", ")", "[", "]", "{", "}", "<", ">", "<=", ">=", ","},
        chars
    );
}

fn operator_from_string(str: []const u8) token.Token {
    if (std.mem.eql(u8, str, "(")) return token.Token {.LPAREN = {}};
    if (std.mem.eql(u8, str, ")")) return token.Token {.RPAREN = {}};
    if (std.mem.eql(u8, str, "[")) return token.Token {.LBRACK = {}};
    if (std.mem.eql(u8, str, "]")) return token.Token {.RBRACK = {}};
    if (std.mem.eql(u8, str, "{")) return token.Token {.LCURLY = {}};
    if (std.mem.eql(u8, str, "}")) return token.Token {.RCURLY = {}};
    if (std.mem.eql(u8, str, "+")) return token.Token {.PLUS = {}};
    if (std.mem.eql(u8, str, "-")) return token.Token {.MINUS = {}};
    if (std.mem.eql(u8, str, "*")) return token.Token {.MUL = {}};
    if (std.mem.eql(u8, str, "/")) return token.Token {.DIV = {}};
    if (std.mem.eql(u8, str, "!")) return token.Token {.NOT = {}};
    if (std.mem.eql(u8, str, "||")) return token.Token {.OR = {}};
    if (std.mem.eql(u8, str, "&&")) return token.Token {.AND = {}};
    if (std.mem.eql(u8, str, "==")) return token.Token {.DEQ = {}};
    if (std.mem.eql(u8, str, "=")) return token.Token {.EQ = {}};
    if (std.mem.eql(u8, str, "<")) return token.Token {.LT = {}};
    if (std.mem.eql(u8, str, ">")) return token.Token {.GT = {}};
    if (std.mem.eql(u8, str, "<=")) return token.Token {.LTE = {}};
    if (std.mem.eql(u8, str, ">=")) return token.Token {.GTE = {}};
    if (std.mem.eql(u8, str, ",")) return token.Token {.COMMA = {}};
    if (std.mem.eql(u8, str, "\n")) return token.Token {.LB = {}};
    unreachable;
}

fn is_keyword(chars: []const u8) bool {
    return contains(&[_][]const u8 {"declare", "print", "if", "else", "while", "break", "continue"}, chars);
}

fn keyword_from_string(str: []const u8) token.Token {
    if (std.mem.eql(u8, str, "declare")) return token.Token {.DECLARE = {}};
    if (std.mem.eql(u8, str, "print")) return token.Token {.PRINT = {}};
    if (std.mem.eql(u8, str, "if")) return token.Token {.IF = {}};
    if (std.mem.eql(u8, str, "else")) return token.Token {.ELSE = {}};
    if (std.mem.eql(u8, str, "while")) return token.Token {.WHILE = {}};
    if (std.mem.eql(u8, str, "break")) return token.Token {.BREAK = {}};
    if (std.mem.eql(u8, str, "continue")) return token.Token {.CONTINUE = {}};
    unreachable;
}

fn is_bool_literal(chars: []const u8) bool {
    return contains(&[_][]const u8 {"true", "false"}, chars);
}

fn bool_literal_from_string(str: []const u8) token.Token {
    if (std.mem.eql(u8, str, "true")) return token.Token {.BOOLLIT = true};
    if (std.mem.eql(u8, str, "false")) return token.Token {.BOOLLIT = false};
    unreachable;
}

pub const Lexer = struct {

    const Self: type = @This();

    pos: usize,
    input: []const u8,
    allocator: std.mem.Allocator,


    pub fn new(input: []const u8, allocator: std.mem.Allocator) Self {
        return .{
            .pos = 0,
            .input = input,
            .allocator = allocator
        };
    }

    fn topdigit(self: *Self) error{EndOfFile}!u8 {
        if (self.pos >= self.input.len) return error.EndOfFile;
        return self.input[self.pos];
    }

    fn advance(self: *Self) error{EndOfFile}!u8 {
        self.pos += 1;
        return self.topdigit();
    }

    fn skipWhitespace(self: *Self) error{EndOfFile}!void {
        var c = try self.topdigit();
        while (is_whitespace_digit[c])
            c = try self.advance();
    }

    fn inputBackSlice(self: *Self, charcount: usize) []const u8 {
        return self.input[(self.pos - charcount)..self.pos];
    }

    fn lexInt(self: *Self) !token.Token {

        // Create a buffer for storing digit characters
        var buf: std.ArrayList(u8) = .init(self.allocator);
        defer buf.deinit();

        // Read full sequence of numeric digits
        // NOTE: we expect first digit to be valid
        var c = self.topdigit() catch unreachable;
        while (is_numeric_digit[c]) {
            try buf.append(c);
            c = self.advance() catch |err| switch (err) {
                error.EndOfFile => break,
                else => unreachable
            };
        }

        // Convert found digits into an int
        // NOTE: we only expect valid characters here
        const lit: i64 = std.fmt.parseInt(i64, buf.items, 10) catch |err| switch (err) {
            std.fmt.ParseIntError.Overflow =>
                return token.Token { .ERROR = self.inputBackSlice(buf.items.len) },
            std.fmt.ParseIntError.InvalidCharacter => unreachable
        };

        // Return constructed integer literal
        return token.Token {.INTLIT = lit};
    }

    fn lexIdent(self: *Self) !token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .init(self.allocator);
        defer buf.deinit();

        // Greedily collect identifier digits
        // NOTE: we expect first digit to be valid
        var c = self.topdigit() catch unreachable;
        while (is_ident_digit[c]) {
            try buf.append(c);
            c = self.advance() catch |err| switch (err) {
                error.EndOfFile => break,
                else => unreachable
            };
        }

        // Check if we are a boolean literal
        if (is_bool_literal(buf.items)) return bool_literal_from_string(buf.items);

        // Check if we are a keyword
        if (is_keyword(buf.items)) return keyword_from_string(buf.items);

        // Otherwise construct identifier
        return token.Token {.IDENT = self.inputBackSlice(buf.items.len)};
    }

    fn lexOperator(self: *Self) !token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .init(self.allocator);
        defer buf.deinit();

        // Greedily read operator digits
        // NOTE: we expect first digit to be valid
        var c = self.topdigit() catch unreachable;
        while (is_operator_digit[c]) {

            // Check if current sequence is valid as operator
            const current_is_legal: bool = is_operator(buf.items);

            // See if appending this character is still valid
            // NOTE: if we want to accept n-char operators, we need a valid
            // operators consisting of the first n-1, n-2, ... chars
            // OR the n-char operator should have no valid prefix
            try buf.append(c);
            if (!is_operator(buf.items) and current_is_legal) {
                _ = buf.pop();
                break;
            }

            // Advance char
            c = self.advance() catch |err| switch (err) {
                error.EndOfFile => break,
                else => unreachable
            };

        }

        // Try to produce token based on construction
        if (!is_operator(buf.items))
            return token.Token {.ERROR = self.inputBackSlice(buf.items.len)};

        return operator_from_string(buf.items);
    }

    pub fn lexToken(self: *Self) token.Token {

        // Container variable for result
        var tok: token.Token = undefined;

        // We start by skipping the whitespace
        self.skipWhitespace() catch |err| switch (err) {
            error.EndOfFile => return token.Token {.EOF = {}},
            else => unreachable
        };

        // Match against the first digit or EOF
        const c: u8 = self.topdigit() catch |err| switch (err) {
            error.EndOfFile => return token.Token {.EOF = {}},
            else => unreachable
        };

        // TODO: Remove unreachables...
        if (is_numeric_digit[c]) {
            tok = self.lexInt() catch unreachable;
        } else if (is_operator_digit[c]) {
            tok = self.lexOperator() catch unreachable;
        } else if (is_ident_digit[c]) {
            tok = self.lexIdent() catch unreachable;
        } else {
            // This is RAW token produce
            tok = token.Token {.ILLEGAL = c};
            self.pos += 1; // Force advance without check
        }

        // Return the resulting token
        return tok;

    }

    // TODO: Refactor this to not maybe not lie inside the Lexer
    fn pushLB(self: *Self, buf: *std.ArrayList(token.Token)) void {
        _ = self;
        // Insert line break if last token is not a line break
        // and there are items before this
        if (buf.items.len == 0) return;
        switch (buf.getLast()) {
            .LB => return,
            else => buf.append(token.Token {.LB = {}}) catch unreachable,
        }
    }

    pub fn lex(self: *Self) std.ArrayList(token.Token) {

        var res: std.ArrayList(token.Token) = .init(self.allocator);
        var tok: token.Token = undefined;

        var isStatement: bool = false;
        while (true) {

            tok = self.lexToken();
            switch (tok) {

                // Treat manual line breaks separetely
                .LB => {
                    // Need to account for following type of case:
                    // PRINT x      (print statement)
                    // x = x - 1    (expression statement)

                    if (isStatement) self.pushLB(&res);
                    isStatement = false;

                },

                // Allow line breaks before statement keywords
                // NOTE: Including single-word statements here to get LB error on "break x"
                .DECLARE, .PRINT, .IF, .ELSE, .WHILE, .BREAK, .CONTINUE => {
                    self.pushLB(&res);
                    res.append(tok) catch unreachable;
                    isStatement = true;
                },

                // Add a line break after block start
                .LCURLY => {
                    self.pushLB(&res);
                    res.append(tok) catch unreachable;
                    isStatement = false;
                },

                // Always insert line break before RCURLY
                .RCURLY => {
                    self.pushLB(&res);
                    res.append(tok) catch unreachable;
                    isStatement = false;
                },

                // EOF is the last token
                .EOF => {
                    res.append(tok) catch unreachable;
                    break;
                },

                // Otherwise, skip it
                else => {
                    if (!isStatement) self.pushLB(&res);
                    res.append(tok) catch unreachable;
                    isStatement = true;
                }
            }
        }

        return res;
    }

};
