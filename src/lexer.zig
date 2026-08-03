const std = @import("std");
const ArrayList = std.ArrayList;

const token = @import("token.zig");
const loc = @import("location.zig");
const log = @import("logger.zig");

pub const LexError = error {
    UnclosedString,
    NotSingleCharacter,
    EndOfFile,
    InvalidCharacter,
    LiteralTooLarge,
};

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
const is_operator_digit: [256]bool = createLU("+-/*|&()[]{}=!<>\n,.");
const is_ident_digit: [256]bool = createLU(
    "abcdefghijklmnopqrstuvwxyz_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
);
const is_comment_digit: [256]bool = createLU(";#");
const is_string_digit: [256]bool = createLU("\"");
const is_char_digit: [256]bool = createLU("'");

fn contains(arr: []const []const u8, item: []const u8) bool {
    for (arr) |v| if (std.mem.eql(u8, v, item)) return true;
    return false;
}

fn is_operator(chars: []const u8) bool {
    return contains(
        &[_][]const u8 { "\n", "+", "-", "*", "/", "!", "||", "&&", "==", "=", "(", ")", "[", "]", "{", "}", "<", ">", "<=", ">=", ",", "."},
        chars
    );
}

fn operator_from_string(str: []const u8) token.TokenType {
    if (std.mem.eql(u8, str, "(")) return token.TokenType {.LPAREN = {}};
    if (std.mem.eql(u8, str, ")")) return token.TokenType {.RPAREN = {}};
    if (std.mem.eql(u8, str, "[")) return token.TokenType {.LBRACK = {}};
    if (std.mem.eql(u8, str, "]")) return token.TokenType {.RBRACK = {}};
    if (std.mem.eql(u8, str, "{")) return token.TokenType {.LCURLY = {}};
    if (std.mem.eql(u8, str, "}")) return token.TokenType {.RCURLY = {}};
    if (std.mem.eql(u8, str, "+")) return token.TokenType {.PLUS = {}};
    if (std.mem.eql(u8, str, "-")) return token.TokenType {.MINUS = {}};
    if (std.mem.eql(u8, str, "*")) return token.TokenType {.MUL = {}};
    if (std.mem.eql(u8, str, "/")) return token.TokenType {.DIV = {}};
    if (std.mem.eql(u8, str, "!")) return token.TokenType {.NOT = {}};
    if (std.mem.eql(u8, str, "||")) return token.TokenType {.OR = {}};
    if (std.mem.eql(u8, str, "&&")) return token.TokenType {.AND = {}};
    if (std.mem.eql(u8, str, "==")) return token.TokenType {.DEQ = {}};
    if (std.mem.eql(u8, str, "=")) return token.TokenType {.EQ = {}};
    if (std.mem.eql(u8, str, "<")) return token.TokenType {.LT = {}};
    if (std.mem.eql(u8, str, ">")) return token.TokenType {.GT = {}};
    if (std.mem.eql(u8, str, "<=")) return token.TokenType {.LTE = {}};
    if (std.mem.eql(u8, str, ">=")) return token.TokenType {.GTE = {}};
    if (std.mem.eql(u8, str, ",")) return token.TokenType {.COMMA = {}};
    if (std.mem.eql(u8, str, ".")) return token.TokenType {.DOT = {}};
    if (std.mem.eql(u8, str, "\n")) return token.TokenType {.LB = {}};
    unreachable;
}

fn is_keyword(chars: []const u8) bool {
    return contains(&[_][]const u8 {"declare", "print", "if", "else", "while", "break", "continue", "return", "function", "and", "or", "as", "Int", "Float", "Bool", "Char"}, chars);
}

fn keyword_from_string(str: []const u8) token.TokenType {
    if (std.mem.eql(u8, str, "declare")) return token.TokenType {.DECLARE = {}};
    if (std.mem.eql(u8, str, "print")) return token.TokenType {.PRINT = {}};
    if (std.mem.eql(u8, str, "if")) return token.TokenType {.IF = {}};
    if (std.mem.eql(u8, str, "else")) return token.TokenType {.ELSE = {}};
    if (std.mem.eql(u8, str, "while")) return token.TokenType {.WHILE = {}};
    if (std.mem.eql(u8, str, "break")) return token.TokenType {.BREAK = {}};
    if (std.mem.eql(u8, str, "continue")) return token.TokenType {.CONTINUE = {}};
    if (std.mem.eql(u8, str, "return")) return token.TokenType {.RETURN = {}};
    if (std.mem.eql(u8, str, "function")) return token.TokenType {.FUN = {}};
    if (std.mem.eql(u8, str, "and")) return token.TokenType {.AND = {}};
    if (std.mem.eql(u8, str, "or")) return token.TokenType {.OR = {}};
    if (std.mem.eql(u8, str, "as")) return token.TokenType {.AS = {}};
    if (std.mem.eql(u8, str, "Int")) return token.TokenType {.INT = {}};
    if (std.mem.eql(u8, str, "Float")) return token.TokenType {.FLOAT = {}};
    if (std.mem.eql(u8, str, "Bool")) return token.TokenType {.BOOL = {}};
    if (std.mem.eql(u8, str, "Char")) return token.TokenType {.CHAR = {}};
    unreachable;
}

fn is_bool_literal(chars: []const u8) bool {
    return contains(&[_][]const u8 {"true", "false"}, chars);
}

fn bool_literal_from_string(str: []const u8) token.TokenType {
    if (std.mem.eql(u8, str, "true")) return token.TokenType {.BOOLLIT = true};
    if (std.mem.eql(u8, str, "false")) return token.TokenType {.BOOLLIT = false};
    unreachable;
}

pub const Lexer = struct {

    const Self: type = @This();

    pos: usize,
    input: []const u8,
    file: []const u8,
    allocator: std.mem.Allocator,

    logger: *log.Logger,

    line: usize = 0,
    column: usize = 0,

    storeLocation: bool = true,

    pub fn new(input: []const u8, file: []const u8, allocator: std.mem.Allocator, errLogger: *log.Logger) Self {
        return .{
            .pos = 0,
            .input = input,
            .file = file,
            .allocator = allocator,
            .logger = errLogger,
        };
    }

    fn getLocationAtPoint(self: *Self, offset: usize) loc.LocationRange {
        if (!self.storeLocation) return loc.LocationRange {
            .from = loc.Location {
                .file = "",
                .line = 0,
                .column = 0
            },
            .to = loc.Location {
                .file = "",
                .line = 0,
                .column = 0
            },
        };
        return loc.LocationRange {
            .from = loc.Location {
                .file = self.file,
                .column = self.column + offset,
                .line = self.line
            },
            .to = loc.Location {
                .file = self.file,
                .column = self.column + offset,
                .line = self.line
            },
        };
    }


    fn getLocationRange(self: *Self, length: usize) loc.LocationRange {
        if (!self.storeLocation) return loc.LocationRange {
            .from = loc.Location {
                .file = "",
                .line = 0,
                .column = 0
            },
            .to = loc.Location {
                .file = "",
                .line = 0,
                .column = 0
            },
        };
        return loc.LocationRange {
            .from = loc.Location {
                .file = self.file,
                .column = self.column - length,
                .line = self.line
            },
            .to = loc.Location {
                .file = self.file,
                .column = self.column - 1,
                .line = self.line
            },
        };
    }

    fn advanceLine(self: *Self) void {
        self.column = 0;
        self.line += 1;
    }

    fn topdigit(self: *Self) LexError!u8 {
        if (self.pos >= self.input.len) return LexError.EndOfFile;
        return self.input[self.pos];
    }

    fn advance(self: *Self) LexError!u8 {
        self.pos += 1;
        self.column += 1;
        return self.topdigit();
    }

    fn skipWhitespace(self: *Self) LexError!void {
        var c = try self.topdigit();
        while (is_whitespace_digit[c])
            c = try self.advance();
    }

    fn inputBackSlice(self: *Self, charcount: usize, revoffset: usize) []const u8 {
        return self.input[(self.pos - charcount - revoffset)..(self.pos - revoffset)];
    }

    fn lexComment(self: *Self) LexError!void {
        var c = try self.topdigit();
        while (c != '\n') {
            c = self.advance() catch |err| switch (err) {
                LexError.EndOfFile => break,
                else => unreachable
            };
        }
    }

    fn lexIntOrFloat(self: *Self) LexError!token.Token {

        // Create a buffer for storing digit characters
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);

        // Read full sequence of numeric digits
        // NOTE: we expect first digit to be valid
        var c = try self.topdigit();
        while (is_numeric_digit[c]) {
            buf.append(self.allocator, c) catch unreachable;
            c = self.advance() catch |err| switch (err) {
                LexError.EndOfFile => break,
                else => unreachable
            };
        }

        // Check if we were able to advance
        if (self.topdigit()) |_| {

            // Check if we are in a real
            if (is_real_digit[c]) {

                // Append decimal separator
                buf.append(self.allocator, c) catch unreachable;

                // Lex remainder of float
                if (self.advance()) |val| {
                    c = val;

                    while (is_numeric_digit[c]) {
                        buf.append(self.allocator, c) catch unreachable;
                        c = self.advance() catch |err| switch (err) {
                            LexError.EndOfFile => break,
                            else => unreachable
                        };
                    }
                }
                else |_| {}

                // Return lexed float literal
                const lit: f64 = std.fmt.parseFloat(f64, buf.items) catch |err| switch (err) {
                    std.fmt.ParseFloatError.InvalidCharacter => unreachable
                };

                return token.Token {
                    .tokenType = token.TokenType { .FLOATLIT = lit },
                    .location = self.getLocationRange(buf.items.len)
                };
            }
        } else |_| {}


        // Convert found digits into an int
        // NOTE: we only expect valid characters here
        const lit: i64 = std.fmt.parseInt(i64, buf.items, 10) catch |err| switch (err) {
            std.fmt.ParseIntError.Overflow => {
                self.logger.logError(LexError.LiteralTooLarge, self.getLocationRange(buf.items.len));
                return LexError.LiteralTooLarge;
            },
            std.fmt.ParseIntError.InvalidCharacter => unreachable
        };

        // Return constructed integer literal
        return token.Token {
            .tokenType = token.TokenType { .INTLIT = lit },
            .location = self.getLocationRange(buf.items.len)
        };
    }

    fn lexIdent(self: *Self) LexError!token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);

        // Greedily collect identifier digits
        // NOTE: we expect first digit to be valid
        var c = try self.topdigit();
        while (is_ident_digit[c]) {
            buf.append(self.allocator, c) catch unreachable;
            c = self.advance() catch |err| switch (err) {
                LexError.EndOfFile => break,
                else => unreachable
            };
        }

        // Check if we are a boolean literal
        if (is_bool_literal(buf.items)) return token.Token {
            .tokenType = bool_literal_from_string(buf.items),
            .location = self.getLocationRange(buf.items.len)
        };

        // Check if we are a keyword
        if (is_keyword(buf.items)) return token.Token {
            .tokenType = keyword_from_string(buf.items),
            .location = self.getLocationRange(buf.items.len)
        };

        // Otherwise construct identifier
        return token.Token {
            .tokenType = token.TokenType {
                .IDENT = self.inputBackSlice(buf.items.len, 0)
            },
            .location = self.getLocationRange(buf.items.len)
        };
    }

    fn lexString(self: *Self) LexError!token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);

        // Greedily collect identifier digits
        // NOTE: we expect first digit to be valid
        var c = try self.advance();
        var escapes: usize = 0;
        while (!is_string_digit[c]) {

            // ESCAPE CHARACTERS:
            // if the character is a backslash, consume the next one as well
            if (c == '\\') {
                escapes += 1;
                const k = try self.advance();
                c = switch (k) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    else => k,
                };
            }

            buf.append(self.allocator, c) catch unreachable;
            c = self.advance() catch |err| {
                self.logger.logError(LexError.UnclosedString, self.getLocationRange(buf.items.len + escapes));
                return err;
            };
        }

        // TODO: decide what to do here
        _ = try self.advance(); // consume the closing "


        // Construct string literal
        return token.Token {
            .tokenType = token.TokenType {
                .STRINGLIT = self.allocator.dupe(u8, buf.items) catch unreachable,
            },
            // NOTE: we must include the quotes in location range
            .location = self.getLocationRange(buf.items.len + escapes)
        };
    }

    fn lexChar(self: *Self) LexError!token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);

        // Greedily collect identifier digits
        // NOTE: we expect first digit to be valid
        var c = try self.advance();
        var escapes: usize = 0;
        while (!is_char_digit[c]) {

            // ESCAPE CHARACTERS:
            // if the character is a backslash, consume the next one as well
            if (c == '\\') {
                escapes += 1;
                const k = try self.advance();
                c = switch (k) {
                    'n' => '\n',
                    't' => '\t',
                    'r' => '\r',
                    '\\' => '\\',
                    else => k,
                };
            }

            buf.append(self.allocator, c) catch unreachable;
            c = self.advance() catch |err| {
                self.logger.logError(LexError.UnclosedString, self.getLocationRange(buf.items.len + escapes));
                return err;
            };
        }

        // TODO: Decide what to do here
        _ = try self.advance(); // consume the closing '

        // Constructing CHAR or STRING literal
        if (buf.items.len != 1) {
            self.logger.logError(LexError.NotSingleCharacter, self.getLocationRange(buf.items.len + escapes));
            return token.Token {
                .tokenType = token.TokenType {
                    .STRINGLIT = self.allocator.dupe(u8, buf.items) catch unreachable,
                },
                // NOTE: we must include the quotes in location range
                .location = self.getLocationRange(buf.items.len + escapes)
            };
        } else {
            return token.Token {
                .tokenType = token.TokenType {
                    .CHARLIT = buf.items[0],
                },
                // NOTE: we must include the quotes in location range
                .location = self.getLocationRange(buf.items.len + escapes)
            };
        }

    }

    fn lexOperator(self: *Self) LexError!token.Token {

        // Create buffer for storing operator characters
        var buf: std.ArrayList(u8) = .empty;
        defer buf.deinit(self.allocator);

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
            buf.append(self.allocator, c) catch unreachable;
            if (!is_operator(buf.items) and current_is_legal) {
                _ = buf.pop();
                break;
            }

            // Advance char unless we have end of file
            c = self.advance() catch |err| switch (err) {
                LexError.EndOfFile => break,
                else => return err
            };

        }

        // Try to produce token based on construction
        if (!is_operator(buf.items))
            return token.Token {
                .tokenType = token.TokenType {
                    .ERROR = self.inputBackSlice(buf.items.len, 0)
                },
                .location = self.getLocationRange(buf.items.len)
            };

        // Construct token with location
        const tok = token.Token {
            .tokenType = operator_from_string(buf.items),
            .location = self.getLocationRange(buf.items.len),
        };

        // Reset line
        switch (tok.tokenType) {
            .LB => self.advanceLine(),
            else => {}
        }

        return tok;
    }

    pub fn lexToken(self: *Self) token.Token {

        // Container variable for result
        var tok: token.Token = undefined;

        // We start by skipping the whitespace
        self.skipWhitespace() catch |err| switch (err) {
            LexError.EndOfFile => return token.Token {
                .tokenType = token.TokenType {.EOF = {}},
                .location = self.getLocationAtPoint(0),
            },
            else => unreachable
        };

        // Match against the first digit or EOF
        const c: u8 = self.topdigit() catch |err| switch (err) {
            LexError.EndOfFile => return token.Token {
                .tokenType = token.TokenType {.EOF = {}},
                .location = self.getLocationAtPoint(0),
            },
            else => unreachable
        };

        // Lex the next token
        var next_token: LexError!token.Token = undefined;
        if (is_numeric_digit[c]) {
            next_token = self.lexIntOrFloat();
        } else if (is_operator_digit[c]) {
            next_token = self.lexOperator();
        } else if (is_ident_digit[c]) {
            next_token = self.lexIdent();
        } else if (is_comment_digit[c]) {
            self.lexComment() catch {};
            next_token = self.lexToken();
        } else if (is_string_digit[c]) {
            next_token = self.lexString();
        } else if (is_char_digit[c]) {
            next_token = self.lexChar();
        } else {
            // This is RAW token produce
            next_token = token.Token {
                .tokenType = token.TokenType {.ILLEGAL = c},
                .location = self.getLocationRange(1),
            };
            self.pos += 1; // Force advance without check
        }

        // All errors have been handled bottom-level
        tok = next_token catch |err| blk: {
            self.logger.logError(err, self.getLocationRange(1));
            break :blk token.Token {
                .tokenType = token.TokenType {.ERROR = ""},
                .location = self.getLocationRange(1)
            };
        };

        // Return the resulting token
        return tok;

    }

    fn pushLB(self: *Self, buf: *std.ArrayList(token.Token)) void {
        // Insert line break if last token is not a line break
        // and there are items before this
        if (buf.items.len == 0) return;
        const lastToken: token.Token = buf.getLast();
        switch (lastToken.tokenType) {
            .LB => return,
            else => buf.append(
                self.allocator,
                token.Token {
                    .tokenType = token.TokenType {.LB = {}},
                    .location = if (!self.storeLocation) loc.LocationRange {
                        .from = loc.Location {
                            .file = lastToken.location.to.file,
                            .column = lastToken.location.from.column,
                            .line = lastToken.location.from.line
                        },
                        .to = loc.Location {
                            .file = lastToken.location.to.file,
                            .column = lastToken.location.to.column,
                            .line = lastToken.location.to.line
                        }
                    } else loc.LocationRange {
                        .from = loc.Location {
                            .file = lastToken.location.to.file,
                            .column = lastToken.location.to.column+1,
                            .line = lastToken.location.to.line
                        },
                        .to = loc.Location {
                            .file = lastToken.location.to.file,
                            .column = lastToken.location.to.column+1,
                            .line = lastToken.location.to.line
                        }
                    }
                }
            ) catch unreachable,
        }
    }

    pub fn lex(self: *Self) std.ArrayList(token.Token) {

        var res: std.ArrayList(token.Token) = .empty;
        var tok: token.Token = undefined;

        var isStatement: bool = false;
        while (true) {

            tok = self.lexToken();
            switch (tok.tokenType) {

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
                .DECLARE, .PRINT, .IF, .ELSE, .WHILE, .BREAK, .CONTINUE, .RETURN, .FUN => {
                    self.pushLB(&res);
                    res.append(self.allocator, tok) catch unreachable;
                    isStatement = true;
                },

                // Add a line break after block start
                .LCURLY => {
                    self.pushLB(&res);
                    res.append(self.allocator, tok) catch unreachable;
                    isStatement = false;
                },

                // Always insert line break before RCURLY
                .RCURLY => {
                    self.pushLB(&res);
                    res.append(self.allocator, tok) catch unreachable;
                    isStatement = false;
                },

                // EOF is the last token
                .EOF => {
                    res.append(self.allocator, tok) catch unreachable;
                    break;
                },

                // Otherwise, skip it
                else => {
                    if (!isStatement) self.pushLB(&res);
                    res.append(self.allocator, tok) catch unreachable;
                    isStatement = true;
                }
            }
        }

        return res;
    }

};
