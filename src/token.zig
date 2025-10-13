const std = @import("std");
const loc = @import("location.zig");

const TokenTypeInner: type = enum {
    INTLIT,
    FLOATLIT,
    BOOLLIT,
    IDENT,
    PLUS,
    MINUS,
    MUL,
    DIV,
    AND,
    OR,
    NOT,
    DEQ,
    EQ,
    LT,
    GT,
    LTE,
    GTE,
    LPAREN,
    RPAREN,
    LBRACK,
    RBRACK,
    LCURLY,
    RCURLY,
    DECLARE,
    PRINT,
    IF,
    ELSE,
    WHILE,
    BREAK,
    CONTINUE,
    RETURN,
    FUN,
    INT,
    FLOAT,
    BOOL,
    AS,
    LB,
    COMMA,
    DOT,
    EOF,
    ERROR,
    ILLEGAL
};

pub const TokenType: type = union(TokenTypeInner) {

    // Literals and identifiers
    INTLIT: i64,
    FLOATLIT: f64,
    BOOLLIT: bool,
    IDENT: []const u8,

    // Operators
    PLUS: void,
    MINUS: void,
    MUL: void,
    DIV: void,
    AND: void,
    OR: void,
    NOT: void,
    DEQ: void,
    EQ: void,
    LT: void,
    GT: void,
    LTE: void,
    GTE: void,

    // Parentheses
    LPAREN: void,
    RPAREN: void,
    LBRACK: void,
    RBRACK: void,
    LCURLY: void,
    RCURLY: void,

    // Keywords
    DECLARE: void,
    PRINT: void,
    IF: void,
    ELSE: void,
    WHILE: void,
    BREAK: void,
    CONTINUE: void,
    RETURN: void,
    FUN: void,
    INT: void,
    FLOAT: void,
    BOOL: void,
    AS: void,

    // Utility
    LB: void,
    COMMA: void,
    DOT: void,
    EOF: void,
    ERROR: []const u8,
    ILLEGAL: u8,

    pub fn getInfixPrecedence(self: TokenType) ?InfixPrecedence {

        return switch(self) {


            // PLACE GT LTE HERE
            .GT => InfixPrecedence {.left = 1, .right = 2},
            .LT => InfixPrecedence {.left = 3, .right = 4},
            .LTE, .GTE => InfixPrecedence {.left = 5, .right = 6},

            // Right-associative assignment
            .EQ           => InfixPrecedence {.left = 8, .right = 7},

            // The rest are left-associative
            .AND, .OR     => InfixPrecedence {.left = 9, .right = 10},

            .DEQ          => InfixPrecedence {.left = 11, .right = 12},

            .AS           => InfixPrecedence {.left = 13, .right = 14},

            .PLUS, .MINUS => InfixPrecedence {.left = 15, .right = 16},
            .MUL, .DIV    => InfixPrecedence {.left = 17, .right = 18},

            .DOT => InfixPrecedence {.left = 25, .right = 26},

            // Some are not operators
            else => null
        };
    }

    pub fn getPrefixPrecedence(self: TokenType) ?u8 {
        return switch(self) {
            .MINUS, .NOT => 19,
            else => null
        };
    }

    pub fn getPostfixPrecedence(self: TokenType) ?u8 {
        return switch(self) {
            .LPAREN => 21,
            .LBRACK => 23,
            else => null
        };
    }
};

pub const Token: type = struct {

    location: loc.Location = .{.file = "", .column = 0, .line = 0},
    tokenType: TokenType,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch (self.tokenType) {
            .EOF      => try writer.print("[EOF     ]: | {}", .{self.location}),
            .LB       => try writer.print("[LB      ]: | {}", .{self.location}),
            .COMMA    => try writer.print("[COMMA   ]: | {}", .{self.location}),
            .DOT      => try writer.print("[DOT     ]: | {}", .{self.location}),
            .ERROR    => |val| try writer.print("[ERROR   ]: {s} | {}", .{val, self.location}),
            .ILLEGAL  => |val| try writer.print("[ILLEGAL ]: {c} | {}", .{val, self.location}),
            .LPAREN   => try writer.print("[LPAREN  ]: | {}", .{self.location}),
            .RPAREN   => try writer.print("[RPAREN  ]: | {}", .{self.location}),
            .LBRACK   => try writer.print("[LBRACK  ]: | {}", .{self.location}),
            .RBRACK   => try writer.print("[RBRACK  ]: | {}", .{self.location}),
            .LCURLY   => try writer.print("[LCURLY  ]: | {}", .{self.location}),
            .RCURLY   => try writer.print("[RCURLY  ]: | {}", .{self.location}),
            .PLUS     => try writer.print("[PLUS    ]: | {}", .{self.location}),
            .MINUS    => try writer.print("[MINUS   ]: | {}", .{self.location}),
            .DIV      => try writer.print("[DIV     ]: | {}", .{self.location}),
            .MUL      => try writer.print("[MUL     ]: | {}", .{self.location}),
            .AND      => try writer.print("[AND     ]: | {}", .{self.location}),
            .OR       => try writer.print("[OR      ]: | {}", .{self.location}),
            .NOT      => try writer.print("[NOT     ]: | {}", .{self.location}),
            .DEQ      => try writer.print("[DEQ     ]: | {}", .{self.location}),
            .EQ       => try writer.print("[EQ      ]: | {}", .{self.location}),
            .LT       => try writer.print("[LT      ]: | {}", .{self.location}),
            .GT       => try writer.print("[GT      ]: | {}", .{self.location}),
            .LTE      => try writer.print("[LTE     ]: | {}", .{self.location}),
            .GTE      => try writer.print("[GTE     ]: | {}", .{self.location}),
            .DECLARE  => try writer.print("[DECLARE ]: | {}", .{self.location}),
            .PRINT    => try writer.print("[PRINT   ]: | {}", .{self.location}),
            .IF       => try writer.print("[IF      ]: | {}", .{self.location}),
            .ELSE     => try writer.print("[ELSE    ]: | {}", .{self.location}),
            .WHILE    => try writer.print("[WHILE   ]: | {}", .{self.location}),
            .BREAK    => try writer.print("[BREAK   ]: | {}", .{self.location}),
            .CONTINUE => try writer.print("[CONTINUE]: | {}", .{self.location}),
            .RETURN   => try writer.print("[RETURN  ]: | {}", .{self.location}),
            .FUN      => try writer.print("[FUN     ]: | {}", .{self.location}),
            .INT      => try writer.print("[INT     ]: | {}", .{self.location}),
            .FLOAT    => try writer.print("[FLOAT   ]: | {}", .{self.location}),
            .BOOL     => try writer.print("[BOOL    ]: | {}", .{self.location}),
            .AS       => try writer.print("[AS      ]: | {}", .{self.location}),
            .INTLIT   => |val| try writer.print("[INTLIT  ]: {} | {}", .{val, self.location}),
            .FLOATLIT => |val| try writer.print("[FLOATLIT]: {} | {}", .{val, self.location}),
            .BOOLLIT  => |val| try writer.print("[BOOLLIT ]: {} | {}", .{val, self.location}),
            .IDENT    => |val| try writer.print("[IDENT   ]: {s} | {}", .{val, self.location})
        }
    }

};

pub const InfixPrecedence: type = struct {
    left: u8,
    right: u8
};
