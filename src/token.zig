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

    location: loc.LocationRange = loc.LocationRange {
        .from = loc.Location {.file = "", .column = 0, .line = 0 },
        .to = loc.Location {.file = "", .column = 0, .line = 0 },
    },
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
            .EOF      => try writer.print("[EOF     ]: \t| {}", .{self.location}),
            .LB       => try writer.print("[LB      ]: \t| {}", .{self.location}),
            .COMMA    => try writer.print("[COMMA   ]: \t| {}", .{self.location}),
            .DOT      => try writer.print("[DOT     ]: \t| {}", .{self.location}),
            .ERROR    => |val| try writer.print("[ERROR   ]: {s} \t| {}", .{val, self.location}),
            .ILLEGAL  => |val| try writer.print("[ILLEGAL ]: {c} \t| {}", .{val, self.location}),
            .LPAREN   => try writer.print("[LPAREN  ]: \t| {}", .{self.location}),
            .RPAREN   => try writer.print("[RPAREN  ]: \t| {}", .{self.location}),
            .LBRACK   => try writer.print("[LBRACK  ]: \t| {}", .{self.location}),
            .RBRACK   => try writer.print("[RBRACK  ]: \t| {}", .{self.location}),
            .LCURLY   => try writer.print("[LCURLY  ]: \t| {}", .{self.location}),
            .RCURLY   => try writer.print("[RCURLY  ]: \t| {}", .{self.location}),
            .PLUS     => try writer.print("[PLUS    ]: \t| {}", .{self.location}),
            .MINUS    => try writer.print("[MINUS   ]: \t| {}", .{self.location}),
            .DIV      => try writer.print("[DIV     ]: \t| {}", .{self.location}),
            .MUL      => try writer.print("[MUL     ]: \t| {}", .{self.location}),
            .AND      => try writer.print("[AND     ]: \t| {}", .{self.location}),
            .OR       => try writer.print("[OR      ]: \t| {}", .{self.location}),
            .NOT      => try writer.print("[NOT     ]: \t| {}", .{self.location}),
            .DEQ      => try writer.print("[DEQ     ]: \t| {}", .{self.location}),
            .EQ       => try writer.print("[EQ      ]: \t| {}", .{self.location}),
            .LT       => try writer.print("[LT      ]: \t| {}", .{self.location}),
            .GT       => try writer.print("[GT      ]: \t| {}", .{self.location}),
            .LTE      => try writer.print("[LTE     ]: \t| {}", .{self.location}),
            .GTE      => try writer.print("[GTE     ]: \t| {}", .{self.location}),
            .DECLARE  => try writer.print("[DECLARE ]: \t| {}", .{self.location}),
            .PRINT    => try writer.print("[PRINT   ]: \t| {}", .{self.location}),
            .IF       => try writer.print("[IF      ]: \t| {}", .{self.location}),
            .ELSE     => try writer.print("[ELSE    ]: \t| {}", .{self.location}),
            .WHILE    => try writer.print("[WHILE   ]: \t| {}", .{self.location}),
            .BREAK    => try writer.print("[BREAK   ]: \t| {}", .{self.location}),
            .CONTINUE => try writer.print("[CONTINUE]: \t| {}", .{self.location}),
            .RETURN   => try writer.print("[RETURN  ]: \t| {}", .{self.location}),
            .FUN      => try writer.print("[FUN     ]: \t| {}", .{self.location}),
            .INT      => try writer.print("[INT     ]: \t| {}", .{self.location}),
            .FLOAT    => try writer.print("[FLOAT   ]: \t| {}", .{self.location}),
            .BOOL     => try writer.print("[BOOL    ]: \t| {}", .{self.location}),
            .AS       => try writer.print("[AS      ]: \t| {}", .{self.location}),
            .INTLIT   => |val| try writer.print("[INTLIT  ]: {} \t| {}", .{val, self.location}),
            .FLOATLIT => |val| try writer.print("[FLOATLIT]: {} \t| {}", .{val, self.location}),
            .BOOLLIT  => |val| try writer.print("[BOOLLIT ]: {} \t| {}", .{val, self.location}),
            .IDENT    => |val| try writer.print("[IDENT   ]: {s} \t| {}", .{val, self.location})
        }
    }

};

pub const InfixPrecedence: type = struct {
    left: u8,
    right: u8
};
