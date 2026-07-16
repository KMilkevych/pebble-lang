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
        writer: *std.Io.Writer
    ) !void {
        switch (self.tokenType) {
            .EOF      => try writer.print("[EOF     ]: \t| {f}", .{self.location}),
            .LB       => try writer.print("[LB      ]: \t| {f}", .{self.location}),
            .COMMA    => try writer.print("[COMMA   ]: \t| {f}", .{self.location}),
            .DOT      => try writer.print("[DOT     ]: \t| {f}", .{self.location}),
            .ERROR    => |val| try writer.print("[ERROR   ]: {s} \t| {f}", .{val, self.location}),
            .ILLEGAL  => |val| try writer.print("[ILLEGAL ]: {c} \t| {f}", .{val, self.location}),
            .LPAREN   => try writer.print("[LPAREN  ]: \t| {f}", .{self.location}),
            .RPAREN   => try writer.print("[RPAREN  ]: \t| {f}", .{self.location}),
            .LBRACK   => try writer.print("[LBRACK  ]: \t| {f}", .{self.location}),
            .RBRACK   => try writer.print("[RBRACK  ]: \t| {f}", .{self.location}),
            .LCURLY   => try writer.print("[LCURLY  ]: \t| {f}", .{self.location}),
            .RCURLY   => try writer.print("[RCURLY  ]: \t| {f}", .{self.location}),
            .PLUS     => try writer.print("[PLUS    ]: \t| {f}", .{self.location}),
            .MINUS    => try writer.print("[MINUS   ]: \t| {f}", .{self.location}),
            .DIV      => try writer.print("[DIV     ]: \t| {f}", .{self.location}),
            .MUL      => try writer.print("[MUL     ]: \t| {f}", .{self.location}),
            .AND      => try writer.print("[AND     ]: \t| {f}", .{self.location}),
            .OR       => try writer.print("[OR      ]: \t| {f}", .{self.location}),
            .NOT      => try writer.print("[NOT     ]: \t| {f}", .{self.location}),
            .DEQ      => try writer.print("[DEQ     ]: \t| {f}", .{self.location}),
            .EQ       => try writer.print("[EQ      ]: \t| {f}", .{self.location}),
            .LT       => try writer.print("[LT      ]: \t| {f}", .{self.location}),
            .GT       => try writer.print("[GT      ]: \t| {f}", .{self.location}),
            .LTE      => try writer.print("[LTE     ]: \t| {f}", .{self.location}),
            .GTE      => try writer.print("[GTE     ]: \t| {f}", .{self.location}),
            .DECLARE  => try writer.print("[DECLARE ]: \t| {f}", .{self.location}),
            .PRINT    => try writer.print("[PRINT   ]: \t| {f}", .{self.location}),
            .IF       => try writer.print("[IF      ]: \t| {f}", .{self.location}),
            .ELSE     => try writer.print("[ELSE    ]: \t| {f}", .{self.location}),
            .WHILE    => try writer.print("[WHILE   ]: \t| {f}", .{self.location}),
            .BREAK    => try writer.print("[BREAK   ]: \t| {f}", .{self.location}),
            .CONTINUE => try writer.print("[CONTINUE]: \t| {f}", .{self.location}),
            .RETURN   => try writer.print("[RETURN  ]: \t| {f}", .{self.location}),
            .FUN      => try writer.print("[FUN     ]: \t| {f}", .{self.location}),
            .INT      => try writer.print("[INT     ]: \t| {f}", .{self.location}),
            .FLOAT    => try writer.print("[FLOAT   ]: \t| {f}", .{self.location}),
            .BOOL     => try writer.print("[BOOL    ]: \t| {f}", .{self.location}),
            .AS       => try writer.print("[AS      ]: \t| {f}", .{self.location}),
            .INTLIT   => |val| try writer.print("[INTLIT  ]: {d} \t| {f}", .{val, self.location}),
            .FLOATLIT => |val| try writer.print("[FLOATLIT]: {d} \t| {f}", .{val, self.location}),
            .BOOLLIT  => |val| try writer.print("[BOOLLIT ]: {} \t| {f}", .{val, self.location}),
            .IDENT    => |val| try writer.print("[IDENT   ]: {s} \t| {f}", .{val, self.location})
        }
    }

};

pub const InfixPrecedence: type = struct {
    left: u8,
    right: u8
};
