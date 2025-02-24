const std = @import("std");

// TODO:
// 1. Segregate tokens by type
// 2. Add following token types:
//      - LESSTHAN, GREATERTHAN, NOTEQ
//      - ASSIGN
//      - SEMICOLON
//      - EQUALS
//
pub const TokenType: type = enum {
    INTLIT,
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
    LB,
    EOF,
    ERROR,
    ILLEGAL
};

// TODO: Token must know at all times where it is
pub const Token: type = union(TokenType) {

    // Literals and identifiers
    INTLIT: i64,
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

    // Utility
    LB: void,
    EOF: void,
    ERROR: []const u8,
    ILLEGAL: u8,

    pub fn format(
        self: Token,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .EOF      => try writer.print("[EOF    ]:", .{}),
            .LB       => try writer.print("[LB     ]:", .{}),
            .ERROR    => |val| try writer.print("[ERROR  ]: {s}", .{val}),
            .ILLEGAL  => |val| try writer.print("[ILLEGAL]: {c}", .{val}),
            .LPAREN   => try writer.print("[LPAREN ]:", .{}),
            .RPAREN   => try writer.print("[RPAREN ]:", .{}),
            .LBRACK   => try writer.print("[LBRACK ]:", .{}),
            .RBRACK   => try writer.print("[RBRACK ]:", .{}),
            .LCURLY   => try writer.print("[LCURLY ]:", .{}),
            .RCURLY   => try writer.print("[RCURLY ]:", .{}),
            .PLUS     => try writer.print("[PLUS   ]:", .{}),
            .MINUS    => try writer.print("[MINUS  ]:", .{}),
            .DIV      => try writer.print("[DIV    ]:", .{}),
            .MUL      => try writer.print("[MUL    ]:", .{}),
            .AND      => try writer.print("[AND    ]:", .{}),
            .OR       => try writer.print("[OR     ]:", .{}),
            .NOT      => try writer.print("[NOT    ]:", .{}),
            .DEQ      => try writer.print("[DEQ    ]:", .{}),
            .EQ       => try writer.print("[EQ     ]:", .{}),
            .LT       => try writer.print("[LT     ]:", .{}),
            .GT       => try writer.print("[GT     ]:", .{}),
            .LTE      => try writer.print("[LTE    ]:", .{}),
            .GTE      => try writer.print("[GTE    ]:", .{}),
            .DECLARE  => try writer.print("[DECLARE]:", .{}),
            .PRINT    => try writer.print("[PRINT  ]:", .{}),
            .IF       => try writer.print("[IF     ]:", .{}),
            .ELSE     => try writer.print("[ELSE   ]:", .{}),
            .WHILE    => try writer.print("[WHILE  ]:", .{}),
            .INTLIT   => |val| try writer.print("[INTLIT ]: {}", .{val}),
            .BOOLLIT  => |val| try writer.print("[BOOLLIT]: {}", .{val}),
            .IDENT    => |val| try writer.print("[IDENT  ]: {s}", .{val})
        }
    }

    pub fn getInfixPrecedence(self: Token) ?InfixPrecedence {
        // TODO:
        // Implement the remaining tokens here..
        return switch(self) {

            // Right-associative assignment
            .EQ           => InfixPrecedence {.left = 2, .right = 1},

            // The rest are left-associative
            .AND, .OR     => InfixPrecedence {.left = 3, .right = 4},

            .DEQ          => InfixPrecedence {.left = 5, .right = 6},

            .LT, .GT, .LTE, .GTE => InfixPrecedence {.left = 7, .right = 8},

            .PLUS, .MINUS => InfixPrecedence {.left = 9, .right = 10},
            .MUL, .DIV    => InfixPrecedence {.left = 11, .right = 12},

            // Some are not operators
            else => null
        };
    }

    pub fn getPrefixPrecedence(self: Token) ?u8 {
        return switch(self) {
            .MINUS, .NOT => 13,
            else => null
        };
    }
};

pub const InfixPrecedence: type = struct {
    left: u8,
    right: u8
};
