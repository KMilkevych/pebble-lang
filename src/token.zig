const std = @import("std");

pub const TokenType: type = enum {
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
    LB,
    COMMA,
    DOT,
    EOF,
    ERROR,
    ILLEGAL
};

// TODO: Token must know at all times where it is
pub const Token: type = union(TokenType) {

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

    // Utility
    LB: void,
    COMMA: void,
    DOT: void,
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
            .EOF      => try writer.print("[EOF     ]:", .{}),
            .LB       => try writer.print("[LB      ]:", .{}),
            .COMMA    => try writer.print("[COMMA   ]:", .{}),
            .DOT      => try writer.print("[DOT     ]:", .{}),
            .ERROR    => |val| try writer.print("[ERROR   ]: {s}", .{val}),
            .ILLEGAL  => |val| try writer.print("[ILLEGAL ]: {c}", .{val}),
            .LPAREN   => try writer.print("[LPAREN  ]:", .{}),
            .RPAREN   => try writer.print("[RPAREN  ]:", .{}),
            .LBRACK   => try writer.print("[LBRACK  ]:", .{}),
            .RBRACK   => try writer.print("[RBRACK  ]:", .{}),
            .LCURLY   => try writer.print("[LCURLY  ]:", .{}),
            .RCURLY   => try writer.print("[RCURLY  ]:", .{}),
            .PLUS     => try writer.print("[PLUS    ]:", .{}),
            .MINUS    => try writer.print("[MINUS   ]:", .{}),
            .DIV      => try writer.print("[DIV     ]:", .{}),
            .MUL      => try writer.print("[MUL     ]:", .{}),
            .AND      => try writer.print("[AND     ]:", .{}),
            .OR       => try writer.print("[OR      ]:", .{}),
            .NOT      => try writer.print("[NOT     ]:", .{}),
            .DEQ      => try writer.print("[DEQ     ]:", .{}),
            .EQ       => try writer.print("[EQ      ]:", .{}),
            .LT       => try writer.print("[LT      ]:", .{}),
            .GT       => try writer.print("[GT      ]:", .{}),
            .LTE      => try writer.print("[LTE     ]:", .{}),
            .GTE      => try writer.print("[GTE     ]:", .{}),
            .DECLARE  => try writer.print("[DECLARE ]:", .{}),
            .PRINT    => try writer.print("[PRINT   ]:", .{}),
            .IF       => try writer.print("[IF      ]:", .{}),
            .ELSE     => try writer.print("[ELSE    ]:", .{}),
            .WHILE    => try writer.print("[WHILE   ]:", .{}),
            .BREAK    => try writer.print("[BREAK   ]:", .{}),
            .CONTINUE => try writer.print("[CONTINUE]:", .{}),
            .RETURN   => try writer.print("[RETURN  ]:", .{}),
            .FUN      => try writer.print("[FUN     ]:", .{}),
            .INTLIT   => |val| try writer.print("[INTLIT  ]: {}", .{val}),
            .FLOATLIT => |val| try writer.print("[FLOATLIT]: {}", .{val}),
            .BOOLLIT  => |val| try writer.print("[BOOLLIT ]: {}", .{val}),
            .IDENT    => |val| try writer.print("[IDENT   ]: {s}", .{val})
        }
    }

    pub fn getInfixPrecedence(self: Token) ?InfixPrecedence {
        // TODO:
        // Implement the remaining tokens here..
        // TODO: FUN should be binding hard here...
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


            .PLUS, .MINUS => InfixPrecedence {.left = 13, .right = 14},
            .MUL, .DIV    => InfixPrecedence {.left = 15, .right = 16},

            .DOT => InfixPrecedence {.left = 23, .right = 24},

            // Some are not operators
            else => null
        };
    }

    pub fn getPrefixPrecedence(self: Token) ?u8 {
        return switch(self) {
            .MINUS, .NOT => 17,
            else => null
        };
    }

    pub fn getPostfixPrecedence(self: Token) ?u8 {
        return switch(self) {
            .LPAREN => 19,
            .LBRACK => 21,
            else => null
        };
    }
};

pub const InfixPrecedence: type = struct {
    left: u8,
    right: u8
};
