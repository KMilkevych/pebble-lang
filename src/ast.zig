const std = @import("std");
const token = @import("token.zig");

pub const Lit = union(enum) {
    Int: i64,
    Bool: bool,

    pub fn format(
        self: Lit,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch(self) {
            .Int => |v| try writer.print("{}", .{v}),
            .Bool => |v| try writer.print("{}", .{v}),
        }
    }
};

pub const Lval = union(enum) {
    Var: []const u8,

    // TODO: Add array index and field lookup


    pub fn format(
        self: Lval,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch(self) {
            .Var => |v| try writer.print("{s}", .{v})
        }
    }
};

pub const LitOrLval = union(enum) {
    Lit: Lit,
    Lval: Lval,

    pub fn format(
        self: LitOrLval,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        switch(self) {
            .Lit => |lit| lit.format(fmt, options, writer),
            .Lval => |lval| lval.format(fmt, options, writer)
        }
    }
};

pub const BinOp = enum {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Lt,
    Gt,
    Lte,
    Gte,

    pub fn format(
        self: BinOp,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch(self) {
            .Add => try writer.print("+", .{}),
            .Sub => try writer.print("-", .{}),
            .Mul => try writer.print("*", .{}),
            .Div => try writer.print("/", .{}),
            .And => try writer.print("&&", .{}),
            .Or  => try writer.print("||", .{}),
            .Eq  => try writer.print("==", .{}),
            .Lt  => try writer.print("<", .{}),
            .Gt  => try writer.print(">", .{}),
            .Lte => try writer.print("<=", .{}),
            .Gte => try writer.print(">=", .{}),
        }
    }

    pub fn from_token(tok: token.Token) ?BinOp {
        return switch (tok) {
            .PLUS => BinOp.Add,
            .MINUS => BinOp.Sub,
            .MUL => BinOp.Mul,
            .DIV => BinOp.Div,
            .AND => BinOp.And,
            .OR => BinOp.Or,
            .DEQ => BinOp.Eq,
            .GT => BinOp.Gt,
            .GTE => BinOp.Gte,
            .LT => BinOp.Lt,
            .LTE => BinOp.Lte,
            else => null
        };
    }
};

pub const UnOp = enum {
    Neg,
    Not,

    pub fn format(
        self: UnOp,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        switch(self) {
            .Neg => try writer.print("-", .{}),
            .Not => try writer.print("!", .{}),
        }
    }

    pub fn from_token(tok: token.Token) ?UnOp {
        return switch(tok) {
            .MINUS => UnOp.Neg,
            .NOT => UnOp.Not,
            else => null
        };
    }
};

pub const BinOpExpr = struct {
    lhs: *const Expr,
    rhs: *const Expr,
    op: BinOp,

    pub fn destroyAll(self: *const BinOpExpr, allocator: std.mem.Allocator) void {
        self.lhs.destroyAll(allocator);
        self.rhs.destroyAll(allocator);
    }

    pub fn format(
        self: BinOpExpr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;

        try writer.print(
            "({} {} {})",
            .{self.op, self.lhs.*, self.rhs.*}
        );
    }
};

pub const UnOpExpr = struct {
    rhs: *const Expr,
    op: UnOp,

    pub fn destroyAll(self: *const UnOpExpr, allocator: std.mem.Allocator) void {
        self.rhs.destroyAll(allocator);
    }

    pub fn format(
        self: UnOpExpr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print(
            "{}{}",
            .{self.op, self.rhs.*}
        );
    }
};

pub const AssignExpr = struct {
    lhs: *const Expr,
    rhs: *const Expr,

    pub fn destroyAll(self: *const AssignExpr, allocator: std.mem.Allocator) void {
        self.lhs.destroyAll(allocator);
        self.rhs.destroyAll(allocator);
    }

    pub fn format(
        self: AssignExpr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print(
            "({} = {})",
            .{self.lhs.*, self.rhs.*}
        );
    }
};

pub const Expr = union(enum) {
    BinOpExpr: BinOpExpr,
    UnOpExpr: UnOpExpr,
    Lit: Lit,
    Lval: Lval,
    AssignExpr: AssignExpr,

    pub fn destroyAll(self: *const Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .BinOpExpr => |*expr| expr.destroyAll(allocator),
            .UnOpExpr => |*expr| expr.destroyAll(allocator),
            .Lit => {},
            .Lval => {}, // TODO: Figure out if this needs implementation
            .AssignExpr => |*expr| expr.destroyAll(allocator),
        }
        allocator.destroy(self);
    }

    pub fn format(
        self: Expr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        return switch (self) {
            .BinOpExpr => |expr| expr.format(fmt, options, writer),
            .UnOpExpr => |expr| expr.format(fmt, options, writer),
            .Lit => |lit| lit.format(fmt, options, writer),
            .Lval => |lval| lval.format(fmt, options, writer),
            .AssignExpr => |expr| expr.format(fmt, options, writer),
        };
    }
};

pub const IfElseStmt = struct {
    cond: *const Expr,
    ifStmt: Stmt,
    elseStmt: ?Stmt,

    pub fn destroyAll(self: *const IfElseStmt, allocator: std.mem.Allocator) void {
        self.cond.destroyAll(allocator);
        self.ifStmt.destroyAll(allocator);
        if (self.elseStmt) |stmt| stmt.destroyAll(allocator);
        allocator.destroy(self);
    }

    pub fn format(
        self: IfElseStmt,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("IF {}\n{}", .{self.cond.*, self.ifStmt});
        if (self.elseStmt) |stmt| try writer.print("ELSE\n{}", .{stmt});
    }

};

pub const WhileStmt = struct {
    cond: *const Expr,
    body: Stmt,

    pub fn destroyAll(self: *const WhileStmt, allocator: std.mem.Allocator) void {
        self.cond.destroyAll(allocator);
        self.body.destroyAll(allocator);
        allocator.destroy(self);
    }

    pub fn format(
        self: WhileStmt,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = options;
        _ = fmt;
        try writer.print("WHILE {}\n{}", .{self.cond.*, self.body});
    }

};


pub const Stmt = union(enum) {
    DeclareStmt: *const Expr,
    ExprStmt: *const Expr,
    PrintStmt: *const Expr,
    BlockStmt: []const Stmt,
    IfElseStmt: *const IfElseStmt,
    WhileStmt: *const WhileStmt,

    pub fn destroyAll(self: *const Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .DeclareStmt => |expr| expr.destroyAll(allocator),
            .ExprStmt => |expr| expr.destroyAll(allocator),
            .PrintStmt => |expr| expr.destroyAll(allocator),
            .BlockStmt => |stmts| {
                for (stmts) |stmt| stmt.destroyAll(allocator);
                allocator.free(stmts);
            },
            .IfElseStmt => |stmt| stmt.destroyAll(allocator),
            .WhileStmt => |stmt| stmt.destroyAll(allocator),
        }
    }

    pub fn format(
        self: Stmt,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = options;
        _ = fmt;
        return switch (self) {
            .DeclareStmt => |expr| try writer.print("DECLARE {}\n", .{expr}),
            .ExprStmt => |expr| try writer.print("{}\n", .{expr}),
            .PrintStmt => |expr| try writer.print("PRINT {}\n", .{expr}),
            .BlockStmt => |stmts| {
                try writer.print("BEGIN BLOCK\n", .{});
                for (stmts) |stmt| try writer.print("{}\n", .{stmt});
                try writer.print("END BLOCK\n", .{});
            },
            .IfElseStmt => |stmt| try writer.print("{}\n", .{stmt}),
            .WhileStmt => |stmt| try writer.print("{}\n", .{stmt}),
        };
    }
};

pub const Proc = struct {

    stmts: []const Stmt,

    pub fn destroyAll(self: *const Proc, allocator: std.mem.Allocator) void {
        for (self.stmts) |stmt| stmt.destroyAll(allocator);
        allocator.free(self.stmts);
    }

    pub fn format(
        self: Proc,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = options;
        _ = fmt;
        for (self.stmts) |stmt| try writer.print("{}", .{stmt});
    }
};
