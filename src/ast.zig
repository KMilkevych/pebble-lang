const std = @import("std");
const token = @import("token.zig");
const venv = @import("env.zig");

pub const Callable = struct {
    params: []const Var,
    body: Stmt,
    closure: *venv.Env,

    pub fn destroyAll(self: *const Callable, allocator: std.mem.Allocator) void {
        allocator.free(self.params);
        self.body.destroyAll(allocator);
    }

    pub fn format(
        self: Callable,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("(", .{});
        for (self.params) |param| try writer.print("{s},", .{param});
        try writer.print(") => {}\n", .{self.body});
    }

};

pub const Lit = union(enum) {
    Int: i64,
    Bool: bool,
    Void: void,
    Callable: Callable,

    pub fn destroyAll(self: *const Lit, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Int, .Bool, .Void => {},
            .Callable => |fun| fun.destroyAll(allocator)
        }
    }

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
            .Void => try writer.print("{{}}", .{}),
            .Callable => |f| try writer.print("{}", .{f}),
        }
    }
};

pub const Var: type = []const u8;

pub const Lval = union(enum) {
    Var: Var,

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

pub const CallExpr = struct {

    id: *const Expr,
    args: []const *const Expr,

    pub fn destroyAll(self: *const CallExpr, allocator: std.mem.Allocator) void {
        self.id.destroyAll(allocator);
        for (self.args) |arg| arg.destroyAll(allocator);
        allocator.free(self.args);
    }

    pub fn format(
        self: CallExpr,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}(", .{self.id});
        for (self.args) |arg| try writer.print("{},", .{arg});
        try writer.print(")", .{});
    }
};

pub const Expr = union(enum) {
    BinOpExpr: BinOpExpr,
    UnOpExpr: UnOpExpr,
    Lit: Lit,
    Lval: Lval,
    AssignExpr: AssignExpr,
    CallExpr: CallExpr,

    pub fn destroyAll(self: *const Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .BinOpExpr => |*expr| expr.destroyAll(allocator),
            .UnOpExpr => |*expr| expr.destroyAll(allocator),
            .Lit => {},
            .Lval => {}, // TODO: Figure out if this needs implementation
            .AssignExpr => |*expr| expr.destroyAll(allocator),
            .CallExpr => |*expr| expr.destroyAll(allocator),
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
            .CallExpr => |expr| expr.format(fmt, options, writer),
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
    DeclareStmt: []const *const Expr,
    ExprStmt: *const Expr,
    PrintStmt: []const *const Expr,
    BlockStmt: []const Stmt,
    IfElseStmt: *const IfElseStmt,
    WhileStmt: *const WhileStmt,
    BreakStmt: void,
    ContinueStmt: void,
    ReturnStmt: *const Expr,

    pub fn destroyAll(self: *const Stmt, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .DeclareStmt => |exprs| {
                for (exprs) |expr| expr.destroyAll(allocator);
                allocator.free(exprs);
            },
            .ExprStmt => |expr| expr.destroyAll(allocator),
            .PrintStmt => |exprs| {
                for (exprs) |expr| expr.destroyAll(allocator);
                allocator.free(exprs);
            },
            .BlockStmt => |stmts| {
                for (stmts) |stmt| stmt.destroyAll(allocator);
                allocator.free(stmts);
            },
            .IfElseStmt => |stmt| stmt.destroyAll(allocator),
            .WhileStmt => |stmt| stmt.destroyAll(allocator),
            .BreakStmt => {},
            .ContinueStmt => {},
            .ReturnStmt => |expr| expr.destroyAll(allocator),
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
            .DeclareStmt => |exprs| {
                try writer.print("DECLARE ", .{});
                for (exprs) |expr| try writer.print("{}, ", .{expr});
                try writer.print("\n", .{});
            },
            .ExprStmt => |expr| try writer.print("{}\n", .{expr}),
            .PrintStmt => |exprs| {
                try writer.print("PRINT ", .{});
                for (exprs) |expr| try writer.print("{}, ", .{expr});
                try writer.print("\n", .{});
            },
            .BlockStmt => |stmts| {
                try writer.print("BEGIN BLOCK\n", .{});
                for (stmts) |stmt| try writer.print("{}\n", .{stmt});
                try writer.print("END BLOCK\n", .{});
            },
            .IfElseStmt => |stmt| try writer.print("{}\n", .{stmt}),
            .WhileStmt => |stmt| try writer.print("{}\n", .{stmt}),
            .BreakStmt => try writer.print("BREAK\n", .{}),
            .ContinueStmt => try writer.print("CONTINUE\n", .{}),
            .ReturnStmt => |expr| try writer.print("RETURN {}\n", .{expr}),
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
