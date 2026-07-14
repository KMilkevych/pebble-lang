const std = @import("std");
const token = @import("token.zig");
const venv = @import("env.zig");

pub const Callable = struct {
    params: []const Var,
    body: Stmt,
    closure: *venv.Env,

    pub fn destroyAll(self: *const Callable, allocator: std.mem.Allocator) void {

        _ = self;
        _ = allocator;

        // BUG: This *shoud* result in memory leaks
        // but so far testing haven't caught it..
        // This will be destroyed when the corresponding FunDeclStatement is destroyed

        // allocator.free(self.params);
        // self.body.destroyAll(allocator);
        // allocator.destroy(self);
    }

    pub fn format(
        self: Callable,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("(", .{});
        for (self.params) |param| try writer.print("{s},", .{param});
        try writer.print(") => {f}", .{self.body});
    }

};

pub const List = struct {

    len: usize,
    items: []Lit,

    refs: u32 = 1,

    pub fn makeReference(self: *List) *List {

        self.refs = self.refs + 1;
        return self;
    }

    pub fn destroyAll(self: *List, allocator: std.mem.Allocator) void {

        // Use reference-counting
        self.refs = self.refs - 1;
        if (self.refs <= 0) {
            for (self.items) |*item| item.destroyAll(allocator);
            allocator.free(self.items);
            allocator.destroy(self);
        }
    }

    pub fn format(
        self: List,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("<", .{});
        if (self.items.len > 0) {
            for (0..self.items.len-1) |i| try writer.print("{f}, ", .{self.items[i]});
            try writer.print("{f}", .{self.items[self.items.len - 1]});
        }
        try writer.print(">", .{});
    }

};

pub const Type = enum {
    Int,
    Float,
    Bool,

    pub fn format(
        self: Type,
        writer: *std.Io.Writer
    ) !void {
        switch(self) {
            .Int => try writer.print("Int", .{}),
            .Float => try writer.print("Float", .{}),
            .Bool => try writer.print("Bool", .{}),
        }
    }
};

pub const Lit = union(enum) {
    Int: i64,
    Float: f64,
    Bool: bool,
    Void: void,
    Callable: Callable,
    List: *List,
    Type: Type,

    pub fn destroyAll(self: *Lit, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Int, .Bool, .Float, .Type, .Void => {},
            .Callable => |fun| fun.destroyAll(allocator),
            .List => |lst| lst.destroyAll(allocator),
        }
    }

    pub fn format(
        self: Lit,
        writer: *std.Io.Writer
    ) !void {
        switch(self) {
            .Int => |v| try writer.print("{d}", .{v}),
            .Float => |v| try writer.print("{d}", .{v}),
            .Bool => |v| try writer.print("{}", .{v}),
            .Void => try writer.print("{{}}", .{}),
            .Callable => |f| try writer.print("{f}", .{f}),
            .List => |l| try writer.print("{f}", .{l}),
            .Type => |t| try writer.print("{f}", .{t}),
        }
    }
};

pub const Var: type = []const u8;

pub const ListIndex: type = struct {
    id: *const Expr,
    idx: *const Expr,

    pub fn destroyAll(self: *const ListIndex, allocator: std.mem.Allocator) void {
        self.id.destroyAll(allocator);
        self.idx.destroyAll(allocator);
    }

    pub fn format(
        self: ListIndex,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("{f}[{f}]", .{self.id, self.idx});
    }
};

pub const PropertyAccess: type = struct {
    lhs: *const Expr,
    prop: *const Expr,

    pub fn destroyAll(self: *const PropertyAccess, allocator: std.mem.Allocator) void {
        self.lhs.destroyAll(allocator);
        self.prop.destroyAll(allocator);
    }

    pub fn format(
        self: PropertyAccess,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("{f}.{f}", .{self.lhs, self.prop});
    }
};

pub const Lval = union(enum) {
    Var: Var,
    ListIndex: ListIndex,
    PropertyAccess: PropertyAccess,

    pub fn destroyAll(self: *const Lval, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .Var => {},
            .ListIndex => |l| l.destroyAll(allocator),
            .PropertyAccess => |p| p.destroyAll(allocator),
        }
    }

    pub fn format(
        self: Lval,
        writer: *std.Io.Writer
    ) !void {
        switch(self) {
            .Var => |v| try writer.print("{s}", .{v}),
            .ListIndex => |l| try writer.print("{f}", .{l}),
            .PropertyAccess => |p| try writer.print("{f}", .{p})
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
        writer: *std.Io.Writer
    ) !void {
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
        writer: *std.Io.Writer
    ) !void {
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
        writer: *std.Io.Writer
    ) !void {

        try writer.print(
            "({f} {f} {f})",
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
        writer: *std.Io.Writer
    ) !void {
        try writer.print(
            "{f}{f}",
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
        writer: *std.Io.Writer
    ) !void {
        try writer.print(
            "({f} = {f})",
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
        writer: *std.Io.Writer
    ) !void {
        try writer.print("{f}(", .{self.id});
        for (self.args) |arg| try writer.print("{f},", .{arg});
        try writer.print(")", .{});
    }
};

pub const AsExpr = struct {
    lhs: *const Expr,
    as: *const Expr,

    pub fn destroyAll(self: *const AsExpr, allocator: std.mem.Allocator) void {
        self.lhs.destroyAll(allocator);
        self.as.destroyAll(allocator);
    }

    pub fn format(
        self: AsExpr,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("{f} as {f}", .{self.lhs, self.as});
    }
};

pub const Expr = union(enum) {
    BinOpExpr: BinOpExpr,
    UnOpExpr: UnOpExpr,
    Lit: Lit,
    Lval: Lval,
    AssignExpr: AssignExpr,
    CallExpr: CallExpr,
    ListExpr: []const *const Expr,
    AsExpr: AsExpr,

    pub fn destroyAll(self: *const Expr, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .BinOpExpr => |*expr| expr.destroyAll(allocator),
            .UnOpExpr => |*expr| expr.destroyAll(allocator),
            .Lit => {},
            .Lval => |*lval| lval.destroyAll(allocator),
            .AssignExpr => |*expr| expr.destroyAll(allocator),
            .CallExpr => |*expr| expr.destroyAll(allocator),
            .ListExpr => |exprs| {
                for (exprs) |expr| expr.destroyAll(allocator);
                allocator.free(exprs);
            },
            .AsExpr => |*expr| expr.destroyAll(allocator),
        }
        allocator.destroy(self);
    }

    pub fn format(
        self: Expr,
        writer: *std.Io.Writer
    ) !void {
        return switch (self) {
            .BinOpExpr => |expr| expr.format(writer),
            .UnOpExpr => |expr| expr.format(writer),
            .Lit => |lit| lit.format( writer),
            .Lval => |lval| lval.format(writer),
            .AssignExpr => |expr| expr.format(writer),
            .CallExpr => |expr| expr.format(writer),
            .ListExpr => |exprs| {
                try writer.print("<", .{});
                for (exprs) |expr| try writer.print("{f},", .{expr});
                try writer.print(">", .{});
            },
            .AsExpr => |expr| expr.format(writer),
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
        writer: *std.Io.Writer
    ) !void {
        try writer.print("IF {f}\n{f}", .{self.cond.*, self.ifStmt});
        if (self.elseStmt) |stmt| try writer.print("ELSE\n{f}", .{stmt});
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
        writer: *std.Io.Writer
    ) !void {
        try writer.print("WHILE {f}\n{f}", .{self.cond.*, self.body});
    }

};

pub const FunDefStmt = struct {
    id: Var,
    params: []const Var,
    body: Stmt,

    pub fn destroyAll(self: *const FunDefStmt, allocator: std.mem.Allocator) void {
        allocator.free(self.params);
        self.body.destroyAll(allocator);
        allocator.destroy(self);
    }

    pub fn format(
        self: FunDefStmt,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("FUNCTION {s}(", .{self.id});
        for (self.params) |param| try writer.print("{s},", .{param});
        try writer.print(") {f}", .{self.body});
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
    FunDefStmt: *const FunDefStmt,

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
            .FunDefStmt => |stmt| stmt.destroyAll(allocator),
        }
    }

    pub fn format(
        self: Stmt,
        writer: *std.Io.Writer
    ) !void {
        return switch (self) {
            .DeclareStmt => |exprs| {
                try writer.print("DECLARE ", .{});
                for (exprs) |expr| try writer.print("{f}, ", .{expr});
                try writer.print("\n", .{});
            },
            .ExprStmt => |expr| try writer.print("{f}\n", .{expr}),
            .PrintStmt => |exprs| {
                try writer.print("PRINT ", .{});
                for (exprs) |expr| try writer.print("{f}, ", .{expr});
                try writer.print("\n", .{});
            },
            .BlockStmt => |stmts| {
                try writer.print("BEGIN BLOCK\n", .{});
                for (stmts) |stmt| try writer.print("{f}\n", .{stmt});
                try writer.print("END BLOCK\n", .{});
            },
            .IfElseStmt => |stmt| try writer.print("{f}\n", .{stmt}),
            .WhileStmt => |stmt| try writer.print("{f}\n", .{stmt}),
            .BreakStmt => try writer.print("BREAK\n", .{}),
            .ContinueStmt => try writer.print("CONTINUE\n", .{}),
            .ReturnStmt => |expr| try writer.print("RETURN {f}\n", .{expr}),
            .FunDefStmt => |stmt| try writer.print("{f}", .{stmt}),
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
        writer: *std.Io.Writer
    ) !void {
        for (self.stmts) |stmt| try writer.print("{f}", .{stmt});
    }
};
