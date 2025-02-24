const std = @import("std");
const ast = @import("ast.zig");

pub const Value: type = union(enum) {
    Int: i64,
    Bool: bool,
    Undefined: void,

    pub fn fromLiteral(literal: ast.Lit) Value {
        return switch (literal) {
            .Int => |int| Value {.Int = int},
            .Bool => |bl| Value {.Bool = bl},
        };
    }
};

// pub const FunValue: type = struct {
//     // TODO:
//     // This should contain all the function information
//     // such as the arguments it takes, the closure of values it has
//     // as well as the actual function contents (sequenceo of statements)
// };

pub const ObjectVal: type = union(enum) {
    Var: Value,
    // Fun: FunValue
};

pub const Env: type = struct {

    const Self: type = @This();

    outer: ?*Self,
    allocator: std.mem.Allocator,
    table: std.StringHashMap(ObjectVal),

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .table = std.StringHashMap(ObjectVal).init(allocator),
            .outer = null
        };
    }

    pub fn newScoped(self: *Self) Self {
        return .{
            .allocator = self.allocator,
            .table = std.StringHashMap(ObjectVal).init(self.allocator),
            .outer = self
        };
    }

    pub fn deinit(self: *Self) void {
        self.table.deinit();
    }

    pub fn insert(self: *Self, key: []const u8, value: ObjectVal) void {
        self.table.put(key, value) catch unreachable;
    }

    pub fn insertScoping(self: *Self, key: []const u8, value: ObjectVal) void {

        // Insert locally if defined locally, otherwise insert into parent
        if (self.isDeclaredLocal(key)) self.insert(key, value)
        else if (self.outer) |ou| {
            ou.insertScoping(key, value);
        }
    }

    pub fn remove(self: *Self, key: []const u8) void {
        self.table.remove(key);
    }

    pub fn lookup(self: *Self, key: []const u8) ?ObjectVal {
        // Lookup using outer scope if given
        return if (self.table.get(key)) |val| val
        else if (self.outer) |ou| ou.lookup(key)
        else null;
    }

    pub fn isDeclaredLocal(self: *Self, key: []const u8) bool {
        return self.table.get(key) != null;
    }

    pub fn isDeclaredGlobal(self: *Self, key: []const u8) bool {
        return
            if (self.isDeclaredLocal(key)) true
            else if (self.outer) |ou| ou.isDeclaredGlobal(key)
            else false;
    }

};
