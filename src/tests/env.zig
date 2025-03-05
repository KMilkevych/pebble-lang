const venv = @import("../env.zig");
const ast = @import("../ast.zig");

const std = @import("std");

test "insert and lookup" {

    // Initialize environment
    var env: venv.Env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Place value
    env.insert("x", venv.ObjectVal {.Var = ast.Lit {.Int = 32}});

    // Lookup
    const v: ?venv.ObjectVal = env.lookup("x");
    try std.testing.expectEqualDeep(
        v,
        venv.ObjectVal { .Var = ast.Lit {.Int = 32}}
    );
}


test "insert and lookup 2" {

    // Initialize environment
    var env: venv.Env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    const identifier: []const u8 = "x";

    // Place value
    env.insert(identifier, venv.ObjectVal { .Var = ast.Lit {.Int = 32}});

    // Lookup
    const v: ?venv.ObjectVal = env.lookup("x");
    try std.testing.expectEqualDeep(
        v,
        venv.ObjectVal { .Var = ast.Lit {.Int = 32}}
    );
}

test "insert and lookup 3" {

    // Initialize environment
    var env: venv.Env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    const identifier: []const u8 = "x";

    // Place value
    env.insert(identifier, venv.ObjectVal { .Var = ast.Lit {.Int = 32}});

    // Lookup
    const v: ?venv.ObjectVal = env.lookup(identifier);
    try std.testing.expectEqualDeep(
        v,
        venv.ObjectVal { .Var = ast.Lit {.Int = 32}}
    );
}


test "insert and lookup 4" {

    // Initialize environment
    var env: venv.Env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    const identifier: []const u8 = "x";
    const identifier2: []const u8 = "x";

    // Place value
    env.insert(identifier, venv.ObjectVal { .Var = ast.Lit {.Int = 32}});

    // Lookup
    const v: ?venv.ObjectVal = env.lookup(identifier2);
    try std.testing.expectEqualDeep(
        v,
        venv.ObjectVal { .Var = ast.Lit {.Int = 32}}
    );
}
