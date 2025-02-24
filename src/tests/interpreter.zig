const ast = @import("../ast.zig");
const interpreter = @import("../interpreter.zig");
const venv = @import("../env.zig");
const std = @import("std");

const expect = std.testing.expect;


test "simple not" {
    const t: ast.Expr = ast.Expr {
        .UnOpExpr = ast.UnOpExpr {
            .rhs = &ast.Expr { .Lit = ast.Lit {.Bool = true} },
            .op = ast.UnOp.Not
        }
    };

    const f: ast.Expr = ast.Expr {
        .UnOpExpr = ast.UnOpExpr {
            .rhs = &ast.Expr { .Lit = ast.Lit {.Bool = false} },
            .op = ast.UnOp.Not
        }
    };

    var env = venv.Env.new(std.testing.allocator);

    try std.testing.expectEqual(
        ast.Lit {.Bool = false},
        try interpreter.evalExpr(&t, &env)
    );
    try std.testing.expectEqual(
        ast.Lit {.Bool = true},
        try interpreter.evalExpr(&f, &env)
    );
}


test "simple neg" {
    const l3: ast.Expr = ast.Expr {
        .UnOpExpr = ast.UnOpExpr {
            .rhs = &ast.Expr { .Lit = ast.Lit {.Int = 3} },
            .op = ast.UnOp.Neg
        }
    };

    const lm19: ast.Expr = ast.Expr {
        .UnOpExpr = ast.UnOpExpr {
            .rhs = &ast.Expr { .Lit = ast.Lit {.Int = -19} },
            .op = ast.UnOp.Neg
        }
    };

    const l0: ast.Expr = ast.Expr {
        .UnOpExpr = ast.UnOpExpr {
            .rhs = &ast.Expr { .Lit = ast.Lit {.Int = 0} },
            .op = ast.UnOp.Neg
        }
    };

    var env = venv.Env.new(std.testing.allocator);

    try std.testing.expectEqual(
        ast.Lit {.Int = -3},
        try interpreter.evalExpr(&l3, &env)
    );
    try std.testing.expectEqual(
        ast.Lit {.Int = 19},
        try interpreter.evalExpr(&lm19, &env)
    );
    try std.testing.expectEqual(
        ast.Lit {.Int = 0},
        try interpreter.evalExpr(&l0, &env)
    );
}

test "variable lookup" {

    // Prepare environment
    const identifier: []const u8 = "x";
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();
    env.insert(identifier, venv.ObjectVal {.Var = venv.Value {.Int = 32}});

    const exp: ast.Expr = ast.Expr {
        .Lval = ast.Lval {.Var = identifier}
    };

    try std.testing.expectEqual(
        ast.Lit {.Int = 32},
        try interpreter.evalExpr(&exp, &env)
    );
}

test "variable assignment" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    defer env.deinit();

    const exp: ast.Expr = ast.Expr {
        .AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}}
        }
    };

    try std.testing.expectEqual(
        ast.Lit {.Int = 13},
        try interpreter.evalExpr(&exp, &env)
    );

}

test "let x (= undefined)" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}};

    // Evaluate statement
    try interpreter.evalStmt(stmt, &env);

    // Assert that environment has been updated with x = 49
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = venv.Value {.Undefined = {}}},
        env.lookup("x")
    );
}

test "let x = 49" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 49}}
    }}};

    // Evaluate statement
    try interpreter.evalStmt(stmt, &env);

    // Assert that environment has been updated with x = 49
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = venv.Value {.Int = 49}},
        env.lookup("x")
    );
}

test "invalid chain declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 19}}
        }}
    }}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!void = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.UndefinedVariable, err);
}


test "redeclaration error" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
    }}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!void = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.IdentifierAlreadyDeclared, err);
}

test "redeclaration error 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!void = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.IdentifierAlreadyDeclared, err);
}

test "smart scoped assignment" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}},

        ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Eq,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}},
            }},
            .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 20}},
                }}},
                ast.Stmt {.PrintStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}}
            }},
            .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &ast.Expr {.Lit = ast.Lit {.Int = 1}}}
            }}
        }},

        ast.Stmt {.PrintStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}}
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = venv.Value {.Int = 20}},
        env.lookup("x")
    );
}


test "smart scoped declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}},

        ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Eq,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}},
            }},
            .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.DeclareStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 20}},
                }}},
                ast.Stmt {.PrintStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}}
            }},
            .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.PrintStmt = &ast.Expr {.Lit = ast.Lit {.Int = 1}}}
            }}
        }},

        ast.Stmt {.PrintStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}}
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = venv.Value {.Int = 10}},
        env.lookup("x")
    );
}


test "comparison expressions" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Test less-than expressions
    const exp1: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp1, &env), ast.Lit {.Bool = true});

    const exp2: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp2, &env), ast.Lit {.Bool = false});

    const exp3: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp3, &env), ast.Lit {.Bool = false});

    // Test greater-than expressions
    const exp4: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp4, &env), ast.Lit {.Bool = false});

    const exp5: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp5, &env), ast.Lit {.Bool = false});

    const exp6: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gt,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp6, &env), ast.Lit {.Bool = true});

    // Test less-than or equal
    const exp7: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp7, &env), ast.Lit {.Bool = true});

    const exp8: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp8, &env), ast.Lit {.Bool = true});

    const exp9: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Lte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp9, &env), ast.Lit {.Bool = false});

    // Test greater-than or equal
    const exp10: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp10, &env), ast.Lit {.Bool = false});

    const exp11: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp11, &env), ast.Lit {.Bool = true});

    const exp12: ast.Expr = ast.Expr {.BinOpExpr = ast.BinOpExpr {
        .lhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}},
        .op = .Gte,
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
    }};
    try std.testing.expectEqual(try interpreter.evalExpr(&exp12, &env), ast.Lit {.Bool = true});
}
