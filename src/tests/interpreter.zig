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
    env.insert(identifier, venv.ObjectVal { .Var = ast.Lit {.Int = 32}});

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
    env.insert("x", venv.ObjectVal { .Undefined = {}});
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
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.Lval = ast.Lval {.Var = "x"}}
    }};

    // Evaluate statement
    _ = try interpreter.evalStmt(stmt, &env);

    // Assert that environment has been updated with x = 49
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Undefined = {}},
        env.lookup("x")
    );
}

test "let x = 49" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 49}}
        }}
    }};

    // Evaluate statement
    _ = try interpreter.evalStmt(stmt, &env);

    // Assert that environment has been updated with x = 49
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 49}},
        env.lookup("x")
    );
}

test "invalid chain declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
        &ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 19}}
            }}
        }
    }}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!interpreter.StmtReturn = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.UndefinedVariable, err);
}


test "redeclaration error" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal { .Undefined = {}});
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        }
    }}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!interpreter.StmtReturn = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.IdentifierAlreadyDeclared, err);
}

test "redeclaration error 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    env.insert("x", venv.ObjectVal { .Undefined = {}});
    defer env.deinit();

    // Prepare statement
    const stmt: ast.Stmt = ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.Lval = ast.Lval {.Var = "x"}}}};

    // Evaluate statement to error for z
    const err: interpreter.EvalError!interpreter.StmtReturn = interpreter.evalStmt(stmt, &env);
    try std.testing.expectEqual(interpreter.EvalError.IdentifierAlreadyDeclared, err);
}

test "smart scoped assignment" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

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
            }},
            .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            }}
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 20}},
        env.lookup("x")
    );
}


test "smart scoped declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Eq,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}},
            }},
            .ifStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 20}},
                }}}},
            }},
            .elseStmt = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            }}
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 10}},
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


test "break works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
                ast.Stmt {.BreakStmt = {}},
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
            }},
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 9}},
        env.lookup("x")
    );
}

test "break in block works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
                ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                    ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {.BreakStmt = {}},
                    }},
                }},
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
            }},
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 9}},
        env.lookup("x")
    );
}

test "continue works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},
        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
                ast.Stmt {.ContinueStmt = {}},
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                        .op = .Add,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
            }},
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 0}},
        env.lookup("y")
    );
}

test "continue in block works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
        }}}},
        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {.AssignExpr = ast.AssignExpr {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}}
        }}}},

        ast.Stmt {.WhileStmt = &ast.WhileStmt {
            .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = ast.BinOp.Gt,
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
            }},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Sub,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
                ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                    ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {.ContinueStmt = {}},
                    }}
                }},
                ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                        .op = .Add,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                }}},
            }},
        }},
    }};

    // Evaluate statement to error for z
    try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal { .Var = ast.Lit {.Int = 0}},
        env.lookup("y")
    );
}


test "break outside error" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.BreakStmt = {}}
        }}
    }};

    // Evaluate statement to error for z
    const res: interpreter.EvalError!void = interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        res,
        interpreter.EvalError.UnexpectedBreak
    );
}

test "break outside error 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {
        ast.Stmt {.BreakStmt = {}}
    }};

    // Evaluate statement to error for z
    const res: interpreter.EvalError!void = interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        res,
        interpreter.EvalError.UnexpectedBreak
    );
}

test "continue outside error" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.BlockStmt = &[_]ast.Stmt {
            ast.Stmt {.ContinueStmt = {}}
        }}
    }};

    // Evaluate statement to error for z
    const res: interpreter.EvalError!void = interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        res,
        interpreter.EvalError.UnexpectedContinue
    );
}


test "continue outside error 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {
        ast.Stmt {.ContinueStmt = {}}
    }};

    // Evaluate statement to error for z
    const res: interpreter.EvalError!void = interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        res,
        interpreter.EvalError.UnexpectedContinue
    );
}

test "comma declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
            }},
            &ast.Expr {.Lval = ast.Lval {.Var = "z"}},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "w"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
        }},
    }};

    // Evaluate procedure
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(venv.ObjectVal { .Var = ast.Lit {.Int = 1}}, env.lookup("x"));
    try std.testing.expectEqualDeep(venv.ObjectVal { .Var = ast.Lit {.Int = 2}}, env.lookup("y"));
    try std.testing.expectEqualDeep(venv.ObjectVal { .Undefined = {}}, env.lookup("z"));
    try std.testing.expectEqualDeep(venv.ObjectVal { .Var = ast.Lit {.Int = 1}}, env.lookup("w"));
}

test "undefined variable in comma declaration" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
            }},
            &ast.Expr {.Lval = ast.Lval {.Var = "z"}},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "w"}},
                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "z"}}
            }},
        }},
    }};

    // Evaluate statement to error for z
    const res: interpreter.EvalError!void = interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        res,
        interpreter.EvalError.UndefinedVariable
    );
}

test "simple funcall" {
    // Prepare environment NOTE: we only deinit the table not the environment here..
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("always1", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "always1"}},
        .args = &[_]*const ast.Expr {},
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function returns 1
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 1});
}

test "simple funcall one param" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("double", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"arg1"},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "arg1"}},
                    .op = .Mul,
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 2}}
                }},
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "double"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 7}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function doubles argument
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 14});
}

test "simple funcall three params" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("sum", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x", "y", "z"},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .op = .Add,
                    .rhs = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                        .op = .Add,
                        .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "z"}}
                    }}
                }},
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "sum"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 7}},
            &ast.Expr {.Lit = ast.Lit {.Int = 13}},
            &ast.Expr {.Lit = ast.Lit {.Int = -5}},
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 15});
}

test "simple funcall no return" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("voidfun", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {},
            .body = ast.Stmt {
                .BlockStmt = &[_] ast.Stmt {},
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "voidfun"}},
        .args = &[_]*const ast.Expr {},
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function returns void
    try std.testing.expectEqualDeep(r, ast.Lit {.Void = {}});
}


test "function closure works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert some variable x
    env.insert("x", venv.ObjectVal {.Var = ast.Lit {.Int = 19}});

    // Insert function into environment
    env.insert("mulbyx", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"other"},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "other"}},
                    .op = .Mul,
                    .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}
                }}
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "mulbyx"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 3}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 57});
}


test "function closure works 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert some variable x
    env.insert("x", venv.ObjectVal {.Var = ast.Lit {.Int = 19}});

    // Insert function into environment
    env.insert("mulbyx", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"other"},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "other"}},
                    .op = .Mul,
                    .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}
                }}
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "mulbyx"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.Var = "x"}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 361});
}


test "function argument shadow closure scope" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert some variable x
    env.insert("x", venv.ObjectVal {.Var = ast.Lit {.Int = 19}});

    // Insert function into environment
    env.insert("mul", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x", "x2"},
            .body = ast.Stmt {
                .ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .op = .Mul,
                    .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "x2"}}
                }}
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "mul"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 2}},
            &ast.Expr {.Lval = ast.Lval {.Var = "x"}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 38});
}


test "re-declaring outer variable in function local scope" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert some variable x
    env.insert("x", venv.ObjectVal {.Var = ast.Lit {.Int = 19}});

    // Insert function into environment
    env.insert("return3", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {},
            .body = ast.Stmt {
                .BlockStmt = &[_]ast.Stmt {
                    ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                        &ast.Expr {.AssignExpr = ast.AssignExpr {
                            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
                        }}
                    }},
                    ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "x"}}}
                }
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "return3"}},
        .args = &[_]*const ast.Expr {},
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 3});
}


test "wrong argument count 1" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("dummyfun", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {},
            .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Void = {}}}},
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "dummyfun"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        },
    }};

    // Evaluate statement
    const r: interpreter.EvalError!ast.Lit = interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(interpreter.EvalError.WrongArgCount, r);
}

test "wrong argument count 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("dummyfun", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var { "x", "y", "z" },
            .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Void = {}}}},
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "dummyfun"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 1}},
            &ast.Expr {.Lit = ast.Lit {.Int = 2}}
        },
    }};

    // Evaluate statement
    const r: interpreter.EvalError!ast.Lit = interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(interpreter.EvalError.WrongArgCount, r);
}

test "recursion 1" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("fac", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x"},
            .body = ast.Stmt {
                .BlockStmt = &[_]ast.Stmt {
                    ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
                        .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                            .op = .Eq,
                            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                        }},
                        .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Int = 1}}},
                        .elseStmt = null,
                    }},
                    ast.Stmt {.ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Mul,
                        .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                            .id = &ast.Expr {.Lval = ast.Lval {.Var = "fac"}},
                            .args = &[_]*const ast.Expr {
                                &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                                    .op = .Sub,
                                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                                }}
                            }
                        }}
                    }}}
                }
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "fac"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 1}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 1});
}


test "recursion 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("fac", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x"},
            .body = ast.Stmt {
                .BlockStmt = &[_]ast.Stmt {
                    ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
                        .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                            .op = .Eq,
                            .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                        }},
                        .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Int = 1}}},
                        .elseStmt = null,
                    }},
                    ast.Stmt {.ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Mul,
                        .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                            .id = &ast.Expr {.Lval = ast.Lval {.Var = "fac"}},
                            .args = &[_]*const ast.Expr {
                                &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                                    .op = .Sub,
                                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                                }}
                            }
                        }}
                    }}}
                }
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "fac"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 4}}
        },
    }};

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(ast.Lit {.Int = 24}, r);
}

test "mutual recursion" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert functions into environment
    env.insert("even", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x"},
            .body = ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
                .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .op = .Eq,
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
                }},
                .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Bool = true}}},
                .elseStmt = ast.Stmt { .IfElseStmt = &ast.IfElseStmt {
                    .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Gt,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 0}},
                    }},
                    .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.CallExpr = ast.CallExpr {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "odd"}},
                        .args = &[_]*const ast.Expr {
                            &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                                .op = .Sub,
                                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                            }}
                        }
                    }}},
                    .elseStmt = ast.Stmt {.ReturnStmt = &ast.Expr { .Lit = ast.Lit {.Bool = false}}}
                }},
            }},
            .closure = &env,
        }
    }});

    env.insert("odd", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x"},
            .body = ast.Stmt {.IfElseStmt = &ast.IfElseStmt {
                .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                    .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                    .op = .Eq,
                    .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                }},
                .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Bool = true}}},
                .elseStmt = ast.Stmt { .IfElseStmt = &ast.IfElseStmt {
                    .cond = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                        .op = .Gt,
                        .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}},
                    }},
                    .ifStmt = ast.Stmt {.ReturnStmt = &ast.Expr {.CallExpr = ast.CallExpr {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "even"}},
                        .args = &[_]*const ast.Expr {
                            &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                                .op = .Sub,
                                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                            }}
                        }
                    }}},
                    .elseStmt = ast.Stmt {.ReturnStmt = &ast.Expr { .Lit = ast.Lit {.Bool = false}}}
                }},
            }},
            .closure = &env,
        }
    }});

    // Prepare expressions
    const expr1: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "even"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 4}}
        },
    }};

    const expr2: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "even"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 5}}
        },
    }};

    const expr3: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "odd"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 5}}
        },
    }};

    const expr4: ast.Expr = ast.Expr {.CallExpr = ast.CallExpr {
        .id = &ast.Expr {.Lval = ast.Lval {.Var = "odd"}},
        .args = &[_]*const ast.Expr {
            &ast.Expr {.Lit = ast.Lit {.Int = 4}}
        },
    }};

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(ast.Lit {.Bool = true}, try interpreter.evalExpr(&expr1, &env));
    try std.testing.expectEqualDeep(ast.Lit {.Bool = false}, try interpreter.evalExpr(&expr2, &env));
    try std.testing.expectEqualDeep(ast.Lit {.Bool = true}, try interpreter.evalExpr(&expr3, &env));
    try std.testing.expectEqualDeep(ast.Lit {.Bool = false}, try interpreter.evalExpr(&expr4, &env));
}


test "function definition" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Insert function into environment
    env.insert("dummyfun", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {},
            .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lit = ast.Lit {.Void = {}}}},
            .closure = &env,
        }
    }});

    // Prepare expression
    const stmt: ast.Stmt = ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
        .id = "last",
        .params = &[_]ast.Var {"x", "y", "z"},
        .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "z"}}}
    }};

    // Evaluate statement
    _ = try interpreter.evalStmt(stmt, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {
            .Callable = ast.Callable {
                .params = &[_] ast.Var {"x", "y", "z"},
                .body = ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "z"}}},
                .closure = &env,
            }
        }},
        env.lookup("last")
    );
}

test "closure test" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.table.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 7}},
            }}
        }},

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {"n"},
            .body = ast.Stmt {.ReturnStmt = &ast.Expr {.BinOpExpr = ast.BinOpExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .op = .Mul,
                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "n"}}
            }}}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "x"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 13}},
            }}
        },

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "y"}},
                .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                    .args = &[_]*const ast.Expr {
                        &ast.Expr {.Lit = ast.Lit {.Int = 2}}
                    }
                }},
            }}
        }},
    }};

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.Int = 26}},
        env.lookup("y")
    );
}

test "make statement test" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 4}}
            }}},
        }},

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 4);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .len = 4,
        .items = items
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("lst")
    );
}

test "multi make statement test" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }}},
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "otherlist"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 7}}
            }}},
        }},

    }};

    var items = try std.testing.allocator.alloc(ast.Lit, 1);
    defer std.testing.allocator.free(items);
    items[0] = ast.Lit {.Void = {}};

    const items2 = try std.testing.allocator.alloc(ast.Lit, 7);
    defer std.testing.allocator.free(items2);
    for (items2) |*item| item.* = ast.Lit {.Void = {}};

    const ptr1 = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr1);
    ptr1.* = ast.List {
        .len = 1,
        .items = items
    };

    const ptr2 = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr2);
    ptr2.* = ast.List {
        .len = 7,
        .items = items2
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr1}},
        env.lookup("list")
    );

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr2}},
        env.lookup("otherlist")
    );
}

test "list mutation" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {
                    .ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                    }
                }},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 3);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};
    items[1] = ast.Lit {.Bool = true};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 3
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("lst")
    );
}

test "list referencing" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "a"}},
                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}
            }}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {
                    .ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                    }
                }},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 3);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};
    items[1] = ast.Lit {.Bool = true};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 3,
        .refs = 2
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("a")
    );
}

test "list referencing 2" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "a"}},
                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}
            }}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {
                    .ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "a"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                    }
                }},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 3);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};
    items[1] = ast.Lit {.Bool = true};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 3,
        .refs = 2
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("lst")
    );
}

test "list overwrite" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }

    }};

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.Bool = true}},
        env.lookup("lst")
    );
}


test "list function mutation" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {"lst"},
            .body = ast.Stmt {.ReturnStmt = &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                    .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                }}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Bool = true}}
            }}
        }}},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "mylist"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
            }}}
        }},

        ast.Stmt {.ExprStmt = &ast.Expr {
            .CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                .args = &[_]*const ast.Expr {
                    &ast.Expr {.Lval = ast.Lval {.Var = "mylist"}}
                }
            }
        }}
    }};

    // Prepare expected list
    const items = try std.testing.allocator.alloc(ast.Lit, 10);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};
    items[1] = ast.Lit {.Bool = true};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 10,
        .refs = 1
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("mylist")
    );
}


test "list function return" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                    &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                    }}}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}}
            }}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "mylist"}},
                .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                    .args = &[_]*const ast.Expr {}
                }}
            }}
        }},

    }};

    // Prepare expected list
    const items = try std.testing.allocator.alloc(ast.Lit, 10);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 10,
        .refs = 1
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("mylist")
    );
}


test "nested list function return" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
                    .id = "g",
                    .params = &[_]ast.Var {},
                    .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                            }}}
                        }},
                        ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}}
                    }}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr{.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "g"}},
                    .args = &[_]*const ast.Expr {}
                }}}
            }}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "mylist"}},
                .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                    .args = &[_]*const ast.Expr {}
                }}
            }}
        }},

    }};

    // Prepare expected list
    const items = try std.testing.allocator.alloc(ast.Lit, 10);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 10,
        .refs = 1
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("mylist")
    );
}

test "multi list declaration with initialization" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                    .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                }}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = -1}}
            }},
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
                    .idx = &ast.Expr {.Lit = ast.Lit {.Int = 5}}
                }}},
                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}
            }},
        }}

    }};

    // Prepare expected list
    const items1 = try std.testing.allocator.alloc(ast.Lit, 1);
    defer std.testing.allocator.free(items1);
    for (items1) |*item| item.* = ast.Lit {.Int = -1};

    const ptr1 = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr1);
    ptr1.* = ast.List {
        .items = items1,
        .len = 1,
        .refs = 6
    };

    const items2 = try std.testing.allocator.alloc(ast.Lit, 5);
    defer std.testing.allocator.free(items2);
    for (items2) |*item| item.* = ast.Lit {.List = ptr1};

    const ptr2 = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr2);
    ptr2.* = ast.List {
        .items = items2,
        .len = 5,
        .refs = 1
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr1}},
        env.lookup("lst")
    );
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr2}},
        env.lookup("list")
    );
}

test "list size" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Insert function into environment
    const items_ptr = try std.testing.allocator.alloc(ast.Lit, 10);
    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items_ptr,
        .len = 10
    };

    env.insert("list", venv.ObjectVal {.Var = ast.Lit {
        .List = ptr
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr { .Lval = ast.Lval {
        .PropertyAccess = ast.PropertyAccess {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
            .prop = &ast.Expr {.Lval = ast.Lval {.Var = "size"}}
        }}
    };

    // Evaluate statement
    const r: ast.Lit = try interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, ast.Lit {.Int = 10});
}

test "list invalid property" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Insert function into environment
    const items_ptr = try std.testing.allocator.alloc(ast.Lit, 10);
    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items_ptr,
        .len = 10
    };

    env.insert("list", venv.ObjectVal {.Var = ast.Lit {
        .List = ptr
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr { .Lval = ast.Lval {
        .PropertyAccess = ast.PropertyAccess {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
            .prop = &ast.Expr {.Lval = ast.Lval {.Var = "sz"}}
        }}
    };

    // Evaluate statement
    const r: interpreter.EvalError!ast.Lit = interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, interpreter.EvalError.InvalidProperty);
}

test "property not on list" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Insert function into environment
    env.insert("var", venv.ObjectVal {.Var = ast.Lit {
        .Int = 1
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr { .Lval = ast.Lval {
        .PropertyAccess = ast.PropertyAccess {
            .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "var"}},
            .prop = &ast.Expr {.Lval = ast.Lval {.Var = "size"}}
        }}
    };

    // Evaluate statement
    const r: interpreter.EvalError!ast.Lit = interpreter.evalExpr(&expr, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(r, interpreter.EvalError.InvalidProperty);
}

test "list overwrite with list" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "list"}},
                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
            }}}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                    .idx = &ast.Expr {.Lit = ast.Lit {.Int = 1}}
                }}},
                .rhs = &ast.Expr {.Lit = ast.Lit {.Int = 3}}
            }}}
        },

        ast.Stmt {.ExprStmt = &ast.Expr {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {
                    .Var = "list"
                }},
                .rhs = &ast.Expr {.Lval = ast.Lval {
                    .Var = "lst"
                }}
            }}
        }

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 1);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Int = 3};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 1,
        .refs = 2
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("lst")
    );
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("list")
    );
}

test "list function return unused" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                    &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                    }}}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}}
            }}
        }},

        ast.Stmt {.ExprStmt =
            &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                .args = &[_]*const ast.Expr {}
            }}
        },

    }};


    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
}

test "nested list function return unused" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
                    .id = "g",
                    .params = &[_]ast.Var {},
                    .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                            }}}
                        }},
                        ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}}
                    }}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr{.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "g"}},
                    .args = &[_]*const ast.Expr {}
                }}}
            }}
        }},

        ast.Stmt {.ExprStmt =
            &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                .args = &[_]*const ast.Expr {}
            }}
        },

    }};

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
}

test "nested list function return assignexpr" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
                    .id = "g",
                    .params = &[_]ast.Var {},
                    .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                            &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                                .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                            }}},

                            &ast.Expr {.Lval = ast.Lval {.Var = "var"}}
                        }},
                        ast.Stmt {.ReturnStmt = &ast.Expr {
                            .AssignExpr = ast.AssignExpr {
                                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "var"}},
                                .rhs = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}
                            }
                        }}
                    }}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr{.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "g"}},
                    .args = &[_]*const ast.Expr {}
                }}}
            }}
        }},

        ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.Lval = ast.Lval {.Var = "mylist"}},
                .rhs = &ast.Expr {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                    .args = &[_]*const ast.Expr {}
                }}
            }}
        }},

        ast.Stmt {.ExprStmt =
            &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                .args = &[_]*const ast.Expr {}
            }}
        },

    }};

    // Prepare expected list
    const items = try std.testing.allocator.alloc(ast.Lit, 10);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Void = {}};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 10,
        .refs = 1
    };

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("mylist")
    );
}

test "list function return print statement" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {.FunDefStmt = &ast.FunDefStmt {
            .id = "f",
            .params = &[_]ast.Var {},
            .body = ast.Stmt {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.DeclareStmt = &[_]*const ast.Expr {
                    &ast.Expr {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}},
                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = 10}}
                    }}}
                }},
                ast.Stmt {.ReturnStmt = &ast.Expr {.Lval = ast.Lval {.Var = "lst"}}}
            }}
        }},

        ast.Stmt {.PrintStmt = &[_]*const ast.Expr {
            &ast.Expr {.CallExpr = ast.CallExpr {
                .id = &ast.Expr {.Lval = ast.Lval {.Var = "f"}},
                .args = &[_]*const ast.Expr {}
            }}
        }},

    }};

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
}
