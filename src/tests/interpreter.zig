const ast = @import("../ast.zig");
const interpreter = @import("../interpreter.zig");
const venv = @import("../env.zig");
const loc = @import("../location.zig");
const nolocation = loc.LocationRange.none;

const std = @import("std");

const expect = std.testing.expect;


test "simple not" {
    const t: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {
            .UnOpExpr = ast.UnOpExpr {
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner { .Lit = ast.Lit {.Bool = true} },
                    .location = nolocation(),
                },
                .op = ast.UnOp.Not
            }
        },
        .location = nolocation(),
    };

    const f: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {
            .UnOpExpr = ast.UnOpExpr {
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner { .Lit = ast.Lit {.Bool = false} },
                    .location = nolocation()
                },
                .op = ast.UnOp.Not
            }
        },
        .location = nolocation()
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
        .expr = ast.ExprInner {
            .UnOpExpr = ast.UnOpExpr {
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},
                    .location = nolocation()
                },
                .op = ast.UnOp.Neg
            }
        },
        .location = nolocation()
    };

    const lm19: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {
            .UnOpExpr = ast.UnOpExpr {
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Int = -19}},
                    .location = nolocation()
                },
                .op = ast.UnOp.Neg
            }
        },
        .location = nolocation()
    };

    const l0: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {
            .UnOpExpr = ast.UnOpExpr {
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},
                    .location = nolocation()
                },
                .op = ast.UnOp.Neg
            }
        },
        .location = nolocation()
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
        .expr = ast.ExprInner {
            .Lval = ast.Lval {.Var = identifier}
        },
        .location = nolocation()
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
        .expr = ast.ExprInner {
            .AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},
                    .location = nolocation(),
                },
                .rhs = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 13}},
                    .location = nolocation(),
                }
            }
        },
        .location = nolocation()
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
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {
            .DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {
                        .Lval = ast.Lval {.Var = "x"}
                    },
                    .location = nolocation()
                }
            }
        },
        .location = nolocation()
    };

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
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {
            .DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {
                    .AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {
                            .expr = ast.ExprInner {
                                .Lval = ast.Lval {.Var = "x"}
                            },
                            .location = nolocation()
                        },
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {
                                .Lit = ast.Lit {.Int = 49}
                            },
                            .location = nolocation()
                        }
                    }
                },
                .location = nolocation()}
            }
        },
        .location = nolocation()
    };

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
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
            &ast.Expr {.expr = ast.ExprInner {
                .AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},
                        .location = nolocation()
                    },
                    .rhs = &ast.Expr {
                        .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                            .lhs = &ast.Expr {
                                .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},
                                .location = nolocation()
                            },
                            .rhs = &ast.Expr {
                                .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 19}},
                                .location = nolocation()
                            }
                        }},
                        .location = nolocation()
                    }
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        };

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
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
            .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}}, .location = nolocation()},
                .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}}, .location = nolocation()}
            }},
            .location = nolocation()
        }}},
        .location = nolocation()
    };

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
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
            .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},
            .location = nolocation()
        }}},
        .location = nolocation()
    };

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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Eq,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()},
                    }},
                    .location = nolocation()
                },
                .ifStmt = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 20}},.location = nolocation()},
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
                .elseStmt = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()},
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Eq,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()},
                    }},
                    .location = nolocation()},
                .ifStmt = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 20}},.location = nolocation()},
                                }},
                                .location = nolocation()
                            }}},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
                .elseStmt = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },
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
    const exp1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp1, &env), ast.Lit {.Bool = true});

    const exp2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp2, &env), ast.Lit {.Bool = false});

    const exp3: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp3, &env), ast.Lit {.Bool = false});

    // Test greater-than expressions
    const exp4: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp4, &env), ast.Lit {.Bool = false});

    const exp5: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp5, &env), ast.Lit {.Bool = false});

    const exp6: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gt,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp6, &env), ast.Lit {.Bool = true});

    // Test less-than or equal
    const exp7: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp7, &env), ast.Lit {.Bool = true});

    const exp8: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp8, &env), ast.Lit {.Bool = true});

    const exp9: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Lte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp9, &env), ast.Lit {.Bool = false});

    // Test greater-than or equal
    const exp10: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp10, &env), ast.Lit {.Bool = false});

    const exp11: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp11, &env), ast.Lit {.Bool = true});

    const exp12: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
            .op = .Gte,
            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqual(try interpreter.evalExpr(&exp12, &env), ast.Lit {.Bool = true});
}


test "break works" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.WhileStmt = &ast.WhileStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Gt,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                    }},
                    .location = nolocation()
                },
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {.stmt = ast.StmtInner {.BreakStmt = {}},.location = nolocation()},
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.WhileStmt = &ast.WhileStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Gt,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                    }},
                    .location = nolocation()
                },
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                ast.Stmt {
                                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                        ast.Stmt {.stmt = ast.StmtInner {.BreakStmt = {}},.location = nolocation()},
                                    }},
                                    .location = nolocation()
                                },
                            }},
                            .location = nolocation()},
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()},
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },
        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.WhileStmt = &ast.WhileStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Gt,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                    }},
                    .location = nolocation()
                },
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {.stmt = ast.StmtInner {.ContinueStmt = {}},.location = nolocation()},
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                                            .op = .Add,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },
        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {&ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()}
                }},
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.WhileStmt = &ast.WhileStmt {
                .cond = &ast.Expr {
                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .op = ast.BinOp.Gt,
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                    }},
                    .location = nolocation()
                },
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                            .op = .Sub,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                ast.Stmt {
                                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                        ast.Stmt {.stmt = ast.StmtInner {.ContinueStmt = {}},.location = nolocation()},
                                    }},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                                            .op = .Add,
                                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    },
                                }},
                                .location = nolocation()}},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },
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

        ast.Stmt {
            .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.stmt = ast.StmtInner {.BreakStmt = {}},.location = nolocation()}
            }},
            .location = nolocation()
        }
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
        ast.Stmt {.stmt = ast.StmtInner {.BreakStmt = {}},.location = nolocation()}
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

        ast.Stmt {
            .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                ast.Stmt {.stmt = ast.StmtInner {.ContinueStmt = {}},.location = nolocation()}
            }},
            .location = nolocation()
        }
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
        ast.Stmt {.stmt = ast.StmtInner {.ContinueStmt = {}},.location = nolocation()}
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "w"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()},
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},.location = nolocation()},
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "w"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },
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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},
                        .location = nolocation()
                    },
                },
                .location = nolocation()},
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "always1"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {},
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "arg1"}},.location = nolocation()},
                            .op = .Mul,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    },
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "double"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                            .op = .Add,
                            .rhs = &ast.Expr {
                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                                    .op = .Add,
                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},.location = nolocation()}
                                }},
                                .location = nolocation()
                            }
                        }},
                        .location = nolocation()
                    },
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "sum"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()},
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 13}},.location = nolocation()},
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -5}},.location = nolocation()},
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .BlockStmt = &[_] ast.Stmt {},
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "voidfun"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {},
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "other"}},.location = nolocation()},
                            .op = .Mul,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()}
                        }},
                        .location = nolocation()}
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mulbyx"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "other"}},.location = nolocation()},
                            .op = .Mul,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    }
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mulbyx"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                            .op = .Mul,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x2"}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    }
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mul"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()},
                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                &ast.Expr {
                                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                                    }},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {
                                .ReturnStmt = &ast.Expr {
                                    .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},
                                    .location = nolocation()
                                }
                            },
                            .location = nolocation()
                        }
                    }
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "return3"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {},
        }},
        .location = nolocation()
    };

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
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Void = {}}},
                    .location = nolocation()
                }},
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "dummyfun"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Void = {}}},
                    .location = nolocation()
                }},
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "dummyfun"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
                .stmt = ast.StmtInner {
                    .BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                                .cond = &ast.Expr {
                                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                        .op = .Eq,
                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                    }},
                                    .location = nolocation()
                                },
                                .ifStmt = ast.Stmt {
                                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}},
                                    .location = nolocation()
                                },
                                .elseStmt = null,
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .op = .Mul,
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "fac"}},.location = nolocation()},
                                            .args = &[_]*const ast.Expr {
                                                &ast.Expr {
                                                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                                        .op = .Sub,
                                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                                    }},
                                                    .location = nolocation()
                                                }
                                            }
                                        }},
                                        .location = nolocation()
                                    }
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "fac"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
            },
        }},
        .location = nolocation()};

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
                .stmt = ast.StmtInner {
                    .BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                                .cond = &ast.Expr {
                                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                        .op = .Eq,
                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                    }},
                                    .location = nolocation()
                                },
                                .ifStmt = ast.Stmt {
                                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}},
                                    .location = nolocation()
                                },
                                .elseStmt = null,
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .op = .Mul,
                                    .rhs = &ast.Expr {
                                        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "fac"}},.location = nolocation()},
                                            .args = &[_]*const ast.Expr {
                                                &ast.Expr {
                                                    .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                                        .op = .Sub,
                                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                                    }},
                                                    .location = nolocation()
                                                }
                                            }
                                        }},
                                        .location = nolocation()
                                    }
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }
                },
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "fac"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                    .cond = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                            .op = .Eq,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                        }},
                        .location = nolocation()
                    },
                    .ifStmt = ast.Stmt {
                        .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}},
                        .location = nolocation()
                    },
                    .elseStmt = ast.Stmt {
                        .stmt = ast.StmtInner { .IfElseStmt = &ast.IfElseStmt {
                            .cond = &ast.Expr {
                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .op = .Gt,
                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                                }},
                                .location = nolocation()
                            },
                            .ifStmt = ast.Stmt {
                                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                    .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "odd"}},.location = nolocation()},
                                        .args = &[_]*const ast.Expr {
                                            &ast.Expr {
                                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                                    .op = .Sub,
                                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                                                }},
                                                .location = nolocation()
                                            }
                                        }
                                    }},
                                    .location = nolocation()
                                }},
                                .location = nolocation()
                            },
                            .elseStmt = ast.Stmt {
                                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner { .Lit = ast.Lit {.Bool = false}},.location = nolocation()}},
                                .location = nolocation()
                            }
                        }},
                        .location = nolocation()
                    },
                }},
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    env.insert("odd", venv.ObjectVal {.Var = ast.Lit {
        .Callable = ast.Callable {
            .params = &[_] ast.Var {"x"},
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.IfElseStmt = &ast.IfElseStmt {
                    .cond = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                            .op = .Eq,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                        }},
                        .location = nolocation()
                    },
                    .ifStmt = ast.Stmt {
                        .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}},
                        .location = nolocation()
                    },
                    .elseStmt = ast.Stmt {
                        .stmt = ast.StmtInner { .IfElseStmt = &ast.IfElseStmt {
                            .cond = &ast.Expr {
                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                    .op = .Gt,
                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                                }},
                                .location = nolocation()
                            },
                            .ifStmt = ast.Stmt {
                                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                    .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "even"}},.location = nolocation()},
                                        .args = &[_]*const ast.Expr {
                                            &ast.Expr {
                                                .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                                                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                                    .op = .Sub,
                                                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                                                }},
                                                .location = nolocation()
                                            }
                                        }
                                    }},
                                    .location = nolocation()
                                }},
                                .location = nolocation()
                            },
                            .elseStmt = ast.Stmt {
                                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                    .expr = ast.ExprInner { .Lit = ast.Lit {.Bool = false}},
                                    .location = nolocation()
                                }},
                                .location = nolocation()
                            }
                        }},
                        .location = nolocation()
                    },
                }},
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expressions
    const expr1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "even"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

    const expr2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "even"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 5}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

    const expr3: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "odd"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 5}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

    const expr4: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
            .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "odd"}},.location = nolocation()},
            .args = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
            },
        }},
        .location = nolocation()
    };

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
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                    .expr = ast.ExprInner {.Lit = ast.Lit {.Void = {}}},
                    .location = nolocation()
                }},
                .location = nolocation()
            },
            .closure = &env,
        }
    }});

    // Prepare expression
    const stmt: ast.Stmt = ast.Stmt {
        .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
            .id = "last",
            .params = &[_]ast.Var {"x", "y", "z"},
            .body = ast.Stmt {
                .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},
                    .location = nolocation()
                }},
                .location = nolocation()
            }
        }},
        .location = nolocation()
    };

    // Evaluate statement
    _ = try interpreter.evalStmt(stmt, &env);

    // Assert that function computes correctly
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {
            .Callable = ast.Callable {
                .params = &[_] ast.Var {"x", "y", "z"},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "z"}},
                        .location = nolocation()
                    }},
                    .location = nolocation()
                },
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()},
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {"n"},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.BinOpExpr = ast.BinOpExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                            .op = .Mul,
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "n"}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 13}},.location = nolocation()},
                }},
                .location = nolocation()
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {
                                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
                                }
                            }},
                            .location = nolocation()
                        },
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 4}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },

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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "list"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "otherlist"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        },

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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {
                .AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {
                            .ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                            }
                        }},
                        .location = nolocation()
                    },
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "a"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {
                .AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {
                            .ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                            }
                        }},
                        .location = nolocation()
                    },
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "a"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {
                .AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {
                            .ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "a"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                            }
                        }},
                        .location = nolocation()
                    },
                    .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {
                    .AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}
                    }
                },
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {"lst"},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                            .lhs = &ast.Expr {
                                .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                    .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                                }}},
                                .location = nolocation()
                            },
                            .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    }
                },
                .location = nolocation()
            }}},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mylist"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {
                    .CallExpr = ast.CallExpr {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                        .args = &[_]*const ast.Expr {
                            &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mylist"}},.location = nolocation()}
                        }
                    }
                },
                .location = nolocation()
            }},
            .location = nolocation()
        }
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                &ast.Expr {
                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                    }}},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mylist"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {}
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
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


test "nested list function return" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                                .id = "g",
                                .params = &[_]ast.Var {},
                                .body = ast.Stmt {
                                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                                &ast.Expr {
                                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                                    }}},
                                                    .location = nolocation()
                                                }
                                            }},
                                            .location = nolocation()
                                        },
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}},
                                            .location = nolocation()
                                        }
                                    }},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr{
                                .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "g"}},.location = nolocation()},
                                    .args = &[_]*const ast.Expr {}
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mylist"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {}
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
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

test "multi list declaration with initialization" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {
                            .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                            }}},
                            .location = nolocation()
                        },
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -1}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {
                            .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "list"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 5}},.location = nolocation()}
                            }}},
                            .location = nolocation()
                        },
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}
                    }},
                    .location = nolocation()
                },
            }},
            .location = nolocation()
        }

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
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner { .Lval = ast.Lval {
            .PropertyAccess = ast.PropertyAccess {
                .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "list"}},.location = nolocation()},
                .prop = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "size"}},.location = nolocation()}
            }}
        },
        .location = nolocation()
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
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner { .Lval = ast.Lval {
            .PropertyAccess = ast.PropertyAccess {
                .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "list"}},.location = nolocation()},
                .prop = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "sz"}},.location = nolocation()}
            }}
                               },
        .location = nolocation()
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
    const expr: ast.Expr = ast.Expr {
        .expr = ast.ExprInner { .Lval = ast.Lval {
            .PropertyAccess = ast.PropertyAccess {
                .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "var"}},.location = nolocation()},
                .prop = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "size"}},.location = nolocation()}
            }}
                               },
        .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "list"}},.location = nolocation()},
                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                    }}},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {
                            .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                            }}},
                            .location = nolocation()
                        },
                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()}
                    }},
                    .location = nolocation()
                }}
            },
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner { .AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {
                            .Var = "list"
                        }},
                        .location = nolocation()
                    },
                    .rhs = &ast.Expr {
                        .expr = ast.ExprInner {.Lval = ast.Lval {
                            .Var = "lst"
                        }},
                        .location = nolocation()
                    }
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                &ast.Expr {
                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                    }}},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {
                .ExprStmt = &ast.Expr {
                    .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                        .args = &[_]*const ast.Expr {}
                    }},
                    .location = nolocation()
                }
            },
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                                .id = "g",
                                .params = &[_]ast.Var {},
                                .body = ast.Stmt {
                                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                                &ast.Expr {
                                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                                    }}},
                                                    .location = nolocation()
                                                }
                                            }},
                                            .location = nolocation()
                                        },
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                                .expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},
                                                .location = nolocation()
                                            }},
                                            .location = nolocation()
                                        }
                                    }},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr{
                                .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "g"}},.location = nolocation()},
                                    .args = &[_]*const ast.Expr {}
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                    .args = &[_]*const ast.Expr {}
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                                .id = "g",
                                .params = &[_]ast.Var {},
                                .body = ast.Stmt {
                                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                                &ast.Expr {
                                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                                    }}},
                                                    .location = nolocation()
                                                },

                                                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "var"}},.location = nolocation()}
                                            }},
                                            .location = nolocation()
                                        },
                                        ast.Stmt {
                                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                                                .expr = ast.ExprInner {
                                                    .AssignExpr = ast.AssignExpr {
                                                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "var"}},.location = nolocation()},
                                                        .rhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}
                                                    }
                                                },
                                                .location = nolocation()
                                            }},
                                            .location = nolocation()
                                        }
                                    }},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr{
                                .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "g"}},.location = nolocation()},
                                    .args = &[_]*const ast.Expr {}
                                }},
                                .location = nolocation()
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "mylist"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {}
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                    .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                    .args = &[_]*const ast.Expr {}
                }},
                .location = nolocation()
            }},
            .location = nolocation()
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

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.BlockStmt = &[_]ast.Stmt {
                        ast.Stmt {
                            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                                &ast.Expr {
                                    .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()},
                                        .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 10}},.location = nolocation()}
                                    }}},
                                    .location = nolocation()
                                }
                            }},
                            .location = nolocation()
                        },
                        ast.Stmt {
                            .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "lst"}},.location = nolocation()}},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.PrintStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                        .args = &[_]*const ast.Expr {}
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    // Make sure that y is set with updated x
    _ = try interpreter.evalProc(proc, &env);
}


test "immediate list function return" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                            &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                        }},
                        .location = nolocation()
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                                .id = &ast.Expr {
                                    .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                        .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                        .args = &[_]*const ast.Expr {}
                                    }},
                                    .location = nolocation()
                                }
                            }}},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    // Evaluate procedure
    _ = try interpreter.evalProc(proc, &env);
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.Int = 1}},
        env.lookup("x")
    );
}

test "declare list immediate" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                &ast.Expr {
                                    .expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},
                                    .location = nolocation()
                                },
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()}
            }},
            .location = nolocation()
        },

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 1);
    defer std.testing.allocator.free(items);
    for (items) |*item| item.* = ast.Lit {.Int = 1};

    const ptr = try std.testing.allocator.create(ast.List);
    defer std.testing.allocator.destroy(ptr);
    ptr.* = ast.List {
        .items = items,
        .len = 1,
        .refs = 1
    };

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("x")
    );
}

test "declare list of list immediates" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {
                            .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 5}},.location = nolocation()}
                            }}},
                            .location = nolocation()
                        },
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    const inner_items = try std.testing.allocator.alloc(ast.Lit, 1);
    for (inner_items) |*item| item.* = ast.Lit {.Int = 1};
    const inner_list = try std.testing.allocator.create(ast.List);
    inner_list.* = ast.List {
        .items = inner_items,
        .len = 1,
        .refs = 5
    };

    const items = try std.testing.allocator.alloc(ast.Lit, 5);
    for (items) |*item| item.* = ast.Lit {.List = inner_list};

    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items,
        .len = 5,
        .refs = 1
    };

    defer ptr.destroyAll(std.testing.allocator);

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("x")
    );
}

test "list immediate to funcall" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {"x"},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {
                            .Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()}
                            }}
                        },
                        .location = nolocation()
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {
                                    &ast.Expr {
                                        .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                            &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 9}},.location = nolocation()},
                                            &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 8}},.location = nolocation()},
                                            &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()},
                                        }},
                                        .location = nolocation()
                                    }
                                }
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.Int = 9}},
        env.lookup("x")
    );
}

test "return list immediate" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {
                            .ListExpr = &[_]*const ast.Expr {
                                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -1}},.location = nolocation()},
                            }
                        },
                        .location = nolocation()
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.CallExpr = ast.CallExpr {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                                .args = &[_]*const ast.Expr {}
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 1);
    for (items) |*item| item.* = ast.Lit {.Int = -1};
    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items,
        .len = 1,
        .refs = 1
    };

    defer ptr.destroyAll(std.testing.allocator);

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("x")
    );
}

test "list immediate assign expression" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                    .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                    .rhs = &ast.Expr {
                        .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "y"}},.location = nolocation()},
                            .rhs = &ast.Expr {
                                .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()}
                                }},
                                .location = nolocation()
                            }
                        }},
                        .location = nolocation()
                    }
                }},
                .location = nolocation()
            }},
            .location = nolocation()
        },

    }};

    const items = try std.testing.allocator.alloc(ast.Lit, 1);
    for (items) |*item| item.* = ast.Lit {.Int = 1};
    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items,
        .len = 1,
        .refs = 2
    };

    defer std.testing.allocator.free(items);
    defer std.testing.allocator.destroy(ptr);

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("x")
    );
    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("y")
    );
}


test "list immediate indexing" {

    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Test less-than expressions
    const exp1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {
                .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
                }},
                .location = nolocation()
            },
            .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()}
        }}},
        .location = nolocation()
    };

    const exp2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.Lval = ast.Lval {.ListIndex = ast.ListIndex {
            .id = &ast.Expr {
                .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
                }},
                .location = nolocation()
            },
            .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()}
        }}},
        .location = nolocation()
    };

    try std.testing.expectEqual(
        try interpreter.evalExpr(&exp1, &env),
        ast.Lit {.Int = 1}
    );

    try std.testing.expectEqual(
        try interpreter.evalExpr(&exp2, &env),
        ast.Lit {.Int = 3}
    );
}

test "nested list immediate" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {


        ast.Stmt {
            .stmt = ast.StmtInner {.DeclareStmt = &[_]*const ast.Expr {
                &ast.Expr {
                    .expr = ast.ExprInner {.AssignExpr = ast.AssignExpr {
                        .lhs = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                        .rhs = &ast.Expr {
                            .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                                &ast.Expr {
                                    .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                                        &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -1}},.location = nolocation()},
                                        &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -1}},.location = nolocation()},
                                    }},
                                    .location = nolocation()
                                },
                                &ast.Expr {.expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {}},.location = nolocation()},
                            }},
                            .location = nolocation()
                        }
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

    }};

    // Main list
    const items = try std.testing.allocator.alloc(ast.Lit, 3);
    items[0] = ast.Lit {.Int = 0};
    const ptr = try std.testing.allocator.create(ast.List);
    ptr.* = ast.List {
        .items = items,
        .len = 3,
        .refs = 1
    };

    //Inner list 1
    {
        const itms = try std.testing.allocator.alloc(ast.Lit, 2);
        for (itms) |*item| item.* = ast.Lit {.Int = -1};
        const lst = try std.testing.allocator.create(ast.List);
        lst.* = ast.List {
            .items = itms,
            .len = 2,
            .refs = 1
        };
        items[1] = ast.Lit {.List = lst};
    }

    // Inner list 2
    {
        const itms = try std.testing.allocator.alloc(ast.Lit, 0);
        const lst = try std.testing.allocator.create(ast.List);
        lst.* = ast.List {
            .items = itms,
            .len = 0,
            .refs = 1
        };
        items[2] = ast.Lit {.List = lst};
    }

    defer ptr.destroyAll(std.testing.allocator);

    _ = try interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        venv.ObjectVal {.Var = ast.Lit {.List = ptr}},
        env.lookup("x")
    );
}

test "list immediate expression statement" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -1}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 3}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 7}},.location = nolocation()},
                }},
                .location = nolocation()
            }},
            .location = nolocation()
        }

    }};

    _ = try interpreter.evalProc(proc, &env);
}

test "callable in list immediate" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Prepare procedure
    const proc: ast.Proc = ast.Proc {.stmts = &[_]ast.Stmt {

        ast.Stmt {
            .stmt = ast.StmtInner {.FunDefStmt = &ast.FunDefStmt {
                .id = "f",
                .params = &[_]ast.Var {"x"},
                .body = ast.Stmt {
                    .stmt = ast.StmtInner {.ReturnStmt = &ast.Expr {
                        .expr = ast.ExprInner {
                            .Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                .id = &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "x"}},.location = nolocation()},
                                .idx = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()}
                            }}
                        },
                        .location = nolocation()
                    }},
                    .location = nolocation()
                }
            }},
            .location = nolocation()
        },

        ast.Stmt {
            .stmt = ast.StmtInner {.ExprStmt = &ast.Expr {
                .expr = ast.ExprInner {.ListExpr = &[_]*const ast.Expr {
                    &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
                    &ast.Expr {.expr = ast.ExprInner {.Lval = ast.Lval {.Var = "f"}},.location = nolocation()},
                }},
                .location = nolocation()
            }},
            .location = nolocation()
        }

    }};

    const r: interpreter.EvalError!void = interpreter.evalProc(proc, &env);

    try std.testing.expectEqualDeep(
        r,
        interpreter.EvalError.InvalidUpcall
    );
}

test "type conversion int - bool" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Test different cases
    const e1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e1, &env),
        ast.Lit {.Bool = true}
    );
    const e2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e2, &env),
        ast.Lit {.Bool = false}
    );
    const e3: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e3, &env),
        ast.Lit {.Int = 1}
    );
    const e4: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = false}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e4, &env),
        ast.Lit {.Int = 0}
    );
    const e5: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 2}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e5, &env),
        ast.Lit {.Bool = true}
    );
    const e6: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -124}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e6, &env),
        ast.Lit {.Bool = true}
    );
    const e7: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = -(0)}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e7, &env),
        ast.Lit {.Bool = false}
    );
}

test "type conversion float - bool" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Test different cases
    const e1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 1.0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e1, &env),
        ast.Lit {.Bool = true}
    );
    const e2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 0.0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e2, &env),
        ast.Lit {.Bool = false}
    );
    const e3: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = true}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Float}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e3, &env),
        ast.Lit {.Float = 1.0}
    );
    const e4: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Bool = false}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Float}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e4, &env),
        ast.Lit {.Float = 0.0}
    );
    const e5: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 2.00203}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e5, &env),
        ast.Lit {.Bool = true}
    );
    const e6: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = -1.3232042}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e6, &env),
        ast.Lit {.Bool = true}
    );
    const e7: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = -(0.0)}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e7, &env),
        ast.Lit {.Bool = false}
    );
    const e8: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 0.000001}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Bool}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e8, &env),
        ast.Lit {.Bool = true}
    );
}


test "type conversion int - float" {
    // Prepare environment
    var env = venv.Env.new(std.testing.allocator);
    defer env.deinit();

    // Test different cases
    const e1: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 1.0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e1, &env),
        ast.Lit {.Int = 1}
    );
    const e2: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 0.0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e2, &env),
        ast.Lit {.Int = 0}
    );
    const e3: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 1}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Float}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e3, &env),
        ast.Lit {.Float = 1.0}
    );
    const e4: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Int = 0}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Float}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e4, &env),
        ast.Lit {.Float = 0.0}
    );
    const e5: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 2.00203}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e5, &env),
        ast.Lit {.Int = 2}
    );
    const e6: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = -1.3232042}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e6, &env),
        ast.Lit {.Int = -1}
    );
    const e7: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = -(0.0)}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()
    };
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e7, &env),
        ast.Lit {.Int = 0}
    );
    const e8: ast.Expr = ast.Expr {
        .expr = ast.ExprInner {.AsExpr = ast.AsExpr {
            .lhs = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Float = 0.000001}},.location = nolocation()},
            .as = &ast.Expr {.expr = ast.ExprInner {.Lit = ast.Lit {.Type = .Int}},.location = nolocation()}
        }},
        .location = nolocation()};
    try std.testing.expectEqualDeep(
        try interpreter.evalExpr(&e8, &env),
        ast.Lit {.Int = 0}
    );
}
