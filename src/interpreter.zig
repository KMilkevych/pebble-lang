const ast = @import("ast.zig");
const venv = @import("env.zig");

const std = @import("std");

// Error types
const TypeError = error {
    MismatchedType,
    NotIdentifier,
};
const ArithmeticError = error {
    DivisionByZero
};
const ValueError = error {
    UndefinedVariable,
    UnexpectedVoidValue,
    IndexOutOfBounds,
    InvalidSize,
    InvalidProperty,
};
const SemanticError = error {
    IdentifierAlreadyDeclared,
    UnexpectedBreak,
    UnexpectedContinue,
    UnexpectedReturn,
    NotCallable,
    WrongArgCount,
    InvalidUpcall,
    NotList,
    ListReference,
    ReadOnlyProperty,
};

pub const EvalError = TypeError || ArithmeticError || ValueError || SemanticError;

pub const StmtReturn: type = union(enum) {
    NoReturn: void,
    Continue: void,
    Break: void,
    Return: ast.Lit,
};

// Functions
pub fn evalLval(lval: ast.Lval, env: *venv.Env) EvalError!ast.Lit {
    const lit: ast.Lit = sw: switch (lval) {
        .Var => |v| {

            // Lookup in environment
            if (!env.isDeclaredGlobal(v)) return ValueError.UndefinedVariable;
            const value: venv.ObjectVal = env.lookup(v).?;

            // Evalue to a literal based on value
            break :sw switch(value) {
                .Var => |val| switch (val) {
                    // TODO: Figure out if this is needed as objectvals store literals
                    .Int => |ival| ast.Lit {.Int = ival},
                    .Bool => |bval| ast.Lit {.Bool = bval},
                    .Void => return ValueError.UnexpectedVoidValue,
                    .Callable => |fun| ast.Lit {.Callable = fun},
                    .List => |lst| ast.Lit {.List = lst.makeReference()},
                },
                .Undefined => return EvalError.UndefinedVariable
            };

        },
        .ListIndex => |l| {

            // Evaluate identifier (should lookup in environment..)
            var list: *ast.List = switch (try evalExpr(l.id, env)) {
                .List => |lst| lst,
                .Int, .Bool, .Void, .Callable => return EvalError.NotList
            };
            defer list.destroyAll(env.allocator);

            // Evaluate index
            const index: i64 = switch (try evalExpr(l.idx, env)) {
                .Int => |i| i,
                .Bool, .Void, .Callable, .List => return EvalError.MismatchedType
            };

            // Make sure index is within bounds
            if (index < 0 or index >= list.len) return EvalError.IndexOutOfBounds;

            // Lookup by index and return
            const lit: ast.Lit = list.items[@as(usize, @intCast(index))];
            break :sw switch (lit) {
                .List => |lst| ast.Lit {.List = lst.makeReference()},
                else => lit
            };
        },
        .PropertyAccess => |p| {

            // Evalutate property
            const id: []const u8 = switch (p.prop.*) {
                .Lval => |lv| switch (lv) {
                    .Var => |v| v,
                    else => return EvalError.InvalidProperty
                },
                else => return EvalError.InvalidProperty
            };

            // Evaluate left-hand-side
            var lhs: ast.Lit = try evalExpr(p.lhs, env);
            defer lhs.destroyAll(env.allocator);

            switch (lhs) {
                .List => |l| {
                    if (std.mem.eql(u8, id, "size")) break :sw ast.Lit {.Int = @intCast(l.len)}
                    else return EvalError.InvalidProperty;
                },
                else => return EvalError.InvalidProperty
            }

        }
    };

    // Create reference on evaluation
    return lit;
}

pub fn evalBinOpExpr(expr: *const ast.BinOpExpr, env: *venv.Env) EvalError!ast.Lit {

    // Evaluate operands
    const lhs = try evalExpr(expr.lhs, env);
    const rhs = try evalExpr(expr.rhs, env);

    // Evaluate expression
    return switch (expr.op) {
        .Add => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l + r},
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .Sub => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l - r},
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .Mul => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l * r},
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .Div => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| {
                    if (r == 0) return ArithmeticError.DivisionByZero;
                    return ast.Lit {.Int = @divFloor(l, r)};
                },
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .And => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l and r},
                .Int, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Int, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .Or => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l or r},
                .Int, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Int, .Callable, .Void, .List => TypeError.MismatchedType,
        },
        .Eq => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l == r},
                .Int, .Callable, .Void, .List => ast.Lit {.Bool = false},
            },
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void, .List => ast.Lit {.Bool = false},
                .Int => |r| ast.Lit {.Bool = l == r}
            },
            .Callable => |l| switch (rhs) {
                .Callable => |r| ast.Lit {.Bool = std.meta.eql(l, r)},
                .Int, .Bool, .Void, .List => ast.Lit {.Bool = false},
            },
            .List => |l| switch (rhs) {
                .Int, .Bool, .Callable, .Void => ast.Lit {.Bool = false},
                .List => |r| ast.Lit {.Bool = std.meta.eql(l.items, r.items)},
            },
            .Void => ast.Lit {.Bool = false},
        },
        .Lt => switch (lhs) {
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l < r}
            }
        },
        .Gt => switch (lhs) {
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l > r}
            }
        },
        .Lte => switch (lhs) {
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l <= r}
            }
        },
        .Gte => switch (lhs) {
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l >= r}
            }
        },
    };
}

pub fn evalUnOpExpr(expr: *const ast.UnOpExpr, env: *venv.Env) EvalError!ast.Lit {

    // Evaluate operand
    var lit = try evalExpr(expr.rhs, env);
    defer lit.destroyAll(env.allocator);

    // Evaluate operator
    return switch (expr.op) {
        .Not => switch (lit) {
            .Int, .Callable, .Void, .List => TypeError.MismatchedType,
            .Bool => |val| ast.Lit { .Bool = !val },
        },
        .Neg => switch (lit) {
            .Int => |val| ast.Lit { .Int = -val },
            .Bool, .Callable, .Void, .List => TypeError.MismatchedType,
        },
    };
}

pub fn evalAssignExpr(expr: *const ast.AssignExpr, env: *venv.Env) EvalError!ast.Lit {

    // TODO: Prevent creating another Python and actually require types and declarations

    // Make sure left-hand side is an lval
    const lval: ast.Lval = switch (expr.lhs.*) {
        .Lval => |lv| lv,
        else => return EvalError.NotIdentifier,
    };

    // Evaluate right-hand side
    const rhs: ast.Lit = try evalExpr(expr.rhs, env);

    // Perform assignment
    switch (lval) {
        .Var => |id| {
            if (!env.isDeclaredGlobal(id)) return EvalError.UndefinedVariable;

            // Destroy existing value
            switch (env.lookup(id) orelse unreachable) {
                .Undefined => {},
                .Var => |v| switch (v) {
                    .List => |l| l.destroyAll(env.allocator),
                    .Callable => |c| c.destroyAll(env.allocator),
                    .Int, .Bool, .Void => {}
                }
            }

            // Insert new value
            env.insertScoping(id, venv.ObjectVal {.Var = rhs});
        },
        .ListIndex => |lidx| {

            // Make sure that left-hand side is a list
            // NOTE: this creates an additional reference
            var lst: *ast.List = switch (try evalExpr(lidx.id, env)) {
                .List => |l| l,
                .Int, .Bool, .Void, .Callable => return EvalError.NotList
            };
            defer lst.destroyAll(env.allocator);

            // Evaluate index
            const idx: i64 = switch (try evalExpr(lidx.idx, env)) {
                .Int => |i| i,
                .Bool, .Void, .Callable, .List => return EvalError.MismatchedType,
            };

            // Bounds checking
            if (idx < 0 or idx >= lst.len) return EvalError.IndexOutOfBounds;

            // Destroy existing value
            switch (lst.items[@as(usize, @intCast(idx))]) {
                .List, .Callable => lst.items[@as(usize, @intCast(idx))].destroyAll(env.allocator),
                .Int, .Bool, .Void => {}
            }

            // Perform assignment
            lst.items[@as(usize, @intCast(idx))] = rhs;
        },
        .PropertyAccess => return EvalError.ReadOnlyProperty // TODO: Implement real properties
    }

    return switch (rhs) {
        .List => |l| ast.Lit {.List = l.makeReference()},
        else => rhs,
    };
}

pub fn evalCallExpr(expr: *const ast.CallExpr, env: *venv.Env) EvalError!ast.Lit {

    // Evaluate identifier
    const callable: ast.Callable = switch(try evalExpr(expr.id, env)) {
        .Callable => |fun| fun,
        .Int, .Bool, .Void, .List => return EvalError.NotCallable,
    };

    // Create a scoped environment based on function closure
    var scopedEnv: venv.Env = callable.closure.newScoped();
    defer scopedEnv.deinit();

    // Bind all args to function parameter names
    if (callable.params.len != expr.args.len) return EvalError.WrongArgCount;
    for (callable.params, expr.args) |id, arg| {

        // NOTE: Evaluating in current environment, not in closure or scoped
        const r: ast.Lit = try evalExpr(arg, env);

        // Bind in environment directly
        // NOTE: Can also declare and then insertScoped
        scopedEnv.insert(id, venv.ObjectVal {.Var = r});
    }

    // Evaluate function body
    return switch(try evalStmt(callable.body, &scopedEnv)) {
        .NoReturn => ast.Lit {.Void = {}},
        .Return => |lit| lit,
        .Break => EvalError.UnexpectedBreak,
        .Continue => EvalError.UnexpectedContinue
    };
}

pub fn evalListExpr(exprs: []const *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {

    // Create accumulator for contents
    var acc: std.ArrayList(ast.Lit) = .init(env.allocator);
    errdefer acc.deinit();
    errdefer for (acc.items) |*item| item.destroyAll(env.allocator);

    // Evaluate each entry while preventing callables
    for (exprs) |expr| {
        const lit: ast.Lit = try evalExpr(expr, env);
        switch (lit) {
            .Callable => return SemanticError.InvalidUpcall,
            else => acc.append(lit) catch unreachable
        }
    }

    // Create a list literal
    const ptr = env.allocator.create(ast.List) catch unreachable;
    ptr.* = ast.List {
        .len = acc.items.len,
        .items = acc.toOwnedSlice() catch unreachable,
    };

    return ast.Lit {.List = ptr};
}


pub fn evalExpr(expr: *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {
    return switch (expr.*) {
        .BinOpExpr => |ex| evalBinOpExpr(&ex, env),
        .UnOpExpr => |ex| evalUnOpExpr(&ex, env),
        .Lval => |lval| evalLval(lval, env),

        // NOTE: We create an extra reference here, assuming this is bottom level
        .Lit => |lit| switch (lit) {
            .List => |l| ast.Lit {.List = l.makeReference()},
            else => lit
        },

        .AssignExpr => |ex| evalAssignExpr(&ex, env),
        .CallExpr => |ex| evalCallExpr(&ex, env),
        .ListExpr => |ex| evalListExpr(ex, env),
    };
}


pub fn evalStmt(statement: ast.Stmt, env: *venv.Env) EvalError!StmtReturn {

    switch (statement) {
        .ExprStmt => |expr| {

            // Evaluate the underlying expression
            // NOTE: Inner-most return statement in CallExpr creates list reference
            // which needs to be discarded for an ExprStmt
            var lit: ast.Lit = try evalExpr(expr, env);
            lit.destroyAll(env.allocator);

            return StmtReturn {.NoReturn = {}};
        },
        .DeclareStmt => |exprs| {

            // DECLARE X evaluates X = undefined
            // DECLARE X = 10 evaluates X = 10

            // Evaluate each expression one by one
            for (exprs) |expr| {

                // Make sure the expression is an assignment expression or Lval
                const lhs: EvalError!ast.Lval = switch (expr.*) {
                    .Lval => |lval| lval,
                    .AssignExpr => |exp| switch (exp.lhs.*) {
                        .Lval => |lval| lval,
                        else => EvalError.NotIdentifier,
                    },
                    else => return EvalError.NotIdentifier
                };

                switch (try lhs) {
                    .Var => |id| {

                        // Do error checking
                        if (env.isDeclaredLocal(id)) return EvalError.IdentifierAlreadyDeclared;

                        // Add lhs identifier to environment
                        env.insert(id, venv.ObjectVal {.Undefined = {}});

                        // Evalute assignment expression
                        switch (expr.*) {
                            .AssignExpr => |*exp| {
                                var val: ast.Lit = try evalAssignExpr(exp, env);
                                val.destroyAll(env.allocator);
                            },
                            .Lval => {},
                            else => unreachable // Sensible here
                        }

                    },
                    .ListIndex => |li| {

                        // Make sure that id is a Var
                        const id: ast.Var = switch (li.id.*) {
                            .Lval => |lv| switch (lv) {
                                .Var => |v| v,
                                .ListIndex, .PropertyAccess => return EvalError.NotIdentifier,
                            },
                            else => return EvalError.NotIdentifier
                        };

                        // Do error checking
                        if (env.isDeclaredLocal(id)) return EvalError.IdentifierAlreadyDeclared;

                        // Evaluate index (size)
                        const size: i64 = switch (try evalExpr(li.idx, env)) {
                            .Int => |i| i,
                            .Bool, .Callable, .Void, .List => return EvalError.MismatchedType,
                        };

                        // Do size checking
                        if (size < 0) return EvalError.InvalidSize;

                        // Add identifier to environment
                        const items_ptr = env.allocator.alloc(ast.Lit, @as(usize, @intCast(size))) catch unreachable;
                        const ptr = env.allocator.create(ast.List) catch unreachable;

                        ptr.* = ast.List {
                            .len = @as(usize, @intCast(size)),
                            .items = items_ptr,
                        };
                        env.insert(id, venv.ObjectVal {.Var = ast.Lit {.List = ptr}});

                        // Evaluate right-hand side and destroy if exists
                        var val: ast.Lit = switch (expr.*) {
                            .AssignExpr => |as| try evalExpr(as.rhs, env),
                            .Lval => ast.Lit {.Void = {}},
                            else => unreachable
                        };
                        defer val.destroyAll(env.allocator);

                        // Initialize list with values
                        var idx: i64 = 0;
                        while (idx < size) : (idx += 1) {

                            const exp: ast.AssignExpr = ast.AssignExpr {
                                .lhs = &ast.Expr {
                                    .Lval = ast.Lval {.ListIndex = ast.ListIndex {
                                        .id = li.id,
                                        .idx = &ast.Expr {.Lit = ast.Lit {.Int = idx}}
                                    }}
                                },
                                .rhs = &ast.Expr {.Lit = val}
                            };

                            // Use reflection-like code to build initialization
                            var lit: ast.Lit = try evalAssignExpr(&exp, env);
                            lit.destroyAll(env.allocator);
                        }
                    },
                    .PropertyAccess => return EvalError.InvalidProperty
                }
            }

            return StmtReturn {.NoReturn = {}};
        },
        .PrintStmt => |exprs| {
            for (exprs) |expr| {
                // Evaluate expression and print resulting literal
                // TODO: Use parameterized writer and remove unreachable
                var res: ast.Lit = try evalExpr(expr, env);
                std.io.getStdOut().writer().print("{} ", .{res}) catch unreachable;
                res.destroyAll(env.allocator);
            }
            std.io.getStdOut().writer().print("\n", .{}) catch unreachable;
            return StmtReturn {.NoReturn = {}};
        },
        .BlockStmt => |stmts| {
            // Start new environment
            var nestedEnv: venv.Env = env.newScoped();
            defer nestedEnv.deinit();

            // Evalute each statement with nested environment
            for (stmts) |stmt| switch (try evalStmt(stmt, &nestedEnv)) {
                .NoReturn => {},
                .Return => |lit| return StmtReturn {.Return = lit},
                .Break => return StmtReturn {.Break = {}},
                .Continue => return StmtReturn {.Continue = {}},
            };

            return StmtReturn {.NoReturn = {}};
        },
        .IfElseStmt => |stmt| {

            // Check condition
            var cond_res: ast.Lit = try evalExpr(stmt.cond, env);
            const res: bool = switch (cond_res) {
                .Int, .Callable, .Void, .List => return TypeError.MismatchedType,
                .Bool => |b| b,
            };

            cond_res.destroyAll(env.allocator);

            // Construct scoped environment
            var scopedEnv = env.newScoped();
            defer scopedEnv.deinit();

            // Evaluate if or else branch
            if (res)
                return try evalStmt(stmt.ifStmt, &scopedEnv)
            else if (stmt.elseStmt) |es|
                return try evalStmt(es, &scopedEnv)
            else
                return StmtReturn {.NoReturn = {}};

        },
        .WhileStmt => |stmt| {

            while (true) {

                // Check condition
                var cond_lit: ast.Lit = try evalExpr(stmt.cond, env);
                const res: bool = switch (cond_lit) {
                    .Int, .Callable, .Void, .List => return TypeError.MismatchedType,
                    .Bool => |b| b,
                };

                cond_lit.destroyAll(env.allocator);

                if (!res) break;

                // Evaluate body in a scoped environment
                var scoped: venv.Env = env.newScoped();
                defer scoped.deinit();

                // Evaluate body and consume break statements
                switch(try evalStmt(stmt.body, &scoped)) {
                    .NoReturn => {},
                    .Return => |lit| return StmtReturn {.Return = lit},
                    .Break => break,
                    .Continue => continue,
                }
            }

            return StmtReturn {.NoReturn = {}};
        },
        .BreakStmt => return StmtReturn {.Break = {}},
        .ContinueStmt => return StmtReturn {.Continue = {}},
        .ReturnStmt => |expr| {
            // Evaluate expression and make sure its not a Callable
            // If expr is a CallExpr, we do not make extra list reference
            const res: ast.Lit = try evalExpr(expr, env);
            return switch (res) {
                .Callable => EvalError.InvalidUpcall,
                else => StmtReturn {.Return = res},
            };
        },
        .FunDefStmt => |stmt| {

            // Insert function into environment
            if (env.isDeclaredLocal(stmt.id)) return EvalError.IdentifierAlreadyDeclared;
            env.insert(
                stmt.id,
                venv.ObjectVal {.Var = ast.Lit {.Callable = ast.Callable {
                    .params = stmt.params,
                    .body = stmt.body,
                    .closure = env
                }}}
            );

            return StmtReturn {.NoReturn = {}};
        },
    }

    unreachable;
}

pub fn evalProc(procedure: ast.Proc, env: *venv.Env) EvalError!void {

    // Evaluate statements one by one
    for (procedure.stmts) |stmt| switch (try evalStmt(stmt, env)) {
        .NoReturn => {},
        .Return => return EvalError.UnexpectedReturn,
        .Break => return EvalError.UnexpectedBreak,
        .Continue => return EvalError.UnexpectedContinue,
    };
}
