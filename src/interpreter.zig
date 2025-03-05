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
};
const SemanticError = error {
    IdentifierAlreadyDeclared,
    UnexpectedBreak,
    UnexpectedContinue,
    UnexpectedReturn,
    NotCallable,
    WrongArgCount,
};

pub const EvalError = TypeError || ArithmeticError || ValueError || SemanticError;

pub const StmtReturn: type = union(enum) {
    NoReturn: void,
    Continue: void,
    Break: void,
    Return: ast.Lit,
};

// Functions
pub fn evalLval(lval: ast.Lval, env: *venv.Env) ValueError!ast.Lit {
    return switch (lval) {
        .Var => |v| {

            // Lookup in environment
            if (!env.isDeclaredGlobal(v)) return ValueError.UndefinedVariable;
            const value: venv.ObjectVal = env.lookup(v).?;

            // Evalue to a literal based on value
            return switch(value) {
                .Var => |val| switch (val) {
                    // TODO: Figure out if this is needed as objectvals store literals
                    .Int => |ival| ast.Lit {.Int = ival},
                    .Bool => |bval| ast.Lit {.Bool = bval},
                    .Void => ValueError.UnexpectedVoidValue,
                    .Callable => |fun| ast.Lit {.Callable = fun}
                },
                .Undefined => EvalError.UndefinedVariable
            };

        }
    };
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
                .Bool, .Callable, .Void => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void => TypeError.MismatchedType,
        },
        .Sub => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l - r},
                .Bool, .Callable, .Void => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void => TypeError.MismatchedType,
        },
        .Mul => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l * r},
                .Bool, .Callable, .Void => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void => TypeError.MismatchedType,
        },
        .Div => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| {
                    if (r == 0) return ArithmeticError.DivisionByZero;
                    return ast.Lit {.Int = @divFloor(l, r)};
                },
                .Bool, .Callable, .Void => TypeError.MismatchedType,
            },
            .Bool, .Callable, .Void => TypeError.MismatchedType,
        },
        .And => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l and r},
                .Int, .Callable, .Void => TypeError.MismatchedType,
            },
            .Int, .Callable, .Void => TypeError.MismatchedType,
        },
        .Or => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l or r},
                .Int, .Callable, .Void => TypeError.MismatchedType,
            },
            .Int, .Callable, .Void => TypeError.MismatchedType,
        },
        .Eq => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l == r},
                .Int, .Callable, .Void => ast.Lit {.Bool = false},
            },
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void => ast.Lit {.Bool = false},
                .Int => |r| ast.Lit {.Bool = l == r}
            },
            .Callable => |l| switch (rhs) {
                .Callable => |r| ast.Lit {.Bool = std.meta.eql(l, r)},
                .Int, .Bool, .Void => ast.Lit {.Bool = false},
            },
            .Void => ast.Lit {.Bool = false},
        },
        .Lt => switch (lhs) {
            .Bool, .Callable, .Void => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l < r}
            }
        },
        .Gt => switch (lhs) {
            .Bool, .Callable, .Void => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l > r}
            }
        },
        .Lte => switch (lhs) {
            .Bool, .Callable, .Void => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l <= r}
            }
        },
        .Gte => switch (lhs) {
            .Bool, .Callable, .Void => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool, .Callable, .Void => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l >= r}
            }
        },
    };
}

pub fn evalUnOpExpr(expr: *const ast.UnOpExpr, env: *venv.Env) EvalError!ast.Lit {

    // Evaluate operand
    const lit = try evalExpr(expr.rhs, env);

    // Evaluate operator
    return switch (expr.op) {
        .Not => switch (lit) {
            .Int, .Callable, .Void => TypeError.MismatchedType,
            .Bool => |val| ast.Lit { .Bool = !val },
        },
        .Neg => switch (lit) {
            .Int => |val| ast.Lit { .Int = -val },
            .Bool, .Callable, .Void => TypeError.MismatchedType,
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
            env.insertScoping(id, venv.ObjectVal {.Var = rhs});
        }
    }

    return rhs;
}

pub fn evalCallExpr(expr: *const ast.CallExpr, env: *venv.Env) EvalError!ast.Lit {

    // Evaluate identifier
    const callable: ast.Callable = switch(try evalExpr(expr.id, env)) {
        .Callable => |fun| fun,
        .Int, .Bool, .Void => return EvalError.NotCallable,
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


pub fn evalExpr(expr: *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {
    return switch (expr.*) {
        .BinOpExpr => |ex| evalBinOpExpr(&ex, env),
        .UnOpExpr => |ex| evalUnOpExpr(&ex, env),
        .Lval => |lval| evalLval(lval, env),
        .Lit => |lit| lit,
        .AssignExpr => |ex| evalAssignExpr(&ex, env),
        .CallExpr => |ex| evalCallExpr(&ex, env),
    };
}


pub fn evalStmt(statement: ast.Stmt, env: *venv.Env) EvalError!StmtReturn {

    switch (statement) {
        .ExprStmt => |expr| {

            // Evaluate and return underlying expression
            _ = try evalExpr(expr, env);
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

                // Evalute left-hand side lval to an identifier
                // TODO: This can also be array indexing, object field/property, etc.
                const id: []const u8 = switch (try lhs) {
                    .Var => |id| id
                };

                // Do error checking
                if (env.isDeclaredLocal(id)) return EvalError.IdentifierAlreadyDeclared;

                // Add lhs identifier to environment
                env.insert(id, venv.ObjectVal {.Undefined = {}});

                // Evaluate assignment expression if exists
                // TODO: Try to manually evaluate the assignment expression to fix
                // declare x = 10
                // { declare x = x + 1 }
                // bug
                switch (expr.*) {
                    .AssignExpr => |*exp| _ = try evalAssignExpr(exp, env),
                    .Lval => {},
                    else => unreachable // Sensible here
                }
            }

            return StmtReturn {.NoReturn = {}};
        },
        .PrintStmt => |exprs| {
            for (exprs) |expr| {
                // Evaluate expression and print resulting literal
                // TODO: Use parameterized writer and remove unreachable
                const res: ast.Lit = try evalExpr(expr, env);
                std.io.getStdOut().writer().print("{} ", .{res}) catch unreachable;
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
            const res: bool = switch (try evalExpr(stmt.cond, env)) {
                .Int, .Callable, .Void => return TypeError.MismatchedType,
                .Bool => |b| b,
            };

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
                const res: bool = switch (try evalExpr(stmt.cond, env)) {
                    .Int, .Callable, .Void => return TypeError.MismatchedType,
                    .Bool => |b| b,
                };

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
        .ReturnStmt => |expr| return StmtReturn {.Return = try evalExpr(expr, env)},
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
