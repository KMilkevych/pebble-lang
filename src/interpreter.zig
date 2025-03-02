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
    UndefinedVariable
};
const SemanticError = error {
    IdentifierAlreadyDeclared,
    UnexpectedBreak,
    UnexpectedContinue,
};

pub const EvalError = TypeError || ArithmeticError || ValueError || SemanticError;

pub const StmtReturn: type = union(enum) {
    NoReturn: void,
    Continue: void,
    Break: void,
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
                    .Int => |ival| ast.Lit {.Int = ival},
                    .Bool => |bval| ast.Lit {.Bool = bval},
                    .Undefined => EvalError.UndefinedVariable,
                }
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
                .Bool => TypeError.MismatchedType,
            },
            .Bool => TypeError.MismatchedType,
        },
        .Sub => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l - r},
                .Bool => TypeError.MismatchedType,
            },
            .Bool => TypeError.MismatchedType,
        },
        .Mul => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| ast.Lit {.Int = l * r},
                .Bool => TypeError.MismatchedType,
            },
            .Bool => TypeError.MismatchedType,
        },
        .Div => switch (lhs) {
            .Int => |l| switch (rhs) {
                .Int => |r| {
                    if (r == 0) return ArithmeticError.DivisionByZero;
                    return ast.Lit {.Int = @divFloor(l, r)};
                },
                .Bool => TypeError.MismatchedType,
            },
            .Bool => TypeError.MismatchedType,
        },
        .And => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l and r},
                .Int => TypeError.MismatchedType,
            },
            .Int => TypeError.MismatchedType,
        },
        .Or => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l or r},
                .Int => TypeError.MismatchedType,
            },
            .Int => TypeError.MismatchedType,
        },
        .Eq => switch (lhs) {
            .Bool => |l| switch (rhs) {
                .Bool => |r| ast.Lit {.Bool = l == r},
                .Int => ast.Lit {.Bool = false},
            },
            .Int => |l| switch (rhs) {
                .Bool => ast.Lit {.Bool = false},
                .Int => |r| ast.Lit {.Bool = l == r}
            }
        },
        .Lt => switch (lhs) {
            .Bool => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l < r}
            }
        },
        .Gt => switch (lhs) {
            .Bool => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l > r}
            }
        },
        .Lte => switch (lhs) {
            .Bool => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool => TypeError.MismatchedType,
                .Int => |r| ast.Lit {.Bool = l <= r}
            }
        },
        .Gte => switch (lhs) {
            .Bool => TypeError.MismatchedType,
            .Int => |l| switch (rhs) {
                .Bool => TypeError.MismatchedType,
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
            .Int => TypeError.MismatchedType,
            .Bool => |val| ast.Lit { .Bool = !val },
        },
        .Neg => switch (lit) {
            .Int => |val| ast.Lit { .Int = -val },
            .Bool => TypeError.MismatchedType,
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
            env.insertScoping(id, venv.ObjectVal {.Var = venv.Value.fromLiteral(rhs)});
        }
    }

    return rhs;
}


pub fn evalExpr(expr: *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {
    return switch (expr.*) {
        .BinOpExpr => |ex| evalBinOpExpr(&ex, env),
        .UnOpExpr => |ex| evalUnOpExpr(&ex, env),
        .Lval => |lval| evalLval(lval, env),
        .Lit => |lit| lit,
        .AssignExpr => |ex| evalAssignExpr(&ex, env),
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
                const id: []const u8 = switch (try lhs) {
                    .Var => |id| id
                };

                // Do error checking
                if (env.isDeclaredLocal(id)) return EvalError.IdentifierAlreadyDeclared;

                // Add lhs identifier to environment
                env.insert(id, venv.ObjectVal {.Var = venv.Value {.Undefined = {}}});

                // Evaluate assignment expression if exists
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
                .Break => return StmtReturn {.Break = {}},
                .Continue => return StmtReturn {.Continue = {}},
            };

            return StmtReturn {.NoReturn = {}};
        },
        .IfElseStmt => |stmt| {

            // Check condition
            const res: bool = switch (try evalExpr(stmt.cond, env)) {
                .Int => return TypeError.MismatchedType,
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
                    .Int => return TypeError.MismatchedType,
                    .Bool => |b| b,
                };

                if (!res) break;

                // Evaluate body in a scoped environment
                var scoped: venv.Env = env.newScoped();
                defer scoped.deinit();

                // Evaluate body and consume break statements
                switch(try evalStmt(stmt.body, &scoped)) {
                    .NoReturn => {},
                    .Break => break,
                    .Continue => continue,
                }
            }

            return StmtReturn {.NoReturn = {}};
        },
        .BreakStmt => return StmtReturn {.Break = {}},
        .ContinueStmt => return StmtReturn {.Continue = {}},
    }

    unreachable;
}

pub fn evalProc(procedure: ast.Proc, env: *venv.Env) EvalError!void {

    // Evaluate statements one by one
    for (procedure.stmts) |stmt| switch (try evalStmt(stmt, env)) {
        .NoReturn => {},
        .Break => return EvalError.UnexpectedBreak,
        .Continue => return EvalError.UnexpectedContinue,
    };
}
