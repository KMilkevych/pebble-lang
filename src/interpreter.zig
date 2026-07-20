const ast = @import("ast.zig");
const venv = @import("env.zig");
const loc = @import("location.zig");
const log = @import("logger.zig");

const std = @import("std");

var writer: *std.Io.Writer = undefined;

pub fn setWriter(outwriter: *std.Io.Writer) void {
    writer = outwriter;
}

// Error types
pub const TypeError = error{
    MismatchedType,
    NotIdentifier,
};
pub const ArithmeticError = error{DivisionByZero};
pub const ValueError = error{
    UndefinedVariable,
    UnexpectedVoidValue,
    IndexOutOfBounds,
    InvalidSize,
    InvalidProperty,
};
pub const SemanticError = error{
    IdentifierAlreadyDeclared,
    UnexpectedBreak,
    UnexpectedContinue,
    UnexpectedReturn,
    NotCallable,
    WrongArgCount,
    InvalidUpcall,
    NotList,
    ReadOnlyProperty,
};

pub const EvalError = TypeError || ArithmeticError || ValueError || SemanticError;

pub const StmtReturn: type = union(enum) {
    NoReturn: void,
    Continue: void,
    Break: void,
    Return: ast.Lit,
};

inline fn i64asf64(in: i64) f64 {
    return @as(f64, @floatFromInt(in));
}

pub const Interpreter = struct {

    const Self: type = @This();

    writer: *std.Io.Writer,
    logger: *log.Logger,

    pub fn new(stdoutWriter: *std.Io.Writer, errLogger: *log.Logger) Self {
        return .{
            .writer = stdoutWriter,
            .logger = errLogger
        };
    }


    // Functions
    pub fn evalLval(self: *Self, lval: ast.Lval, env: *venv.Env, location: loc.LocationRange) EvalError!ast.Lit {

        const lit: ast.Lit = sw: switch (lval) {
            .Var => |v| {

                // Lookup in environment
                if (!env.isDeclaredGlobal(v)) return ValueError.UndefinedVariable;
                const value: venv.ObjectVal = env.lookup(v).?;

                // Evalue to a literal based on value
                break :sw switch (value) {
                    .Var => |val| switch (val) {
                        .Int => |ival| ast.Lit{ .Int = ival },
                        .Float => |fval| ast.Lit{ .Float = fval },
                        .Bool => |bval| ast.Lit{ .Bool = bval },
                        .Void => {
                            self.logger.logError(ValueError.UnexpectedVoidValue, location);
                            return ValueError.UnexpectedVoidValue;
                        },
                        .Callable => |fun| ast.Lit{ .Callable = fun },
                        .List => |lst| ast.Lit{ .List = lst.makeReference() },
                        .String => |str| ast.Lit {.String = env.allocator.dupe(u8, str) catch unreachable},
                        .Type => |tp| ast.Lit{ .Type = tp },
                    },
                    .Undefined => {
                        self.logger.logError(EvalError.UndefinedVariable, location);
                        return EvalError.UndefinedVariable;
                    }
                };
            },
            .ListIndex => |l| {

                // Evaluate identifier (should lookup in environment..)
                var id = try self.evalExpr(l.id, env);
                defer id.destroyAll(env.allocator);
                const list: *ast.List = switch (id) {
                    .List => |lst| lst,
                    .Int, .Float, .Bool, .Type, .Void, .Callable, .String => {
                        self.logger.logError(EvalError.NotList, l.id.location);
                        return EvalError.NotList;
                    }
                };
                // defer list.destroyAll(env.allocator);

                // Evaluate index
                var idx = try self.evalExpr(l.idx, env);
                defer idx.destroyAll(env.allocator);
                const index: i64 = switch (idx) {
                    .Int => |i| i,
                    .Bool, .Float, .Type, .Void, .Callable, .String, .List => {
                        self.logger.logError(EvalError.MismatchedType, l.idx.location);
                        return EvalError.MismatchedType;
                    }
                };

                // Make sure index is within bounds
                if (index < 0 or index >= list.len) {
                    self.logger.logError(EvalError.IndexOutOfBounds, l.idx.location);
                    return EvalError.IndexOutOfBounds;
                }

                // Lookup by index and return
                const lit: ast.Lit = list.items[@as(usize, @intCast(index))];
                break :sw switch (lit) {
                    .List => |lst| ast.Lit{ .List = lst.makeReference() },
                    else => lit,
                };
            },
            .PropertyAccess => |p| {

                // Evalutate property
                const id: []const u8 = switch (p.prop.expr) {
                    .Lval => |lv| switch (lv) {
                        .Var => |v| v,
                        else => {
                            self.logger.logError(EvalError.InvalidProperty, p.prop.location);
                            return EvalError.InvalidProperty;
                        }
                    },
                    else => {
                        self.logger.logError(EvalError.InvalidProperty, p.prop.location);
                        return EvalError.InvalidProperty;
                    }
                };

                // Evaluate left-hand-side
                var lhs: ast.Lit = try self.evalExpr(p.lhs, env);
                defer lhs.destroyAll(env.allocator);

                switch (lhs) {
                    .List => |l| {
                        if (std.mem.eql(u8, id, "size")) break :sw ast.Lit{ .Int = @intCast(l.len) } else {
                            self.logger.logError(EvalError.InvalidProperty, p.lhs.location);
                            return EvalError.InvalidProperty;
                        }
                    },
                    else =>  {
                        self.logger.logError(EvalError.InvalidProperty, p.lhs.location);
                        return EvalError.InvalidProperty;
                    }
                }
            },
        };

        // Create reference on evaluation
        return lit;
    }

    pub fn evalBinOpExpr(self: *Self, expr: *const ast.BinOpExpr, env: *venv.Env) EvalError!ast.Lit {

        // Evaluate operands
        var lhs = try self.evalExpr(expr.lhs, env);
        defer lhs.destroyAll(env.allocator);
        var rhs = try self.evalExpr(expr.rhs, env);
        defer rhs.destroyAll(env.allocator);

        // Evaluate expression
        return switch (expr.op) {
            .Add => switch (lhs) {
                .Int => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Int = l + r },
                    .Float => |r| ast.Lit{ .Float = i64asf64(l) + r },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .Float => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Float = l + i64asf64(r) },
                    .Float => |r| ast.Lit{ .Float = l + r },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .String => |l| switch (rhs) {
                    .String => |r| blk: {
                        var res = env.allocator.alloc(u8, l.len + r.len) catch unreachable;
                        @memmove(res[0..l.len], l);
                        @memmove(res[l.len..], r);
                        break :blk ast.Lit {.String = res};
                    },
                    .Int, .Float, .Type, .Callable, .Void, .List, .Bool => TypeError.MismatchedType
                },
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Sub => switch (lhs) {
                .Int => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Int = l - r },
                    .Float => |r| ast.Lit{ .Float = i64asf64(l) - r },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .Float => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Float = l - i64asf64(r) },
                    .Float => |r| ast.Lit{ .Float = l - r },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .String => |l| switch (rhs) {
                    .String => |r| blk: {
                        const sz = std.mem.replacementSize(u8, l, r, "");
                        const res = env.allocator.alloc(u8, sz) catch unreachable;
                        _ = std.mem.replace(u8, l, r, "", res);
                        break :blk ast.Lit {.String = res};
                    },
                    .Int, .Float, .Type, .Bool, .Callable, .Void, .List => TypeError.MismatchedType
                },
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Mul => switch (lhs) {
                .Int => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Int = l * r },
                    .Float => |r| ast.Lit{ .Float = i64asf64(l) * r },
                    .String => |r| blk: {
                        var res = env.allocator.alloc(u8, r.len * @abs(l)) catch unreachable;
                        for (0..@abs(l)) |v| {
                            @memmove(res[(v * r.len)..((v+1)*r.len)], r);
                        }
                        break :blk ast.Lit {.String = res};
                    },
                    .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
                },
                .Float => |l| switch (rhs) {
                    .Int => |r| ast.Lit{ .Float = l * i64asf64(r) },
                    .Float => |r| ast.Lit{ .Float = l * r },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .String => |l| switch (rhs) {
                    .Int => |r| blk: {
                        var res = env.allocator.alloc(u8, l.len * @abs(r)) catch unreachable;
                        for (0..@abs(r)) |v| {
                            @memmove(res[(v * l.len)..((v+1)*l.len)], l);
                        }
                        break :blk ast.Lit {.String = res};
                    },
                    .String, .Float, .Type, .Bool, .Callable, .Void, .List => TypeError.MismatchedType
                },
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .Div => switch (lhs) {
                .Int => |l| switch (rhs) {
                    .Int => |r| {
                        if (r == 0) return ArithmeticError.DivisionByZero;
                        return ast.Lit{ .Int = @divFloor(l, r) };
                    },
                    .Float => |r| {
                        if (r == 0) return ArithmeticError.DivisionByZero;
                        return ast.Lit{ .Float = i64asf64(l) / r };
                    },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .Float => |l| switch (rhs) {
                    .Int => |r| {
                        if (r == 0) return ArithmeticError.DivisionByZero;
                        return ast.Lit{ .Float = l / i64asf64(r) };
                    },
                    .Float => |r| {
                        if (r == 0) return ArithmeticError.DivisionByZero;
                        return ast.Lit{ .Float = l / r };
                    },
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .String => |l| switch (rhs) {
                    .String => |r| blk: {

                        // Building list of strings as result
                        var acc: std.ArrayList(ast.Lit) = .empty;
                        errdefer acc.deinit(env.allocator);
                        errdefer for (acc.items) |*item| item.destroyAll(env.allocator);

                        // Iterate over split parts and add to list
                        var it = std.mem.splitSequence(u8, l, r);
                        while (it.next()) |item| {
                            acc.append(env.allocator, ast.Lit {.String = env.allocator.dupe(u8, item) catch unreachable}) catch unreachable;
                        }

                        // Construct list of strings
                        const ptr = env.allocator.create(ast.List) catch unreachable;
                        ptr.* = ast.List {
                            .len = acc.items.len,
                            .items = acc.toOwnedSlice(env.allocator) catch unreachable
                        };
                        break :blk ast.Lit {.List = ptr};
                    },
                    .Int, .Float, .Type, .Callable, .Bool, .Void, .List => TypeError.MismatchedType
                },
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
            },
            .And => switch (lhs) {
                .Bool => |l| switch (rhs) {
                    .Bool => |r| ast.Lit{ .Bool = l and r },
                    .Int, .Float, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .Int, .Float, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
            },
            .Or => switch (lhs) {
                .Bool => |l| switch (rhs) {
                    .Bool => |r| ast.Lit{ .Bool = l or r },
                    .Int, .Float, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                },
                .Int, .Float, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
            },
            .Eq => switch (lhs) {
                .Bool => |l| switch (rhs) {
                    .Bool => |r| ast.Lit{ .Bool = l == r },
                    .Int, .Float, .Type, .Callable, .Void, .List, .String => ast.Lit{ .Bool = false },
                },
                .Int => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => ast.Lit{ .Bool = false },
                    .Int => |r| ast.Lit{ .Bool = l == r },
                    .Float => |r| ast.Lit{ .Bool = i64asf64(l) == r },
                },
                .Float => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => ast.Lit{ .Bool = false },
                    .Int => |r| ast.Lit{ .Bool = l == i64asf64(r) },
                    .Float => |r| ast.Lit{ .Bool = l == r },
                },
                .Callable => |l| switch (rhs) {
                    .Callable => |r| ast.Lit{ .Bool = std.meta.eql(l, r) },
                    .Int, .Float, .Bool, .Type, .Void, .List, .String => ast.Lit{ .Bool = false },
                },
                .List => |l| switch (rhs) {
                    .Int, .Float, .Bool, .Type, .Callable, .Void, .String => ast.Lit{ .Bool = false },
                    .List => |r| ast.Lit{ .Bool = std.meta.eql(l.items, r.items) },
                },
                .String => |l| switch (rhs) {
                    .String => |r| blk: {
                        if (std.mem.eql(u8, l, r)) break :blk ast.Lit {.Bool = true}
                        else break :blk ast.Lit {.Bool = false};
                    },
                    .Int, .Float, .Bool, .Type, .Callable, .Void, .List => ast.Lit {.Bool = false},
                },
                .Type => |tl| switch (rhs) {
                    .Bool, .Int, .Float, .Callable, .Void, .List, .String => ast.Lit{ .Bool = false },
                    .Type => |tr| ast.Lit{ .Bool = tl == tr },
                },
                .Void => ast.Lit{ .Bool = false },
            },
            .Lt => switch (lhs) {
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l < r },
                    .Float => |r| ast.Lit{ .Bool = i64asf64(l) < r },
                },
                .Float => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l < i64asf64(r) },
                    .Float => |r| ast.Lit{ .Bool = l < r },
                },
                .String => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .Int, .Float => TypeError.MismatchedType,
                    .String => |r| switch (std.mem.order(u8, l, r)) {
                        .lt => ast.Lit {.Bool = true},
                        else => ast.Lit {.Bool = false}
                    }
                }
            },
            .Gt => switch (lhs) {
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l > r },
                    .Float => |r| ast.Lit{ .Bool = i64asf64(l) > r },
                },
                .Float => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l > i64asf64(r) },
                    .Float => |r| ast.Lit{ .Bool = l > r },
                },
                .String => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .Int, .Float => TypeError.MismatchedType,
                    .String => |r| switch (std.mem.order(u8, l, r)) {
                        .gt => ast.Lit {.Bool = true},
                        else => ast.Lit {.Bool = false}
                    }
                }
            },
            .Lte => switch (lhs) {
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l <= r },
                    .Float => |r| ast.Lit{ .Bool = i64asf64(l) <= r },
                },
                .Float => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l <= i64asf64(r) },
                    .Float => |r| ast.Lit{ .Bool = l <= r },
                },
                .String => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .Int, .Float => TypeError.MismatchedType,
                    .String => |r| switch (std.mem.order(u8, l, r)) {
                        .lt, .eq => ast.Lit {.Bool = true},
                        else => ast.Lit {.Bool = false}
                    }
                }
            },
            .Gte => switch (lhs) {
                .Bool, .Type, .Callable, .Void, .List => TypeError.MismatchedType,
                .Int => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l >= r },
                    .Float => |r| ast.Lit{ .Bool = i64asf64(l) >= r },
                },
                .Float => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .String => TypeError.MismatchedType,
                    .Int => |r| ast.Lit{ .Bool = l >= i64asf64(r) },
                    .Float => |r| ast.Lit{ .Bool = l >= r },
                },
                .String => |l| switch (rhs) {
                    .Bool, .Type, .Callable, .Void, .List, .Int, .Float => TypeError.MismatchedType,
                    .String => |r| switch (std.mem.order(u8, l, r)) {
                        .gt, .eq => ast.Lit {.Bool = true},
                        else => ast.Lit {.Bool = false}
                    }
                }
            },
        } catch |err| {
            self.logger.logError(err, loc.LocationRange {
                .from = expr.lhs.location.from,
                .to = expr.rhs.location.to,
            });
            return err;
        };
    }

    pub fn evalUnOpExpr(self: *Self, expr: *const ast.UnOpExpr, env: *venv.Env) EvalError!ast.Lit {

        // Evaluate operand
        var lit = try self.evalExpr(expr.rhs, env);
        defer lit.destroyAll(env.allocator);

        // Evaluate operator
        return switch (expr.op) {
            .Not => switch (lit) {
                .Int, .Float, .Type, .Callable, .Void, .List, .String => {
                    self.logger.logError(TypeError.MismatchedType, expr.rhs.location);
                    return TypeError.MismatchedType;
                },
                .Bool => |val| ast.Lit{ .Bool = !val },
            },
            .Neg => switch (lit) {
                .Int => |val| ast.Lit{ .Int = -val },
                .Float => |val| ast.Lit{ .Float = -val },
                .Bool, .Type, .Callable, .Void, .List, .String => {
                    self.logger.logError(TypeError.MismatchedType, expr.rhs.location);
                    return TypeError.MismatchedType;
                }
            },
        };
    }

    pub fn evalAssignExpr(self: *Self, expr: *const ast.AssignExpr, env: *venv.Env) EvalError!ast.Lit {

        // TODO: Prevent creating another Python and actually require types and declarations

        // Make sure left-hand side is an lval
        const lval: ast.Lval = switch (expr.lhs.expr) {
            .Lval => |lv| lv,
            else => {
                self.logger.logError(EvalError.NotIdentifier, expr.lhs.location);
                return EvalError.NotIdentifier;
            }
        };

        // Evaluate right-hand side
        var rhs: ast.Lit = try self.evalExpr(expr.rhs, env);
        errdefer rhs.destroyAll(env.allocator); // NOTE: errdefer here because normally we make a reference

        // Perform assignment
        switch (lval) {
            .Var => |id| {
                if (!env.isDeclaredGlobal(id)) {
                    self.logger.logError(EvalError.UndefinedVariable, expr.lhs.location);
                    return EvalError.UndefinedVariable;
                }

                // Destroy existing value
                switch (env.lookup(id) orelse unreachable) {
                    .Undefined => {},
                    .Var => |v| v.destroyAll(env.allocator),
                }

                // Insert new value
                env.insertScoping(id, venv.ObjectVal{ .Var = rhs });
            },
            .ListIndex => |lidx| {

                // Make sure that left-hand side is a list
                // NOTE: this creates an additional reference
                var lhs = try self.evalExpr(lidx.id, env);
                defer lhs.destroyAll(env.allocator);

                var lst: *ast.List = switch (lhs) {
                    .List => |l| l,
                    .Int, .Float, .Bool, .Type, .Void, .Callable, .String => {
                        self.logger.logError(EvalError.NotList, lidx.id.location);
                        return EvalError.NotList;
                    }
                };

                // Evaluate index
                var index = try self.evalExpr(lidx.idx, env);
                defer index.destroyAll(env.allocator);
                const idx: i64 = switch (index) {
                    .Int => |i| i,
                    .Bool, .Float, .Type, .Void, .Callable, .List, .String => {
                        self.logger.logError(EvalError.MismatchedType, lidx.idx.location);
                        return EvalError.MismatchedType;
                    }
                };

                // Bounds checking
                if (idx < 0 or idx >= lst.len) {
                    self.logger.logError(EvalError.IndexOutOfBounds, lidx.idx.location);
                    return EvalError.IndexOutOfBounds;
                }

                // Destroy existing value
                switch (lst.items[@as(usize, @intCast(idx))]) {
                    .List, .Callable, .String => lst.items[@as(usize, @intCast(idx))].destroyAll(env.allocator),
                    .Int, .Float, .Type, .Bool, .Void => {},
                }

                // Perform assignment
                lst.items[@as(usize, @intCast(idx))] = rhs;
            },
            .PropertyAccess => {
                self.logger.logError(EvalError.ReadOnlyProperty, expr.lhs.location);
                return EvalError.ReadOnlyProperty; // TODO: Implement real properties
            }
        }

        return switch (rhs) {
            .List => |l| ast.Lit{ .List = l.makeReference() },
            .String => |s| ast.Lit {.String = env.allocator.dupe(u8, s) catch unreachable},
            else => rhs,
        };
    }

    pub fn evalCallExpr(self: *Self, expr: *const ast.CallExpr, env: *venv.Env, location: loc.LocationRange) EvalError!ast.Lit {

        // Evaluate identifier
        var ident = try self.evalExpr(expr.id, env);
        defer ident.destroyAll(env.allocator);
        const callable: ast.Callable = switch (ident) {
            .Callable => |fun| fun,
            .Int, .Float, .Bool, .Type, .Void, .List, .String => {
                self.logger.logError(EvalError.NotCallable, expr.id.location);
                return EvalError.NotCallable;
            }
        };

        // Create a scoped environment based on function closure
        var scopedEnv: venv.Env = callable.closure.newScoped();
        defer scopedEnv.deinit();

        // Bind all args to function parameter names
        if (callable.params.len != expr.args.len) {
            self.logger.logError(EvalError.WrongArgCount, location);
            return EvalError.WrongArgCount;
        }

        for (callable.params, expr.args) |id, arg| {

            // NOTE: Evaluating in current environment, not in closure or scoped
            const r: ast.Lit = try self.evalExpr(arg, env);

            // Bind in environment directly
            // NOTE: Can also declare and then insertScoped
            scopedEnv.insert(id, venv.ObjectVal{ .Var = r });
        }

        // Evaluate function body
        return switch (try self.evalStmt(callable.body, &scopedEnv)) {
            .NoReturn => ast.Lit{ .Void = {} },
            .Return => |lit| lit,
            .Break => {
                self.logger.logError(EvalError.UnexpectedBreak, callable.body.location);
                return EvalError.UnexpectedBreak;
            },
            .Continue => {
                self.logger.logError(EvalError.UnexpectedContinue, callable.body.location);
                return EvalError.UnexpectedContinue;
            }
        };
    }

    pub fn evalListExpr(self: *Self, exprs: []const *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {

        // Create accumulator for contents
        var acc: std.ArrayList(ast.Lit) = .empty;
        errdefer acc.deinit(env.allocator);
        errdefer for (acc.items) |*item| item.destroyAll(env.allocator);

        // Evaluate each entry while preventing callables
        for (exprs) |expr| {
            const lit: ast.Lit = try self.evalExpr(expr, env);
            switch (lit) {
                .Callable => {
                    self.logger.logError(SemanticError.InvalidUpcall, expr.location);
                    return SemanticError.InvalidUpcall;
                },
                else => acc.append(env.allocator, lit) catch unreachable,
            }
        }

        // Create a list literal
        const ptr = env.allocator.create(ast.List) catch unreachable;
        ptr.* = ast.List{
            .len = acc.items.len,
            .items = acc.toOwnedSlice(env.allocator) catch unreachable,
        };

        return ast.Lit{ .List = ptr };
    }

    pub fn evalAsExpr(self: *Self, expr: *const ast.AsExpr, env: *venv.Env) EvalError!ast.Lit {

        // Evaluate left-hand-side
        var lhs: ast.Lit = try self.evalExpr(expr.lhs, env);
        defer lhs.destroyAll(env.allocator);

        // Evaluate right-hand side to a type literal
        var tpe = try self.evalExpr(expr.as, env);
        defer tpe.destroyAll(env.allocator);

        const tp: ast.Type = switch (tpe) {
            .Type => |tp| tp,
            else => {
                self.logger.logError(EvalError.MismatchedType, loc.LocationRange {
                    .from = expr.lhs.location.from,
                    .to = expr.as.location.to
                });
                return EvalError.MismatchedType;
            }
        };

        // Evaluate type conversion
        return switch (lhs) {
            .Callable, .List, .Void, .Type, .String => {
                self.logger.logError(EvalError.MismatchedType, loc.LocationRange {
                    .from = expr.lhs.location.from,
                    .to = expr.as.location.to
                });
                return EvalError.MismatchedType;
            },
            .Bool => |b| switch (tp) {
                .Bool => ast.Lit{ .Bool = b },
                .Int => ast.Lit{ .Int = if (b) 1 else 0 },
                .Float => ast.Lit{ .Float = if (b) 1.0 else 0.0 },
            },
            .Float => |f| switch (tp) {
                .Bool => ast.Lit{ .Bool = f != 0.0 },
                .Int => ast.Lit{ .Int = @intFromFloat(f) },
                .Float => ast.Lit{ .Float = f },
            },
            .Int => |i| switch (tp) {
                .Bool => ast.Lit{ .Bool = i != 0 },
                .Float => ast.Lit{ .Float = @floatFromInt(i) },
                .Int => ast.Lit{ .Int = i },
            },
        };
    }

    pub fn evalExpr(self: *Self, expr: *const ast.Expr, env: *venv.Env) EvalError!ast.Lit {

        return switch (expr.expr) {
            .BinOpExpr => |ex| self.evalBinOpExpr(&ex, env),
            .UnOpExpr => |ex| self.evalUnOpExpr(&ex, env),

            .Lval => |lval| self.evalLval(lval, env, expr.location),

            // NOTE: For list, we implement reference counting
            // and for strings we implement clone semantics
            .Lit => |lit| switch (lit) {
                .List => |l| ast.Lit{ .List = l.makeReference() },
                .String => |s| ast.Lit {.String = env.allocator.dupe(u8, s) catch unreachable},
                else => lit,
            },

            .AssignExpr => |ex| self.evalAssignExpr(&ex, env),
            .CallExpr => |ex| self.evalCallExpr(&ex, env, expr.location),
            .ListExpr => |ex| self.evalListExpr(ex, env),
            .AsExpr => |ex| self.evalAsExpr(&ex, env)
        };
    }

    pub fn evalStmt(self: *Self, statement: ast.Stmt, env: *venv.Env) EvalError!StmtReturn {

        switch (statement.stmt) {
            .ExprStmt => |expr| {

                // Evaluate the underlying expression
                // NOTE: Inner-most return statement in CallExpr creates list reference
                // which needs to be discarded for an ExprStmt
                var lit: ast.Lit = self.evalExpr(expr, env) catch |err| {
                    self.logger.logError(err, expr.location);
                    return err;
                };
                lit.destroyAll(env.allocator);

                return StmtReturn{ .NoReturn = {} };
            },
            .DeclareStmt => |exprs| {

                // DECLARE X evaluates X = undefined
                // DECLARE X = 10 evaluates X = 10

                // Evaluate each expression one by one
                for (exprs) |expr| {

                    // Make sure the expression is an assignment expression or Lval
                    const lhs: EvalError!ast.Lval = switch (expr.expr) {
                        .Lval => |lval| lval,
                        .AssignExpr => |exp| switch (exp.lhs.expr) {
                            .Lval => |lval| lval,
                            else => blk: {
                                self.logger.logError(EvalError.NotIdentifier, exp.lhs.location);
                                break :blk EvalError.NotIdentifier;
                            },
                        },
                        else => {
                            self.logger.logError(EvalError.NotIdentifier, expr.location);
                            return EvalError.NotIdentifier;
                        }
                    };

                    switch (try lhs) {
                        .Var => |id| {

                            // Do error checking
                            if (env.isDeclaredLocal(id)) {
                                self.logger.logError(EvalError.IdentifierAlreadyDeclared, expr.location);
                                return EvalError.IdentifierAlreadyDeclared;
                            }

                            // Add lhs identifier to environment
                            env.insert(id, venv.ObjectVal{ .Undefined = {} });

                            // Evalute assignment expression
                            switch (expr.expr) {
                                .AssignExpr => |*exp| {
                                    var val: ast.Lit = try self.evalAssignExpr(exp, env);
                                    val.destroyAll(env.allocator);
                                },
                                .Lval => {},
                                else => unreachable, // Sensible here
                            }
                        },
                        .ListIndex => |li| {

                            // Make sure that id is a Var
                            const id: ast.Var = switch (li.id.expr) {
                                .Lval => |lv| switch (lv) {
                                    .Var => |v| v,
                                    .ListIndex, .PropertyAccess => {
                                        self.logger.logError(EvalError.NotIdentifier, li.id.location);
                                        return EvalError.NotIdentifier;
                                    }
                                },
                                else => {
                                    self.logger.logError(EvalError.NotIdentifier, li.id.location);
                                    return EvalError.NotIdentifier;
                                }
                            };

                            // Do error checking
                            if (env.isDeclaredLocal(id)) {
                                self.logger.logError(EvalError.IdentifierAlreadyDeclared, li.id.location);
                                return EvalError.IdentifierAlreadyDeclared;
                            }

                            // Evaluate index (size)
                            var sz = try self.evalExpr(li.idx, env);
                            defer sz.destroyAll(env.allocator);
                            const size: i64 = switch (sz) {
                                .Int => |i| i,
                                .Bool, .Float, .Type, .Callable, .Void, .List, .String => {
                                    self.logger.logError(EvalError.MismatchedType, li.idx.location);
                                    return EvalError.MismatchedType;
                                }
                            };

                            // Do size checking
                            if (size < 0) {
                                self.logger.logError(EvalError.InvalidSize, li.idx.location);
                                return EvalError.InvalidSize;
                            }

                            // Add identifier to environment
                            const items_ptr = env.allocator.alloc(ast.Lit, @as(usize, @intCast(size))) catch unreachable;
                            const ptr = env.allocator.create(ast.List) catch unreachable;

                            ptr.* = ast.List{
                                .len = @as(usize, @intCast(size)),
                                .items = items_ptr,
                            };
                            env.insert(id, venv.ObjectVal{ .Var = ast.Lit{ .List = ptr } });

                            // Evaluate right-hand side and destroy if exists
                            var val: ast.Lit = switch (expr.expr) {
                                .AssignExpr => |as| try self.evalExpr(as.rhs, env),
                                .Lval => ast.Lit{ .Void = {} },
                                else => unreachable,
                            };
                            defer val.destroyAll(env.allocator);

                            // Initialize list with values
                            var idx: i64 = 0;
                            while (idx < size) : (idx += 1) {
                                const exp: ast.AssignExpr = ast.AssignExpr{ .lhs = &ast.Expr{ .location = expr.location, .expr = ast.ExprInner{ .Lval = ast.Lval{ .ListIndex = ast.ListIndex{ .id = li.id, .idx = &ast.Expr{ .location = li.idx.location, .expr = ast.ExprInner{ .Lit = ast.Lit{ .Int = idx } } } } } } }, .rhs = &ast.Expr{ .location = expr.location, .expr = ast.ExprInner{ .Lit = val } } };

                                // Use reflection-like code to build initialization
                                var lit: ast.Lit = try self.evalAssignExpr(&exp, env);
                                lit.destroyAll(env.allocator);
                            }
                        },
                        .PropertyAccess => {
                            self.logger.logError(EvalError.InvalidProperty, expr.location);
                            return EvalError.InvalidProperty;
                        },
                    }
                }

                return StmtReturn{ .NoReturn = {} };
            },
            .PrintStmt => |exprs| {
                for (exprs) |expr| {
                    // Evaluate expression and print resulting literal
                    // TODO: Get rid of unreachable
                    var res: ast.Lit = try self.evalExpr(expr, env);
                    defer res.destroyAll(env.allocator);
                    self.writer.print("{f} ", .{res}) catch unreachable;
                }
                self.writer.print("\n", .{}) catch unreachable;
                return StmtReturn{ .NoReturn = {} };
            },
            .BlockStmt => |stmts| {
                // Start new environment
                var nestedEnv: venv.Env = env.newScoped();
                defer nestedEnv.deinit();

                // Evalute each statement with nested environment
                for (stmts) |stmt| switch (try self.evalStmt(stmt, &nestedEnv)) {
                    .NoReturn => {},
                    .Return => |lit| return StmtReturn{ .Return = lit },
                    .Break => return StmtReturn{ .Break = {} },
                    .Continue => return StmtReturn{ .Continue = {} },
                };

                return StmtReturn{ .NoReturn = {} };
            },
            .IfElseStmt => |stmt| {

                // Check condition
                var cond_res: ast.Lit = try self.evalExpr(stmt.cond, env);
                defer cond_res.destroyAll(env.allocator);

                const res: bool = switch (cond_res) {
                    .Int, .Float, .Type, .Callable, .Void, .List, .String => {
                        self.logger.logError(TypeError.MismatchedType, stmt.cond.location);
                        return TypeError.MismatchedType;
                    },
                    .Bool => |b| b,
                };

                // Construct scoped environment
                var scopedEnv = env.newScoped();
                defer scopedEnv.deinit();

                // Evaluate if or else branch
                if (res)
                    return try self.evalStmt(stmt.ifStmt, &scopedEnv)
                else if (stmt.elseStmt) |es|
                    return try self.evalStmt(es, &scopedEnv)
                else
                    return StmtReturn{ .NoReturn = {} };
            },
            .WhileStmt => |stmt| {
                while (true) {

                    // Check condition
                    var cond_lit: ast.Lit = try self.evalExpr(stmt.cond, env);
                    defer cond_lit.destroyAll(env.allocator);
                    const res: bool = switch (cond_lit) {
                        .Int, .Float, .Type, .Callable, .Void, .List, .String => {
                            self.logger.logError(TypeError.MismatchedType, stmt.cond.location);
                            return TypeError.MismatchedType;
                        },
                        .Bool => |b| b,
                    };


                    if (!res) break;

                    // Evaluate body in a scoped environment
                    var scoped: venv.Env = env.newScoped();
                    defer scoped.deinit();

                    // Evaluate body and consume break statements
                    switch (try self.evalStmt(stmt.body, &scoped)) {
                        .NoReturn => {},
                        .Return => |lit| return StmtReturn{ .Return = lit },
                        .Break => break,
                        .Continue => continue,
                    }
                }

                return StmtReturn{ .NoReturn = {} };
            },
            .BreakStmt => return StmtReturn{ .Break = {} },
            .ContinueStmt => return StmtReturn{ .Continue = {} },
            .ReturnStmt => |expr| {
                // Evaluate expression and make sure its not a Callable
                // If expr is a CallExpr, we do not make extra list reference
                const res: ast.Lit = try self.evalExpr(expr, env);
                return switch (res) {
                    .Callable => {
                        self.logger.logError(EvalError.InvalidUpcall, expr.location);
                        return EvalError.InvalidUpcall;
                    },
                    else => StmtReturn{ .Return = res },
                };
            },
            .FunDefStmt => |stmt| {

                // Insert function into environment
                if (env.isDeclaredLocal(stmt.id)) {
                    self.logger.logError(EvalError.IdentifierAlreadyDeclared, statement.location);
                    return EvalError.IdentifierAlreadyDeclared;
                }
                env.insert(stmt.id, venv.ObjectVal{ .Var = ast.Lit{ .Callable = ast.Callable{ .params = stmt.params, .body = stmt.body, .closure = env } } });

                return StmtReturn{ .NoReturn = {} };
            },
        }

        unreachable;
    }

    pub fn evalProc(self: *Self, procedure: ast.Proc, env: *venv.Env) EvalError!void {

        // Evaluate statements one by one
        for (procedure.stmts) |stmt| switch (try self.evalStmt(stmt, env)) {
            .NoReturn => {},
            .Return => |lit| {
                lit.destroyAll(env.allocator);
                self.logger.logError(EvalError.UnexpectedReturn, stmt.location);
                return EvalError.UnexpectedReturn;
            },
            .Break => {
                self.logger.logError(EvalError.UnexpectedBreak, stmt.location);
                return EvalError.UnexpectedBreak;
            },
            .Continue => {
                self.logger.logError(EvalError.UnexpectedContinue, stmt.location);
                return EvalError.UnexpectedContinue;
            },
        };
    }
};
