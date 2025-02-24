const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const venv = @import("env.zig");

const std = @import("std");

pub fn main() !void {

    // Allocate and read command line arguments
    var argsIterator: std.process.ArgIterator = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer argsIterator.deinit();

    // Decide if we should compile or launch interactive shell
    if (argsIterator.skip()) {

        // Read command argument
        if (argsIterator.next()) |cmd| {

            if (std.mem.eql(u8, cmd, "compile")) {
                if (argsIterator.next()) |file| try compile(file)
                else try std.io.getStdOut()
                    .writer()
                    .print("Expected filename\n", .{});
            }
            else if (std.mem.eql(u8, cmd, "interactive")) {
                try interactive();
            }
            else try std.io.getStdOut()
                .writer()
                .print("Unrecognized command: {s}\n", .{cmd});
        }
    }

}

fn compile(loc: [:0]const u8) !void {

    var file = try std.fs.cwd().openFile(loc, .{});
    defer file.close();

    var env: venv.Env = venv.Env.new(std.heap.page_allocator);
    defer env.deinit();

    // Read the whole file
    const input: []const u8 = try file.reader()
        .readAllAlloc(std.heap.page_allocator, 4096 * 4096);

    // Perform lexing parsing and interpreting...
    var lxr: lexer.Lexer = lexer.Lexer.new(input, std.heap.page_allocator);
    var tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit();

    // DEBUG: Prin tokens
    // for (tokens.items) |tok| std.debug.print("{}\n", .{tok});

    // Create parser
    var prsr: parser.Parser = parser.Parser.new(tokens, std.heap.page_allocator);

    // Parse all statements into a procedure
    const proc: ast.Proc = try prsr.parseProcedure();
    // std.debug.print("PROCEDURE: {}\n", .{proc});

    // Evaluate all
    for (proc.stmts) |stmt| try interpreter.evalStmt(stmt, &env);

}

fn interactive() !void {
    const outw = std.io.getStdOut().writer();
    const inr = std.io.getStdIn().reader();

    try outw.print("REPL Shell\n", .{});

    var env: venv.Env = venv.Env.new(std.heap.page_allocator);
    defer env.deinit();

    while (true) {
        try outw.print("> ", .{});

        const input: []u8 = (try inr.readUntilDelimiterOrEofAlloc(std.heap.page_allocator, '\n', 1024)).?;

        var lxr: lexer.Lexer = lexer.Lexer.new(input, std.heap.page_allocator);
        var tokens = lxr.lex();
        defer tokens.deinit();

        var prsr: parser.Parser = parser.Parser.new(tokens, std.heap.page_allocator);
        const tree: ast.Stmt = prsr.parseStmt() catch |err| {
            try outw.print("{}\n", .{err});
            continue;
        };
        defer tree.destroyAll(std.heap.page_allocator);

        const res: ?ast.Lit = blk: switch (tree) {
            .ExprStmt => |exp| interpreter.evalExpr(exp, &env) catch |err| {
                try outw.print("{}\n", .{err});
                break :blk null;
            },
            else => {
                interpreter.evalStmt(tree, &env) catch |err| {
                    try outw.print("{}\n", .{err});
                    break :blk null;
                };
                break :blk null;
            },
        };

        // try outw.print("{}\n", .{tree.*});
        if (res) |r| try outw.print("=> {}\n", .{r});
    }
}
