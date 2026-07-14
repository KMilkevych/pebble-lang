const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");
const lexer = @import("lexer.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const venv = @import("env.zig");

const Log = @import("logger.zig");

// const Io = std.Io;

const std = @import("std");

pub fn main(init: std.process.Init) !void {

    // Get io and gpa
    const io = init.io;
    const gpa = init.gpa;


    // Init sdtout writing
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file = std.Io.File.stdout().writer(io, &stdout_buffer);
    const stdout: *std.Io.Writer = &stdout_file.interface;

    defer stdout.flush() catch {};

    // Allocate and read command line arguments
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 2) return;

    // Decide if we want to interpret file or run interactive shell
    const cmd = args[1];
    if (std.mem.eql(u8, cmd, "run")) {

        if (args.len >= 3) {
            try run(io, gpa, stdout, args[2]);
        } else {
            try stdout.print("Expected filename\n", .{});
        }

    } else if (std.mem.eql(u8, cmd, "interactive")) {
        try interactive(io, gpa, stdout);
    } else {
        try stdout.print("Unrecognized comand: {s}\n", .{cmd});
    }


}

fn run(io: std.Io, allocator: std.mem.Allocator, stdout: *std.Io.Writer, loc: [:0]const u8) !void {

    // Open file for reading
    var file = try std.Io.Dir.cwd().openFile(io, loc, .{});
    defer file.close(io);

    // Prepare execution environment
    var env: venv.Env = venv.Env.new(allocator);
    defer env.deinit();

    // Read whole file up till 4096 * 4096 size limit
    var file_buffer: [4096]u8 = undefined;
    var file_reader: std.Io.File.Reader = file.reader(io, &file_buffer);
    const input: []const u8 = try file_reader.interface.allocRemaining(allocator, .limited(4096 * 4096));
    defer allocator.free(input);

    // Perform lexing parsing and interpreting...
    var lxr: lexer.Lexer = lexer.Lexer.new(input, loc, allocator);
    var tokens: std.ArrayList(token.Token) = lxr.lex();
    defer tokens.deinit(allocator);

    // Create parser
    var prsr: parser.Parser = parser.Parser.new(tokens, allocator, Log.Logger.new(allocator));

    // Parse all statements into a procedure
    const proc: ast.Proc = try prsr.parseProcedure();
    defer proc.destroyAll(allocator);

    // Evaluate all
    interpreter.setWriter(stdout);
    try interpreter.evalProc(proc, &env);
}

fn interactive(io: std.Io, gpa: std.mem.Allocator, out: *std.Io.Writer) !void {

    // env keeps references to each line's text for the whole session, so use a
    // session arena: all per-line allocations are freed together at exit (and
    // the leak checker in init.gpa stays quiet).
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const alloc = arena.allocator();

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_file = std.Io.File.stdin().reader(io, &stdin_buffer);
    const inr = &stdin_file.interface;

    try out.print("REPL Shell\n", .{});

    var env: venv.Env = venv.Env.new(alloc);
    defer env.deinit();

    while (true) {
        try out.print("> ", .{});
        try out.flush(); // flush so the prompt shows before we block on input

        // takeDelimiter's slice points into stdin_buffer and dies on next read;
        // dupe it because env holds references across iterations.
        const raw_line = (try inr.takeDelimiter('\n')) orelse break; // EOF / Ctrl-D
        const line = std.mem.trimEnd(u8, raw_line, "\r");
        const input = try alloc.dupe(u8, line);

        var lxr: lexer.Lexer = lexer.Lexer.new(input, "", alloc);
        var tokens = lxr.lex();
        defer tokens.deinit(alloc);

        var prsr = parser.Parser.new(tokens, alloc, Log.Logger.new(alloc));
        const tree: ast.Stmt = prsr.parseStmt() catch |err| {
            try out.print("{}\n", .{err});
            try out.flush();
            continue;
        };

        interpreter.setWriter(out);
        const res: ?ast.Lit = blk: switch (tree.stmt) {
            .ExprStmt => |exp| interpreter.evalExpr(exp, &env) catch |err| {
                try out.print("{}\n", .{err});
                break :blk null;
            },
            else => {
                _ = interpreter.evalStmt(tree, &env) catch |err| {
                    try out.print("{}\n", .{err});
                    break :blk null;
                };
                break :blk null;
            },
        };

        if (res) |r| try out.print("=> {}\n", .{r});
        try out.flush();
    }
}
