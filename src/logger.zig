const std = @import("std");
const ArrayList = std.ArrayList;

const lexer = @import("lexer.zig");
const parser = @import("parser.zig");
const interpreter = @import("interpreter.zig");

const location = @import("location.zig");

// Accumulate all the errors...
pub const Error = parser.ParseError || interpreter.EvalError;

// Error-information which includes location and description
pub const ErrorInfo = struct {
    err: Error,
    location: location.LocationRange,
    description: []const u8,

    pub fn format(
        self: ErrorInfo,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("ERROR {}: {s} | {f}\n", .{self.err, self.description, self.location});
    }
};


// Logger to store all accumulated errors...
pub const Logger = struct {

    const Self: type = @This();

    errors: ArrayList(ErrorInfo),
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .errors = ArrayList(ErrorInfo).empty,
            .allocator = allocator,
        };
    }

    pub fn destroyAll(self: *Self) void {
        self.errors.deinit(self.allocator);
    }

    pub fn logError(self: *Self, err: ErrorInfo) void {
        self.errors.append(self.allocator, err) catch unreachable;
    }


};
