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
};


// Logger to store all accumulated errors...
pub const Logger = struct {

    const Self: type = @This();

    errors: ArrayList(ErrorInfo),
    allocator: std.mem.Allocator,

    pub fn new(allocator: std.mem.Allocator) Self {
        return .{
            .errors = ArrayList(ErrorInfo).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn logError(self: Self, err: ErrorInfo) void {
        self.errors.append(err);
    }



};
