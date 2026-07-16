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

        const pe = parser.ParseError;
        const ee = interpreter.EvalError;
        _ = ee;
        const error_message: []const u8 = switch (self.err) {
            pe.ErrorOrIllegalToken => "ERROR or ILLEGAL token",
            pe.ExpectedAngleClose => "Expected '>'",
            pe.ExpectedExpression => "Expected expression",
            pe.ExpectedIdentifier => "Expected identifier",
            pe.ExpectedIf => "Expected 'if'",
            pe.ExpectedLineBreak => "Expected line break",
            pe.ExpectedPClose => "Expected ')'",
            pe.ExpectedPOpen => "Expected '('",
            pe.ExpectedPrint => "Expected 'print'",
            pe.ExpectedStatement => "Expected statement",
            pe.ExpectedTokenOrEOF => "Expected token or EOF",
            pe.ExpectedWhile => "Expected 'while'",

            else => unreachable

        };

        // TODO: Implement bufPrint or allocPrint to properly pad location
        try writer.print("({f}) {s}", .{self.location, error_message});
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
