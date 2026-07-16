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
        // const ee = interpreter.EvalError;
        const te = interpreter.TypeError;
        const ae = interpreter.ArithmeticError;
        const ve = interpreter.ValueError;
        const se = interpreter.SemanticError;

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

            te.MismatchedType => "Mismatched type",
            te.NotIdentifier => "Not identifier",
            ae.DivisionByZero => "Division by zero",
            ve.UndefinedVariable => "Undefined variable",
            ve.UnexpectedVoidValue => "Unexpected void value",
            ve.IndexOutOfBounds => "Index out of bounds",
            ve.InvalidSize => "Invalid size",
            ve.InvalidProperty => "Invalid property",
            se.IdentifierAlreadyDeclared => "Identifier already declared",
            se.UnexpectedBreak => "Unexpected 'break'",
            se.UnexpectedContinue => "Unexpected 'continue'",
            se.UnexpectedReturn => "Unexpected 'return'",
            se.NotCallable => "Object not callable",
            se.WrongArgCount => "Wrong argument count",
            se.InvalidUpcall => "Invalid function up-call",
            se.NotList => "Not indexable",
            se.ReadOnlyProperty => "Property is read-only"
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

    // pub fn logError(self: *Self, err: ErrorInfo) void {
    //     self.errors.append(self.allocator, err) catch unreachable;
    // }

    pub fn logError(self: *Self, err: Error, loc: location.LocationRange) void {
        self.errors.append(self.allocator, ErrorInfo {
            .description = "",
            .err = err,
            .location = loc
        }) catch unreachable;
    }


};
