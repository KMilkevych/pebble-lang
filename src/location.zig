const std = @import("std");

pub const Location = struct {
    file: []const u8,
    line: usize,
    column: usize,

    pub fn format(
        self: Location,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("({s}: l.{} c.{})", .{self.file, self.line, self.column});
    }
};

pub const LocationRange = struct {
    from: Location,
    to: Location,

    pub fn format(
        self: LocationRange,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("{} - {}", .{self.from, self.to});
    }
};
