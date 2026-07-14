const std = @import("std");

pub const Location = struct {
    file: []const u8,
    line: usize,
    column: usize,

    pub fn format(
        self: Location,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("({s}: l.{d} c.{d})", .{self.file, self.line, self.column});
    }
};

pub const LocationRange = struct {
    from: Location,
    to: Location,

    pub fn format(
        self: LocationRange,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("{f} - {f}", .{self.from, self.to});
    }

    pub fn none() LocationRange {
        return LocationRange {
            .from = Location {.column = 0, .file = "", .line = 0},
            .to = Location {.column = 0, .file = "", .line = 0},
        };
    }
};
