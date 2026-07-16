const std = @import("std");

pub const Location = struct {
    file: []const u8,
    line: usize,
    column: usize,

    pub fn format(
        self: Location,
        writer: *std.Io.Writer
    ) !void {
        try writer.print("({s: >8}: l.{d: >4} c.{d: >4})", .{self.file, self.line, self.column});
    }
};

pub const LocationRange = struct {
    from: Location,
    to: Location,

    pub fn format(
        self: LocationRange,
        writer: *std.Io.Writer
    ) !void {
        try writer.print(
            "{s: >8}: l{d}.c{d} - l{d}.c{d}",
            .{
                self.from.file,
                self.from.line + 1,
                self.from.column + 1,
                self.to.line + 1,
                self.to.column + 1,
            }
        );
    }

    pub fn none() LocationRange {
        return LocationRange {
            .from = Location {.column = 0, .file = "", .line = 0},
            .to = Location {.column = 0, .file = "", .line = 0},
        };
    }
};
