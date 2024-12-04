const std = @import("std");

pub const BitMatrix = struct {
    x: usize,
    y: usize,
    row_len: usize,
    buffer: []u8,

    pub fn init(mem: std.mem.Allocator, x: usize, y: usize) !*BitMatrix {
        const bm = try mem.create(BitMatrix);

        // x and y are in number of bits, so lets convert to bytes
        // since the buffer will be u8.
        const roundx = (x + 7) & ~@as(usize, 7);
        const roundy = (y + 7) & ~@as(usize, 7);
        const rowlen = roundx / 8;

        const buff_size = rowlen + (roundy / 8);

        const buff = try mem.alloc(u8, buff_size);
        @memset(buff, 0);
        bm.* = .{ .x = x, .y = y, .row_len = rowlen, .buffer = buff };
        return bm;
    }

    pub fn set(self: *const BitMatrix, x: usize, y: usize) void {
        const row = x * self.row_len;
        const column = y / 8;
        const bit = @as(u8, 1) << @as(u3, @intCast(@mod(y, 8)));
        self.buffer[row + column] |= bit;
    }

    pub fn isSet(self: *const BitMatrix, x: usize, y: usize) bool {
        const row = x * self.row_len;
        const column = y / 8;
        const bit = @as(u8, 1) << @as(u3, @intCast(@mod(y, 8)));
        return (self.buffer[row + column] & bit) == bit;
    }

    pub fn deinit(self: *const BitMatrix, mem: std.mem.Allocator) void {
        mem.free(self.buffer);
        mem.destroy(self);
    }
};

test "can allocate" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    try std.testing.expectEqual(8, matrix.x);
    try std.testing.expectEqual(8, matrix.y);
}

test "buffer is rounded up" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 7);
    defer matrix.deinit(alloc);

    try std.testing.expectEqual((8 / 8) + (8 / 8), matrix.buffer.len);
}

test "set x y" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    try std.testing.expect(matrix.isSet(0, 0));
    try std.testing.expect(!matrix.isSet(0, 1));
}

test "set x y big" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 2048);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    matrix.set(7, 1024);
    try std.testing.expect(matrix.isSet(0, 0));
    try std.testing.expect(matrix.isSet(7, 1024));
    try std.testing.expect(!matrix.isSet(0, 1));
}
