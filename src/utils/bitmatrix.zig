const std = @import("std");
const bitmap = @import("bitmap.zig");

pub fn BitMatrixSize(comptime T: type) type {
    return struct {
        const Self = @This();

        rows: usize,
        columns: usize,
        row_len: usize,
        buffer: []T,

        pub fn init(mem: std.mem.Allocator, rows: usize, y: usize) !*Self {
            const bm = try mem.create(Self);

            const atom_bits = @typeInfo(T).int.bits;

            // Round row bits up to the nearest multiple of our atom bits
            // For example, if atom is a u64, then we round 32 up to 64,
            // or 110 to 128.
            const roundy = (y + (atom_bits - 1)) & ~@as(usize, (atom_bits - 1));

            // Calculate the number of atoms each row needs.
            const atoms_per_row = roundy / atom_bits;

            const buff_size = rows * atoms_per_row;

            const buff = try mem.alloc(T, buff_size);
            @memset(buff, 0);
            bm.* = .{ .rows = rows, .columns = y, .row_len = atoms_per_row, .buffer = buff };
            return bm;
        }

        pub fn set(self: *const Self, row: usize, column: usize) void {
            const r = row * self.row_len;
            const bits = @typeInfo(T).int.bits;
            const c = column / bits;
            const bit = @as(T, 1) << @intCast(@mod(column, bits));
            self.buffer[r + c] |= bit;
        }

        pub fn isSet(self: Self, x: usize, y: usize) bool {
            const row = x * self.row_len;
            const bits = @typeInfo(T).int.bits;
            const column = y / bits;
            const bit = @as(T, 1) << @intCast(@mod(y, bits));
            return (self.buffer[row + column] & bit) == bit;
        }

        pub fn getRow(self: Self, x: usize) bitmap.BitMapSized(T) {
            const row = x * self.row_len;
            const slice = self.buffer[row..(row + self.row_len)];
            return bitmap.BitMapSized(T).initShared(self.columns, slice);
        }

        pub fn deinit(self: *const Self, mem: std.mem.Allocator) void {
            mem.free(self.buffer);
            mem.destroy(self);
        }
    };
}

pub const BitMatrix = BitMatrixSize(u8);

test "bit64" {
    const alloc = std.testing.allocator;

    const bm64 = try BitMatrixSize(u64).init(alloc, 64, 64);
    defer bm64.deinit(alloc);

    // Number of rows * Atoms per row
    try std.testing.expectEqual(64, bm64.buffer.len);

    const bm8 = try BitMatrixSize(u8).init(alloc, 64, 64);
    defer bm8.deinit(alloc);
    try std.testing.expectEqual(64 * (64 / 8), bm8.buffer.len);
}

test "can allocate" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    try std.testing.expectEqual(8, matrix.rows);
    try std.testing.expectEqual(8, matrix.columns);
}

test "buffer is rounded up" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 7);
    defer matrix.deinit(alloc);

    try std.testing.expectEqual(8, matrix.buffer.len);
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

test "get row as bitmap" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 2048);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    matrix.set(0, 1);
    matrix.set(1, 4);
    matrix.set(1, 5);

    const bm = matrix.getRow(0);
    try std.testing.expectEqual(2048, bm.getBits());
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(1));

    const bm2 = matrix.getRow(1);
    try std.testing.expectEqual(2048, bm2.getBits());
    try std.testing.expect(bm2.isSet(4));
    try std.testing.expect(bm2.isSet(5));
}

test "wtf" {
    const alloc = std.testing.allocator;

    // 3 rows, 1 column
    const matrix = try BitMatrix.init(alloc, 3, 1);
    defer matrix.deinit(alloc);

    // 3 rows * atoms/row (u8)
    try std.testing.expectEqual(3, matrix.buffer.len);

    matrix.set(0, 0);
    matrix.set(2, 0);
}
