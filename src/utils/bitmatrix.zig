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

        // Get all of the Y values for a given X
        pub fn getColumn(self: Self, x: usize) bitmap.BitMapSized(T) {
            const row = x * self.row_len;
            const slice = self.buffer[row..(row + self.row_len)];
            return bitmap.BitMapSized(T).initShared(self.columns, slice);
        }

        const Iterator = struct {
            y_value_plane: T,
            matrix: Self,
            plane_index: usize = 0,
            x: usize = 0,
            y: usize = 0,

            const Pair = struct { x: usize, y: usize };

            pub fn next(self: *Iterator) ?Pair{
                const atom_bits = @typeInfo(T).int.bits;

                if (self.plane_index >= self.matrix.buffer.len) {
                    return null;
                }

                while (self.y_value_plane == 0) {
                    // Either increment y by the number of atom bits, or
                    // round up to the bit size.
                    if (@mod(self.y, atom_bits) == 0) {
                        self.y += atom_bits;
                    } else {
                        self.y = (self.y + (atom_bits - 1)) & ~@as(usize, (atom_bits - 1));
                    }

                    if (self.y / atom_bits == self.matrix.row_len) {
                        self.y = 0;
                        self.x += 1;
                    }

                    if (self.x == self.matrix.rows) return null;

                    self.plane_index += 1;
                    self.y_value_plane = self.matrix.buffer[self.plane_index];
                }

                while (self.y_value_plane & 0x1 == 0) {
                    self.y_value_plane >>= 1;
                    self.y += 1;
                }

                const v = Pair { .x = self.x, .y = self.y };

                self.y += 1;

                if (@mod(self.y, atom_bits) == 0) {
                    self.plane_index += 1;
                    if (self.plane_index < self.matrix.buffer.len) {
                        self.y_value_plane = self.matrix.buffer[self.plane_index];
                    }
                } else {
                    self.y_value_plane >>= 1;
                }

                return v;
            }
        };

        pub fn iter(self: Self) Iterator {
            return .{
                .y_value_plane = self.buffer[0],
                .matrix = self
            };
        }

        pub fn popCount(self: Self) usize {
            var count: usize = 0;
            for (self.buffer) |slice| {
                count += @popCount(slice);
            }
            return count;
        }

        pub fn clear(self: Self) void {
            @memset(self.buffer, 0);
        }

        pub fn deinit(self: *const Self, mem: std.mem.Allocator) void {
            mem.free(self.buffer);
            mem.destroy(self);
        }
    };
}

pub fn BitMatrix3DSize(comptime T: type) type {
    return struct {
        const Self = @This();
        const AtomBits = @typeInfo(T).int.bits;

        xbits: usize,
        ybits: usize,
        zbits: usize,
        buffer: []T,

        pub fn init(mem: std.mem.Allocator, xbits: usize, ybits: usize, zbits: usize) !*Self {
            const bm = try mem.create(Self);

            const roundx = (xbits + (AtomBits - 1)) & ~@as(usize, (AtomBits - 1));
            const roundy = (ybits + (AtomBits - 1)) & ~@as(usize, (AtomBits - 1));
            const roundz = (zbits + (AtomBits - 1)) & ~@as(usize, (AtomBits - 1));

            const alloc = (roundx * roundy * roundz) / AtomBits;

            const buff = try mem.alloc(T, alloc);
            @memset(buff, 0);

            bm.* = .{ .xbits = roundx, .ybits = roundy, .zbits = roundz, .buffer = buff };
            return bm;
        }

        pub fn isSet(self: Self, x: usize, y: usize, z: usize) !bool {
            if (x >= self.xbits) return error.ArgumentError;
            if (y >= self.ybits) return error.ArgumentError;
            if (z >= self.zbits) return error.ArgumentError;

            const xoffset = ((self.ybits + self.zbits) / AtomBits) * x;
            const yoffset = ((self.zbits) / AtomBits) * y;
            const zoffset = y / AtomBits;
            const bit = @as(T, 1) << @intCast(@mod(z, AtomBits));
            return (self.buffer[xoffset + yoffset + zoffset] & bit) == bit;
        }

        pub fn set(self: *const Self, x: usize, y: usize, z: usize) !void {
            if (x >= self.xbits) return error.ArgumentError;
            if (y >= self.ybits) return error.ArgumentError;
            if (z >= self.zbits) return error.ArgumentError;

            const xoffset = ((self.ybits + self.zbits) / AtomBits) * x;
            const yoffset = ((self.zbits) / AtomBits) * y;
            const zoffset = y / AtomBits;
            const bit = @as(T, 1) << @intCast(@mod(z, AtomBits));
            self.buffer[xoffset + yoffset + zoffset] |= bit;
        }

        pub fn deinit(self: *const Self, mem: std.mem.Allocator) void {
            mem.free(self.buffer);
            mem.destroy(self);
        }
    };
}

pub const BitMatrix = BitMatrixSize(u8);
pub const BitMatrix3D = BitMatrix3DSize(u8);

test "popcount" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    try std.testing.expectEqual(1, matrix.popCount());
    matrix.set(0, 1);
    try std.testing.expectEqual(2, matrix.popCount());
    matrix.set(0, 1);
    try std.testing.expectEqual(2, matrix.popCount());
    matrix.clear();
    try std.testing.expectEqual(0, matrix.popCount());
}

test "3d matrix init" {
    const alloc = std.testing.allocator;

    const bm64 = try BitMatrix3D.init(alloc, 8, 8, 8);
    defer bm64.deinit(alloc);
}

test "3d matrix set" {
    const alloc = std.testing.allocator;

    const bm3d = try BitMatrix3D.init(alloc, 8, 8, 8);
    defer bm3d.deinit(alloc);
    try std.testing.expect(!(try bm3d.isSet(1, 1, 0)));
    try bm3d.set(1, 1, 0);
    try bm3d.set(7, 7, 7);
    try std.testing.expect(try bm3d.isSet(1, 1, 0));
    try std.testing.expect(try bm3d.isSet(7, 7, 7));
}

test "3d matrix not symmetrical" {
    const alloc = std.testing.allocator;

    const bm3d = try BitMatrix3D.init(alloc, 8, 8, 16);
    defer bm3d.deinit(alloc);
    try std.testing.expect(!(try bm3d.isSet(1, 1, 0)));
    try bm3d.set(1, 1, 0);
    try bm3d.set(7, 7, 7);
    try bm3d.set(7, 7, 15);
    try std.testing.expect(try bm3d.isSet(1, 1, 0));
    try std.testing.expect(try bm3d.isSet(7, 7, 7));
    try std.testing.expect(try bm3d.isSet(7, 7, 15));
}

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

    const bm = matrix.getColumn(0);
    try std.testing.expectEqual(2048, bm.getBits());
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(1));

    const bm2 = matrix.getColumn(1);
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

test "bit iterator empty" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    //matrix.set(0, 0);
    //matrix.set(2, 0);

    var itr = matrix.iter();
    while (itr.next()) |_| {
        try std.testing.expect(false);
    }
}

test "bit iterator one" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 16, 16);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);

    var itr = matrix.iter();
    var points: usize = 0;
    while (itr.next()) |point| {
        try std.testing.expectEqual(0, point.x);
        try std.testing.expectEqual(0, point.y);
        points += 1;
    }
    try std.testing.expectEqual(1, points);
}

test "bit iterator many planes" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 16, 16);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    matrix.set(1, 8);
    matrix.set(2, 2);
    matrix.set(3, 10);
    matrix.set(4, 7);
    matrix.set(15, 15);

    const list = [_][2]u16{
        [_]u16{0, 0},
        [_]u16{1, 8},
        [_]u16{2, 2},
        [_]u16{3, 10},
        [_]u16{4, 7},
        [_]u16{15, 15},
    };

    var itr = matrix.iter();
    var points: usize = 0;
    while (itr.next()) |point| {
        try std.testing.expectEqual(list[points][0], point.x);
        try std.testing.expectEqual(list[points][1], point.y);
        points += 1;
    }
    try std.testing.expectEqual(list.len, points);
}

test "non power of 2 matrix" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 25, 25);
    defer matrix.deinit(alloc);

    var itr = matrix.iter();
    while (itr.next()) |point| {
        try std.testing.expectEqual(0, point.x);
        try std.testing.expectEqual(0, point.y);
        try std.testing.expect(false);
    }
    try std.testing.expect(true);
}
