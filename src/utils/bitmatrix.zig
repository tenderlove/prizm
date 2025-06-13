const std = @import("std");
const bitmap = @import("bitmap.zig");

pub fn BitMatrixSize(comptime T: type) type {
    return struct {
        const Self = @This();

        rows: usize,
        columns: usize,
        buffer: []*bitmap.BitMapSized(T),

        pub fn init(mem: std.mem.Allocator, rows: usize, y: usize) !*Self {
            const bm = try mem.create(Self);

            const bitmap_array = try mem.alloc(*bitmap.BitMapSized(T), rows);
            for (0..rows) |i| {
                bitmap_array[i] = try bitmap.BitMapSized(T).initEmpty(mem, y);
            }

            bm.* = .{ .rows = rows, .columns = y, .buffer = bitmap_array };
            return bm;
        }

        pub fn set(self: *const Self, row: usize, column: T) void {
            self.buffer[row].set(column);
        }

        pub fn isSet(self: Self, x: usize, y: T) bool {
            return self.buffer[x].isSet(y);
        }

        // Get all of the Y values for a given X
        pub fn getColumn(self: Self, x: usize) *bitmap.BitMapSized(T) {
            return self.buffer[x];
        }

        pub fn count(self: Self) usize {
            var n: usize = 0;
            for (self.buffer) |slice| {
                n += slice.count();
            }
            return n;
        }

        pub fn clear(self: Self) void {
            for (self.buffer) |slice| {
                slice.unsetAll();
            }
        }

        pub fn deinit(self: *const Self, mem: std.mem.Allocator) void {
            for (self.buffer) |slice| {
                slice.deinit(mem);
            }
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

pub const BitMatrix = BitMatrixSize(u64);
pub const BitMatrix3D = BitMatrix3DSize(u8);

test "popcount" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);
    try std.testing.expectEqual(1, matrix.count());
    matrix.set(0, 1);
    try std.testing.expectEqual(2, matrix.count());
    matrix.set(0, 1);
    try std.testing.expectEqual(2, matrix.count());
    matrix.clear();
    try std.testing.expectEqual(0, matrix.count());
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
    try std.testing.expectEqual(64, bm8.buffer.len);
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

    for (matrix.buffer) |plane| {
        var itr = plane.iterator(.{});
        while (itr.next()) |_| {
            try std.testing.expect(false);
        }
    }
}

test "bit iterator one" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 16, 16);
    defer matrix.deinit(alloc);

    matrix.set(0, 0);

    var points: usize = 0;

    for (matrix.buffer, 0..) |plane, x| {
        var itr = plane.iterator(.{});
        while (itr.next()) |y| {
            try std.testing.expectEqual(0, x);
            try std.testing.expectEqual(0, y);
            points += 1;
        }
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

    var points: usize = 0;
    for (matrix.buffer, 0..) |plane, x| {
        var itr = plane.iterator(.{});
        while (itr.next()) |y| {
            try std.testing.expectEqual(list[points][0], x);
            try std.testing.expectEqual(list[points][1], y);
            points += 1;
        }
    }

    try std.testing.expectEqual(list.len, points);
}

test "non power of 2 matrix" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 25, 25);
    defer matrix.deinit(alloc);

    for (matrix.buffer) |ys| {
        var y_itr = ys.iterator(.{});
        while (y_itr.next()) |_| {
            try std.testing.expect(false);
        }
    }
    try std.testing.expect(true);
}

test "last bit on first of many planes" {
    const alloc = std.testing.allocator;

    const matrix = try BitMatrix.init(alloc, 2, 8);
    defer matrix.deinit(alloc);

    matrix.set(0, matrix.columns - 1);

    const list = [_][2]usize{
        [_]usize{0, matrix.columns - 1},
    };

    var points: usize = 0;
    for (matrix.buffer, 0..) |plane, x| {
        var itr = plane.iterator(.{});
        while (itr.next()) |y| {
            try std.testing.expectEqual(list[points][0], x);
            try std.testing.expectEqual(list[points][1], y);
            points += 1;
        }
    }

    try std.testing.expectEqual(list.len, points);
}
