const std = @import("std");
const BitMap = std.DynamicBitSetUnmanaged;

pub const BitMatrix = struct {
    const Self = @This();

    rows: usize,
    columns: usize,
    buffer: []BitMap,

    pub fn init(mem: std.mem.Allocator, rows: usize, y: usize) !*Self {
        const bm = try mem.create(Self);

        const bitmap_array = try mem.alloc(BitMap, rows);
        for (0..rows) |i| {
            bitmap_array[i] = try BitMap.initEmpty(mem, y);
        }

        bm.* = .{ .rows = rows, .columns = y, .buffer = bitmap_array };
        return bm;
    }

    pub fn set(self: *const Self, row: usize, column: usize) void {
        self.buffer[row].set(column);
    }

    pub fn isSet(self: Self, x: usize, y: usize) bool {
        return self.buffer[x].isSet(y);
    }

    // Get all of the Y values for a given X
    pub fn getColumn(self: Self, x: usize) BitMap {
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
        for (self.buffer) |*slice| {
            slice.unsetAll();
        }
    }

    pub fn deinit(self: *const Self, mem: std.mem.Allocator) void {
        for (self.buffer) |*slice| {
            slice.deinit(mem);
        }
        mem.free(self.buffer);
        mem.destroy(self);
    }
};

pub const SymmetricMatrix = struct {
    const Self = @This();

    matrix: *BitMatrix,

    pub fn init(allocator: std.mem.Allocator, rows: usize, cols: usize) !*Self {
        const self = try allocator.create(Self);
        self.matrix = try BitMatrix.init(allocator, rows, cols);
        return self;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.matrix.deinit(allocator);
        allocator.destroy(self);
    }

    fn normalizeCoords(row: usize, col: usize) struct { usize, usize } {
        return if (row <= col) .{ row, col } else .{ col, row };
    }

    pub fn set(self: *Self, row: usize, col: usize) void {
        const coords = normalizeCoords(row, col);
        self.matrix.set(coords[0], coords[1]);
    }

    pub fn unset(self: *Self, row: usize, col: usize) void {
        const coords = normalizeCoords(row, col);
        self.matrix.unset(coords[0], coords[1]);
    }

    pub fn isSet(self: *Self, row: usize, col: usize) bool {
        const coords = normalizeCoords(row, col);
        return self.matrix.isSet(coords[0], coords[1]);
    }
};

test "set both directions" {
    const alloc = std.testing.allocator;

    const matrix = try SymmetricMatrix.init(alloc, 8, 8);
    defer matrix.deinit(alloc);

    matrix.set(2, 3);
    try std.testing.expect(matrix.isSet(3, 2));
}

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

test "bit64" {
    const alloc = std.testing.allocator;

    const bm64 = try BitMatrix.init(alloc, 64, 64);
    defer bm64.deinit(alloc);

    // Number of rows * Atoms per row
    try std.testing.expectEqual(64, bm64.buffer.len);
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
    try std.testing.expectEqual(2048, bm.bit_length);
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(1));

    const bm2 = matrix.getColumn(1);
    try std.testing.expectEqual(2048, bm2.bit_length);
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
        [_]u16{ 0, 0 },
        [_]u16{ 1, 8 },
        [_]u16{ 2, 2 },
        [_]u16{ 3, 10 },
        [_]u16{ 4, 7 },
        [_]u16{ 15, 15 },
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
        [_]usize{ 0, matrix.columns - 1 },
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
