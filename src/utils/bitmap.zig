const std = @import("std");

pub const BitMap = struct {
    bits: usize,
    single: u64,
    many: ?[]u64,

    pub fn init(mem: std.mem.Allocator, bits: usize) !*BitMap {
        const bm = try mem.create(BitMap);
        bm.* = .{ .bits = bits, .single = 0, .many = null };
        if (bits > 64) {
            const planes = ((bits + 63) / 64) + 1;
            const memory = try mem.alloc(u64, planes);
            @memset(memory, 0);
            bm.*.many = memory;
        }
        return bm;
    }

    pub fn setBit(self: *BitMap, bit: u64) void {
        if (self.bits > 64) {
            const plane = bit / 64;
            const theBit: u6 = @intCast(@mod(bit, 64));
            self.many.?[plane] |= (@as(u64, 1) << theBit);
        } else {
            self.single |= (@as(u64, 1) << @as(u6, @intCast(bit)));
        }
    }

    const SetBitsIterator = struct {
        bit_index: usize,
        plane_index: usize,
        current_plane: u64,
        bm: *BitMap,

        pub fn next(self: *SetBitsIterator) ?usize {
            while (self.bit_index <= self.bm.bits) {
                var idx: ?usize = null;

                if (self.current_plane & 0x1 == 0x1) {
                    idx = self.bit_index;
                }

                self.bit_index += 1;

                if (@mod(self.bit_index, 64) == 0) {
                    self.plane_index = self.bit_index / 64;
                    self.current_plane = self.bm.many.?[self.plane_index];
                } else {
                    self.current_plane >>= 1;
                }

                if (idx) |x| { return x; }
            }
            return null;
        }
    };

    pub fn setBitsIterator(self: *BitMap) SetBitsIterator {
        const plane = if (self.bits > 64) self.many.?[0] else self.single;

        return .{
            .bit_index = 0,
            .plane_index = 0,
            .current_plane = plane,
            .bm = self
        };
    }

    pub fn unsetBit(self: *BitMap, bit: u64) void {
        if (self.bits > 64) {
            const plane = bit / 64;
            const theBit: u6 = @intCast(@mod(bit, 64));
            self.many.?[plane] &= ~(@as(u64, 1) << theBit);
        } else {
            const mask = ~(@as(u64, 1) << @as(u6, @intCast(bit)));
            self.single &= mask;
        }
    }

    pub fn isBitSet(self: *BitMap, bit: u64) bool {
        if (self.bits > 64) {
            const plane = bit / 64;
            const theBit: u6 = @intCast(@mod(bit, 64));
            const mask = (@as(u64, 1) << theBit);
            return mask == (self.many.?[plane] & mask);
        } else {
            const v = (@as(u64, 1) << @as(u6, @intCast(bit)));
            return v == (self.single & v);
        }
    }

    pub fn deinit(self: *BitMap, mem: std.mem.Allocator) void {
        if (self.bits > 64) {
            mem.free(self.many.?);
        }
        mem.destroy(self);
    }
};

test "create bitmap" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 32);
    defer bm.deinit(alloc);

    bm.setBit(1);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(0));

    bm.unsetBit(1);
    try std.testing.expect(!bm.isBitSet(1));
}

test "create bitmap with 100 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 100);
    defer bm.deinit(alloc);

    bm.setBit(1);
    bm.setBit(70);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(bm.isBitSet(70));
    try std.testing.expect(!bm.isBitSet(0));
    try std.testing.expect(!bm.isBitSet(71));

    bm.unsetBit(1);
    bm.unsetBit(70);
    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(70));
}

test "create bitmap with 64 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 64);
    defer bm.deinit(alloc);

    bm.setBit(1);
    bm.setBit(63);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(bm.isBitSet(63));
    try std.testing.expect(!bm.isBitSet(0));
    try std.testing.expect(!bm.isBitSet(62));

    bm.unsetBit(1);
    bm.unsetBit(63);

    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(63));
}

test "bitset iterator single plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0 };

    const bm = try BitMap.init(alloc, 20);
    defer bm.deinit(alloc);

    bm.setBit(1);
    bm.setBit(15);

    var bitidx: usize = 0;
    var iter = bm.setBitsIterator();
    while (iter.next()) |num| {
        bits[bitidx] = num;
        bitidx += 1;
    }
    try std.testing.expectEqual(1, bits[0]);
    try std.testing.expectEqual(15, bits[1]);
}

test "bitset iterator multi plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0, 0, 0 };

    const bm = try BitMap.init(alloc, 128);
    defer bm.deinit(alloc);

    bm.setBit(1);
    bm.setBit(63);
    bm.setBit(64);
    bm.setBit(128);

    var bitidx: usize = 0;
    var iter = bm.setBitsIterator();
    while (iter.next()) |num| {
        bits[bitidx] = num;
        bitidx += 1;
    }
    try std.testing.expectEqual(1, bits[0]);
    try std.testing.expectEqual(63, bits[1]);
    try std.testing.expectEqual(64, bits[2]);
    try std.testing.expectEqual(128, bits[3]);
}
