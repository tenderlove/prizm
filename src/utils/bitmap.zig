const std = @import("std");

pub const BitMap = struct {
    bits: usize,
    single: u64,
    many: ?[]u64,

    pub fn init(mem: std.mem.Allocator, bits: usize) !*BitMap {
        const bm = try mem.create(BitMap);
        bm.* = .{ .bits = bits, .single = 0, .many = null };
        if (bits > 63) {
            const planes = ((bits + 63) / 64) + 1;
            const memory = try mem.alloc(u64, planes);
            @memset(memory, 0);
            bm.*.many = memory;
        }
        return bm;
    }

    pub fn setBit(self: *BitMap, bit: u64) void {
        if (self.bits > 63) {
            const plane = bit / 64;
            const theBit: u6 = @intCast(@mod(bit, 64));
            self.many.?[plane] |= (@as(u64, 1) << theBit);
        } else {
            self.single |= (@as(u64, 1) << @as(u6, @intCast(bit)));
        }
    }

    pub fn unsetBit(self: *BitMap, bit: u64) void {
        if (self.bits > 63) {
            const plane = bit / 64;
            const theBit: u6 = @intCast(@mod(bit, 64));
            self.many.?[plane] &= ~(@as(u64, 1) << theBit);
        } else {
            const mask = ~(@as(u64, 1) << @as(u6, @intCast(bit)));
            self.single &= mask;
        }
    }

    pub fn isBitSet(self: *BitMap, bit: u64) bool {
        if (self.bits > 63) {
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
        if (self.bits > 63) {
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
    bm.setBit(64);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(bm.isBitSet(64));
    try std.testing.expect(!bm.isBitSet(0));
    try std.testing.expect(!bm.isBitSet(63));

    bm.unsetBit(1);
    bm.unsetBit(64);

    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(64));
}
