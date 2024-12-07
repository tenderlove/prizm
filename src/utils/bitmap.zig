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

    pub fn dup(orig: *BitMap, mem: std.mem.Allocator) !*BitMap {
        const new = try mem.create(BitMap);
        const bits = orig.bits;
        new.* = .{ .bits = bits, .single = orig.single, .many = null };
        if (bits > 64) {
            const planes = ((bits + 63) / 64) + 1;
            const memory = try mem.alloc(u64, planes);
            @memcpy(memory, orig.many.?);
            new.*.many = memory;
        }
        return new;
    }

    pub fn eq(self: *BitMap, other: *BitMap) bool {
        if (self.bits != other.bits) return false;

        if (self.bits > 64) {
            return std.mem.eql(u64, self.many.?, other.many.?);
        } else {
            return self.single == other.single;
        }
    }

    pub fn popCount(self: *BitMap) usize {
        if (self.bits > 64) {
            var count: usize = 0;
            for (self.many.?) |plane| {
                count += @popCount(plane);
            }
            return count;
        } else {
            return @popCount(self.single);
        }
    }

    pub fn setBit(self: *BitMap, bit: u64) !void {
        if (bit >= self.bits) return error.OutOfBoundsError;

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

    pub fn unsetBit(self: *BitMap, bit: u64) !void {
        if (bit >= self.bits) return error.OutOfBoundsError;

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
        if (bit >= self.bits) return false;

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

    pub fn setIntersection(self: *BitMap, other: *BitMap) !void {
        if (self.bits != other.bits) return error.ArgumentError;

        if (self.bits > 64) {
            const planes = ((self.bits + 63) / 64) + 1;
            for (0..planes) |i| {
                self.many.?[i] &= other.many.?[i];
            }
        } else {
            self.single &= other.single;
        }
    }

    pub fn intersection(self: *BitMap, other: *BitMap, mem: std.mem.Allocator) !*BitMap {
        const new = try self.dup(mem);
        try new.setIntersection(other);
        return new;
    }

    pub fn setNot(self: *BitMap) void {
        if (self.bits > 64) {
            const planes = ((self.bits + 63) / 64) + 1;
            for (0..planes) |i| {
                const plane = self.many.?[i];
                self.many.?[i] = ~plane;
            }
        } else {
            self.single = ~self.single;
        }
    }

    pub fn not(self: *BitMap, mem: std.mem.Allocator) !*BitMap {
        const new = try self.dup(mem);
        new.setNot();
        return new;
    }

    pub fn replace(self: *BitMap, other: *BitMap) !void {
        if (self.bits != other.bits) return error.ArgumentError;

        if (self.bits > 64) {
            @memcpy(self.many.?, other.many.?);
        } else {
            self.single = other.single;
        }
    }

    pub fn Union(self: *BitMap, other: *BitMap, mem: std.mem.Allocator) !*BitMap {
        const new = try self.dup(mem);
        try new.setUnion(other);
        return new;
    }

    pub fn setUnion(self: *BitMap, other: *BitMap) !void {
        if (self.bits != other.bits) return error.ArgumentError;

        if (self.bits > 64) {
            const planes = ((self.bits + 63) / 64) + 1;
            for (0..planes) |i| {
                self.many.?[i] |= other.many.?[i];
            }
        } else {
            self.single |= other.single;
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

    try bm.setBit(1);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(0));

    try bm.unsetBit(1);
    try std.testing.expect(!bm.isBitSet(1));
}

test "create bitmap with 100 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 100);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(70);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(bm.isBitSet(70));
    try std.testing.expect(!bm.isBitSet(0));
    try std.testing.expect(!bm.isBitSet(71));

    try bm.unsetBit(1);
    try bm.unsetBit(70);
    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(70));
}

test "create bitmap with 64 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    try std.testing.expect(bm.isBitSet(1));
    try std.testing.expect(bm.isBitSet(63));
    try std.testing.expect(!bm.isBitSet(0));
    try std.testing.expect(!bm.isBitSet(62));

    try bm.unsetBit(1);
    try bm.unsetBit(63);

    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(63));
}

test "bitset iterator single plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0 };

    const bm = try BitMap.init(alloc, 20);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(15);

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

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);
    try std.testing.expectError(error.OutOfBoundsError, bm.setBit(128));
    try std.testing.expectError(error.OutOfBoundsError, bm.unsetBit(128));

    var bitidx: usize = 0;
    var iter = bm.setBitsIterator();
    while (iter.next()) |num| {
        bits[bitidx] = num;
        bitidx += 1;
    }
    try std.testing.expectEqual(1, bits[0]);
    try std.testing.expectEqual(63, bits[1]);
    try std.testing.expectEqual(64, bits[2]);
}

test "popcount single plane" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.init(alloc, 20);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(15);

    try std.testing.expectEqual(2, bm.popCount());
}

test "popcount multi-plane" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);

    try std.testing.expectEqual(3, bm.popCount());
}

test "not big" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);

    bm.setNot();

    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(63));
    try std.testing.expect(!bm.isBitSet(64));
    try std.testing.expect(bm.isBitSet(0));
    try std.testing.expect(bm.isBitSet(2));
    try std.testing.expect(bm.isBitSet(65));
}

test "not small" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    bm.setNot();

    try std.testing.expect(!bm.isBitSet(1));
    try std.testing.expect(!bm.isBitSet(63));
    try std.testing.expect(bm.isBitSet(0));
    try std.testing.expect(bm.isBitSet(2));
}

test "dup small" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    const new = try bm.dup(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isBitSet(1));
    try std.testing.expect(new.isBitSet(63));
}

test "dup big" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(70);

    const new = try bm.dup(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isBitSet(1));
    try std.testing.expect(new.isBitSet(63));
    try std.testing.expect(new.isBitSet(70));
}

test "not" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.init(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(70);

    const new = try bm.not(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(!new.isBitSet(1));
    try std.testing.expect(!new.isBitSet(63));
    try std.testing.expect(!new.isBitSet(70));
}

test "set intersection small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    try bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isBitSet(0));
    try std.testing.expect(bm1.isBitSet(1));
    try std.testing.expect(!bm1.isBitSet(2));
}

test "set intersection large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try bm2.setBit(0);
    try bm2.setBit(1);
    try bm2.setBit(70);

    try bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isBitSet(0));
    try std.testing.expect(bm1.isBitSet(1));
    try std.testing.expect(!bm1.isBitSet(2));
    try std.testing.expect(bm1.isBitSet(70));
}

test "intersection" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    const bm3 = try bm1.intersection(bm2, alloc);
    defer bm3.deinit(alloc);

    try std.testing.expect(!bm3.isBitSet(0));
    try std.testing.expect(bm3.isBitSet(1));
    try std.testing.expect(!bm3.isBitSet(2));
}

test "set union small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    try bm1.setUnion(bm2);

    try std.testing.expect(bm1.isBitSet(0));
    try std.testing.expect(bm1.isBitSet(1));
    try std.testing.expect(bm1.isBitSet(2));
}

test "set union large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try bm2.setBit(0);
    try bm2.setBit(1);
    try bm2.setBit(70);

    try bm1.setUnion(bm2);

    try std.testing.expect(bm1.isBitSet(0));
    try std.testing.expect(bm1.isBitSet(1));
    try std.testing.expect(bm1.isBitSet(2));
    try std.testing.expect(bm1.isBitSet(70));
}

test "union" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    const bm3 = try bm1.Union(bm2, alloc);
    defer bm3.deinit(alloc);

    try std.testing.expect(bm3.isBitSet(0));
    try std.testing.expect(bm3.isBitSet(1));
    try std.testing.expect(bm3.isBitSet(2));
}

test "eq small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(0);
    try bm1.setBit(1);

    try bm2.setBit(0);
    try bm2.setBit(1);

    try std.testing.expect(bm1.eq(bm2));
    try bm2.setBit(2);
    try std.testing.expect(!bm1.eq(bm2));
}

test "eq large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try bm2.setBit(1);
    try bm2.setBit(2);
    try bm2.setBit(70);

    try std.testing.expect(bm1.eq(bm2));
    try bm2.setBit(0);
    try std.testing.expect(!bm1.eq(bm2));
}

test "replace small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(0);
    try bm1.setBit(1);
    try bm1.setBit(15);

    try std.testing.expect(!bm1.eq(bm2));
    try std.testing.expect(!bm2.isBitSet(15));
    try bm2.replace(bm1);
    try std.testing.expect(bm2.isBitSet(0));
    try std.testing.expect(bm2.isBitSet(1));
    try std.testing.expect(bm2.isBitSet(15));
    try std.testing.expect(bm1.eq(bm2));
}

test "replace large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.init(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.init(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try std.testing.expect(!bm1.eq(bm2));
    try std.testing.expect(!bm2.isBitSet(70));
    try bm2.replace(bm1);
    try std.testing.expect(bm2.isBitSet(1));
    try std.testing.expect(bm2.isBitSet(2));
    try std.testing.expect(bm2.isBitSet(70));
    try std.testing.expect(bm1.eq(bm2));
}
