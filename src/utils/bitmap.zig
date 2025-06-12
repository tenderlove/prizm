const std = @import("std");

const BitMapTypes = enum {
    single,
    heap,
    shared,
    nullMap,
};

pub fn BitMapSized(comptime T: type) type {
    return union(BitMapTypes) {
        const Self = @This();
        const AtomBits = @typeInfo(T).int.bits;

        single: struct {
            bits: usize,
            buff: T,
        },

        heap: struct {
            bits: usize,
            buff: []T,
        },

        shared: struct {
            bits: usize,
            buff: []const T,
        },

        nullMap: struct {
        },

        pub fn initShared(bits: usize, buff: []const T) Self {
            return .{ .shared = .{ .bits = bits, .buff = buff } };
        }

        pub fn initEmpty(mem: std.mem.Allocator, bits: usize) !*Self {
            const bm = try mem.create(Self);
            bm.* = try fillBm(mem, 0, bits);

            switch(bm.*) {
                .single => {},
                .heap => @memset(bm.heap.buff, 0),
                .shared => unreachable,
                .nullMap => unreachable,
            }

            return bm;
        }

        fn fillBm(mem: std.mem.Allocator, single: T, bits: usize) !Self {
            if (bits > AtomBits) {
                const mask: usize = AtomBits - 1;
                const storage: usize = (bits + mask) & ~mask;
                const pls = storage / AtomBits;
                const memory = try mem.alloc(T, pls);
                return .{ .heap = .{ .bits = bits, .buff = memory } };
            } else {
                return .{ .single = .{ .bits = bits, .buff = single } };
            }
        }

        pub const Null: Self = .{ .nullMap = .{ } };

        pub fn isNullBlock(self: Self) bool {
            return switch(self) {
                .nullMap => true,
                inline else => false,
            };
        }

        pub fn getBits(self: Self) usize {
            return switch(self) {
                .nullMap => 0,
                inline else => |payload| payload.bits
            };
        }

        pub fn fsb(self: Self) usize {
            return self.getBits() - self.clz() - 1;
        }

        pub fn clz(self: Self) usize {
            const mask: usize = AtomBits - 1;
            const bit_storage = (self.getBits() + mask) & ~mask;
            const padding = bit_storage - self.getBits();

            switch(self) {
                .single => return @clz(self.single.buff) - padding,
                .nullMap => unreachable,
                inline .shared, .heap => |payload| {
                    var i = payload.buff.len;
                    var acc: usize = 0;
                    while (i > 0) {
                        i -= 1;
                        const plane = payload.buff[i];
                        if (plane == 0) {
                            acc += AtomBits;
                        } else {
                            acc += @clz(plane);
                            break;
                        }
                    }
                    return acc - padding;
                }
            }
        }

        pub fn dup(orig: Self, mem: std.mem.Allocator) !*Self {
            if (orig.isNullBlock()) {
                return @constCast(&orig);
            }
            const bm = try mem.create(Self);
            bm.* = try fillBm(mem, 0, orig.getBits());
            switch(bm.*) {
                .single => bm.single.buff = orig.single.buff,
                .shared, .heap => @memcpy(bm.heap.buff, orig.heap.buff),
                .nullMap => unreachable,
            }
            return bm;
        }

        pub fn eq(self: Self, other: *Self) bool {
            if (self.getBits() != other.getBits()) return false;

            return switch(self) {
                .single => |payload| payload.buff == other.single.buff,
                .heap => |payload| std.mem.eql(T, payload.buff, other.heap.buff),
                .shared => |payload| std.mem.eql(T, payload.buff, other.shared.buff),
                .nullMap => false,
            };
        }

        pub fn count(self: Self) usize {
            switch(self) {
                .single => |p| return @popCount(p.buff),
                .nullMap => return 0,
                inline .shared, .heap => |p| {
                    var n: usize = 0;
                    for (p.buff) |plane| {
                        n += @popCount(plane);
                    }
                    return n;
                }
            }
        }

        pub fn setBit(self: *Self, bit: T) !void {
            if (bit >= self.getBits()) return error.OutOfBoundsError;

            switch(self.*) {
                .single => self.single.buff |= (@as(T, 1) << @intCast(bit)),
                .heap => {
                    const plane = bit / AtomBits;
                    self.heap.buff[plane] |= (@as(T, 1) << @intCast(@mod(bit, AtomBits)));
                },
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn set(self: *Self, bit: T) !void {
            try self.setBit(bit);
        }

        const SetBitsIterator = struct {
            bit_index: usize,
            plane_index: usize,
            current_plane: T,
            bm: Self,

            pub fn next(self: *SetBitsIterator) ?usize {
                while (self.bit_index < self.bm.getBits()) {
                    var idx: ?usize = null;

                    if (self.current_plane & 0x1 == 0x1) {
                        idx = self.bit_index;
                    }

                    self.bit_index += 1;

                    if (@mod(self.bit_index, AtomBits) == 0) {
                        self.plane_index = self.bit_index / AtomBits;

                        if (self.bit_index < self.bm.getBits()) {
                            switch(self.bm) {
                                .single => unreachable,
                                .nullMap => unreachable,
                                inline .heap, .shared => |p| {
                                    self.current_plane = p.buff[self.plane_index];
                                }
                            }
                        } else {
                            self.current_plane = 0;
                        }
                    } else {
                        self.current_plane >>= 1;
                    }

                    if (idx) |x| { return x; }
                }
                return null;
            }
        };

        pub fn setBitsIterator(self: Self) SetBitsIterator {
            const plane = switch(self) {
                inline .shared, .heap => |p| p.buff[0],
                .single => |p| p.buff,
                .nullMap => unreachable,
            };

            return .{
                .bit_index = 0,
                .plane_index = 0,
                .current_plane = plane,
                .bm = self
            };
        }

        pub fn iter(self: Self) SetBitsIterator {
            return self.setBitsIterator();
        }

        pub fn unsetBit(self: *Self, bit: T) !void {
            if (bit >= self.getBits()) return error.OutOfBoundsError;

            switch(self.*) {
                .single => {
                    const mask = ~(@as(T, 1) << @intCast(bit));
                    self.single.buff &= mask;
                },
                .heap => {
                    const plane = bit / AtomBits;
                    self.heap.buff[plane] &= ~(@as(T, 1) << @intCast(@mod(bit, AtomBits)));
                },
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn isSet(self: Self, bit: T) bool {
            if (bit >= self.getBits()) return false;

            switch(self) {
                .single => |p| {
                    const v = (@as(T, 1) << @intCast(bit));
                    return v == (p.buff & v);
                },
                .nullMap => return false,
                inline .shared, .heap => |p| {
                    const plane = bit / AtomBits;
                    const mask = (@as(T, 1) << @intCast(@mod(bit, AtomBits)));
                    return mask == (p.buff[plane] & mask);
                }
            }
        }

        pub fn setIntersection(self: *Self, other: *Self) !void {
            if (self.getBits() != other.getBits()) return error.ArgumentError;

            switch (self.*) {
                .single => self.single.buff &= other.single.buff,
                .heap => {
                    for (0..self.heap.buff.len) |i| {
                        self.heap.buff[i] &= other.heap.buff[i];
                    }
                },
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn intersection(self: Self, other: *Self, mem: std.mem.Allocator) !*Self {
            const new = try self.dup(mem);
            try new.setIntersection(other);
            return new;
        }

        pub fn setNot(self: *Self) void {
            switch (self.*) {
                .single => self.single.buff = ~self.single.buff,
                .heap => {
                    for (0..self.heap.buff.len) |i| {
                        self.heap.buff[i] = ~self.heap.buff[i];
                    }
                },
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn not(self: Self, mem: std.mem.Allocator) !*Self {
            const new = try self.dup(mem);
            new.setNot();
            return new;
        }

        pub fn replace(self: *Self, other: *Self) !void {
            if (self.getBits() != other.getBits()) return error.ArgumentError;

            switch(self.*) {
                .single => self.single.buff = other.single.buff,
                .heap => @memcpy(self.heap.buff, other.heap.buff),
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn Union(self: Self, other: *Self, mem: std.mem.Allocator) !*Self {
            const new = try self.dup(mem);
            try new.setUnion(other);
            return new;
        }

        pub fn setUnion(self: *Self, other: *Self) !void {
            if (self.getBits() != other.getBits()) return error.ArgumentError;

            switch(self.*) {
                .single => self.single.buff |= other.single.buff,
                .heap => {
                    for (0..self.heap.buff.len) |i| {
                        self.heap.buff[i] |= other.heap.buff[i];
                    }
                },
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn deinit(self: *Self, mem: std.mem.Allocator) void {
            switch(self.*) {
                .heap => mem.free(self.heap.buff),
                .shared => {},
                .single => {},
                .nullMap => return,
            }
            mem.destroy(self);
        }
    };
}

pub const BitMap = BitMapSized(u64);

test "works with smaller ints" {
    const alloc = std.testing.allocator;

    const bmu8 = try BitMapSized(u8).initEmpty(alloc, 8);
    defer bmu8.deinit(alloc);
    try bmu8.setBit(1);
    try std.testing.expect(bmu8.isSet(1));

    const bmu832 = try BitMapSized(u8).initEmpty(alloc, 32);
    defer bmu832.deinit(alloc);
    try bmu832.setBit(31);
    try std.testing.expect(bmu832.isSet(31));
}

test "create bitmap" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.initEmpty(alloc, 32);
    defer bm.deinit(alloc);

    try bm.setBit(1);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(!bm.isSet(0));

    try bm.unsetBit(1);
    try std.testing.expect(!bm.isSet(1));
}

test "create bitmap with 100 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.initEmpty(alloc, 100);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(70);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(bm.isSet(70));
    try std.testing.expect(!bm.isSet(0));
    try std.testing.expect(!bm.isSet(71));

    try bm.unsetBit(1);
    try bm.unsetBit(70);
    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(70));
}

test "create bitmap with 64 bits" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(bm.isSet(63));
    try std.testing.expect(!bm.isSet(0));
    try std.testing.expect(!bm.isSet(62));

    try bm.unsetBit(1);
    try bm.unsetBit(63);

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
}

test "bitset iterator single plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0 };

    const bm = try BitMap.initEmpty(alloc, 20);
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

test "bitset iterator extreme" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0 };

    const bm = try BitMap.initEmpty(alloc, 7);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(6);

    var bitidx: usize = 0;
    var iter = bm.setBitsIterator();
    while (iter.next()) |num| {
        bits[bitidx] = num;
        bitidx += 1;
    }
    try std.testing.expectEqual(1, bits[0]);
    try std.testing.expectEqual(6, bits[1]);
}

test "bitset iterator multi plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0, 0, 0, 0 };

    const bm = try BitMap.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);
    try bm.setBit(127);
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
    try std.testing.expectEqual(127, bits[3]);
}

test "popcount single plane" {
    const alloc = std.testing.allocator;

    const bm = try BitMap.initEmpty(alloc, 20);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(15);

    try std.testing.expectEqual(2, bm.count());
}

test "popcount multi-plane" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);

    try std.testing.expectEqual(3, bm.count());
}

test "not big" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(64);

    bm.setNot();

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
    try std.testing.expect(!bm.isSet(64));
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(2));
    try std.testing.expect(bm.isSet(65));
}

test "not small" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    bm.setNot();

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(2));
}

test "dup small" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);

    const new = try bm.dup(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isSet(1));
    try std.testing.expect(new.isSet(63));
}

test "dup big" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(70);

    const new = try bm.dup(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isSet(1));
    try std.testing.expect(new.isSet(63));
    try std.testing.expect(new.isSet(70));
}

test "not" {
    const alloc = std.testing.allocator;
    const bm = try BitMap.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    try bm.setBit(1);
    try bm.setBit(63);
    try bm.setBit(70);

    const new = try bm.not(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(!new.isSet(1));
    try std.testing.expect(!new.isSet(63));
    try std.testing.expect(!new.isSet(70));
}

test "set intersection small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    try bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(!bm1.isSet(2));
}

test "set intersection large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try bm2.setBit(0);
    try bm2.setBit(1);
    try bm2.setBit(70);

    try bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(!bm1.isSet(2));
    try std.testing.expect(bm1.isSet(70));
}

test "intersection" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    const bm3 = try bm1.intersection(bm2, alloc);
    defer bm3.deinit(alloc);

    try std.testing.expect(!bm3.isSet(0));
    try std.testing.expect(bm3.isSet(1));
    try std.testing.expect(!bm3.isSet(2));
}

test "set union small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    try bm1.setUnion(bm2);

    try std.testing.expect(bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(bm1.isSet(2));
}

test "set union large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try bm2.setBit(0);
    try bm2.setBit(1);
    try bm2.setBit(70);

    try bm1.setUnion(bm2);

    try std.testing.expect(bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(bm1.isSet(2));
    try std.testing.expect(bm1.isSet(70));
}

test "union" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);

    try bm2.setBit(0);
    try bm2.setBit(1);

    const bm3 = try bm1.Union(bm2, alloc);
    defer bm3.deinit(alloc);

    try std.testing.expect(bm3.isSet(0));
    try std.testing.expect(bm3.isSet(1));
    try std.testing.expect(bm3.isSet(2));
}

test "eq small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
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
    const bm1 = try BitMap.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 128);
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
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    try bm1.setBit(0);
    try bm1.setBit(1);
    try bm1.setBit(15);

    try std.testing.expect(!bm1.eq(bm2));
    try std.testing.expect(!bm2.isSet(15));
    try bm2.replace(bm1);
    try std.testing.expect(bm2.isSet(0));
    try std.testing.expect(bm2.isSet(1));
    try std.testing.expect(bm2.isSet(15));
    try std.testing.expect(bm1.eq(bm2));
}

test "replace large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    const bm2 = try BitMap.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    try bm1.setBit(1);
    try bm1.setBit(2);
    try bm1.setBit(70);

    try std.testing.expect(!bm1.eq(bm2));
    try std.testing.expect(!bm2.isSet(70));
    try bm2.replace(bm1);
    try std.testing.expect(bm2.isSet(1));
    try std.testing.expect(bm2.isSet(2));
    try std.testing.expect(bm2.isSet(70));
    try std.testing.expect(bm1.eq(bm2));
}

test "clz small" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);

    try std.testing.expectEqual(16, bm1.clz());
    try bm1.setBit(0);
    try std.testing.expectEqual(15, bm1.clz());
    try bm1.setBit(1);
    try std.testing.expectEqual(14, bm1.clz());
    try bm1.setBit(15);
    try std.testing.expectEqual(0, bm1.clz());

    const bm2 = try BitMap.initEmpty(alloc, 64);
    defer bm2.deinit(alloc);

    try std.testing.expectEqual(64, bm2.clz());
    try bm2.setBit(0);
    try std.testing.expectEqual(63, bm2.clz());
    try bm2.setBit(1);
    try std.testing.expectEqual(62, bm2.clz());
    try bm2.setBit(63);
    try std.testing.expectEqual(0, bm2.clz());
}

test "clz large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 70);
    defer bm1.deinit(alloc);

    try std.testing.expectEqual(70, bm1.clz());
    try bm1.setBit(0);
    try std.testing.expectEqual(69, bm1.clz());
    try bm1.setBit(1);
    try std.testing.expectEqual(68, bm1.clz());
    try bm1.setBit(69);
    try std.testing.expectEqual(0, bm1.clz());
}

test "fsb" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);

    try bm1.setBit(0);
    try std.testing.expectEqual(0, bm1.fsb());
    try bm1.setBit(1);
    try std.testing.expectEqual(1, bm1.fsb());
    try bm1.setBit(15);
    try std.testing.expectEqual(15, bm1.fsb());
}

test "fsb large" {
    const alloc = std.testing.allocator;
    const bm1 = try BitMap.initEmpty(alloc, 70);
    defer bm1.deinit(alloc);

    try bm1.setBit(0);
    try std.testing.expectEqual(0, bm1.fsb());
    try bm1.setBit(1);
    try std.testing.expectEqual(1, bm1.fsb());
    try bm1.setBit(15);
    try std.testing.expectEqual(15, bm1.fsb());
    try bm1.setBit(64);
    try std.testing.expectEqual(64, bm1.fsb());
}

test "init shared" {
    const bits = [_]u64 { 0b1, 0b1 };
    const bm = BitMapSized(u64).initShared(128, &bits);
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(bm.isSet(64));
}

test "iterate shared" {
    const bits = [_]u64 { 0b1, 0b1 };
    const bm = BitMapSized(u64).initShared(128, &bits);
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(64));

    var check = [_]usize { 0, 0 };

    var bitidx: usize = 0;
    var iter = bm.setBitsIterator();
    while (iter.next()) |num| {
        check[bitidx] = num;
        bitidx += 1;
    }
    try std.testing.expectEqual(0, check[0]);
    try std.testing.expectEqual(64, check[1]);
}

test "dup null" {
    const bm = BitMap.Null;
    const duped = try bm.dup(std.testing.allocator);
    try std.testing.expect(duped.isNullBlock());
    try std.testing.expectEqual(bm, duped.*);
}
