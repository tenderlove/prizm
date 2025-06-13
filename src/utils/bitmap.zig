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

        pub fn findLastSet(self: Self) usize {
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

        pub fn unsetAll(self: *Self) void {
            switch(self.*) {
                .single => self.single.buff = 0,
                .heap => @memset(self.heap.buff, 0),
                .shared => unreachable,
                .nullMap => unreachable,
            }
        }

        pub fn clone(orig: Self, mem: std.mem.Allocator) !*Self {
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

        pub fn eql(self: Self, other: *Self) bool {
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

        pub fn set(self: *Self, bit: T) void {
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

        pub const SetBitsIterator = struct {
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

        pub fn iterator(self: Self, _: anytype) SetBitsIterator {
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

        pub fn unset(self: *Self, bit: T) void {
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

        pub fn setIntersection(self: *Self, other: *Self) void {
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

        pub fn toggleAll(self: *Self) void {
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

        pub fn setUnion(self: *Self, other: *Self) void {
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

const dbs = std.DynamicBitSetUnmanaged;

test "works with smaller ints" {
    const alloc = std.testing.allocator;

    const bmu8 = try BitMapSized(u8).initEmpty(alloc, 8);
    defer bmu8.deinit(alloc);
    bmu8.set(1);
    try std.testing.expect(bmu8.isSet(1));

    const bmu832 = try BitMapSized(u8).initEmpty(alloc, 32);
    defer bmu832.deinit(alloc);
    bmu832.set(31);
    try std.testing.expect(bmu832.isSet(31));
}

test "create bitmap" {
    const alloc = std.testing.allocator;

    var bm = try dbs.initEmpty(alloc, 32);
    defer bm.deinit(alloc);

    bm.set(1);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(!bm.isSet(0));

    bm.unset(1);
    try std.testing.expect(!bm.isSet(1));
}

test "create bitmap with 100 bits" {
    const alloc = std.testing.allocator;

    var bm = try dbs.initEmpty(alloc, 100);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(70);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(bm.isSet(70));
    try std.testing.expect(!bm.isSet(0));
    try std.testing.expect(!bm.isSet(71));

    bm.unset(1);
    bm.unset(70);
    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(70));
}

test "create bitmap with 64 bits" {
    const alloc = std.testing.allocator;

    var bm = try dbs.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);

    try std.testing.expect(bm.isSet(1));
    try std.testing.expect(bm.isSet(63));
    try std.testing.expect(!bm.isSet(0));
    try std.testing.expect(!bm.isSet(62));

    bm.unset(1);
    bm.unset(63);

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
}

test "bitset iterator single plane" {
    const alloc = std.testing.allocator;
    var bits = [_]usize { 0, 0 };

    var bm = try dbs.initEmpty(alloc, 20);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(15);

    var bitidx: usize = 0;
    var iter = bm.iterator(.{});
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

    var bm = try dbs.initEmpty(alloc, 7);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(6);

    var bitidx: usize = 0;
    var iter = bm.iterator(.{});
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

    var bm = try dbs.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);
    bm.set(64);
    bm.set(127);
    // try std.testing.expectError(error.OutOfBoundsError, bm.set(128));
    // try std.testing.expectError(error.OutOfBoundsError, bm.unset(128));

    var bitidx: usize = 0;
    var iter = bm.iterator(.{});
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

    var bm = try dbs.initEmpty(alloc, 20);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(15);

    try std.testing.expectEqual(2, bm.count());
}

test "popcount multi-plane" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);
    bm.set(64);

    try std.testing.expectEqual(3, bm.count());
}

test "not big" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);
    bm.set(64);

    bm.toggleAll();

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
    try std.testing.expect(!bm.isSet(64));
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(2));
    try std.testing.expect(bm.isSet(65));
}

test "not small" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);

    bm.toggleAll();

    try std.testing.expect(!bm.isSet(1));
    try std.testing.expect(!bm.isSet(63));
    try std.testing.expect(bm.isSet(0));
    try std.testing.expect(bm.isSet(2));
}

test "dup small" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 64);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);

    var new = try bm.clone(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isSet(1));
    try std.testing.expect(new.isSet(63));
}

test "dup big" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);
    bm.set(70);

    var new = try bm.clone(alloc);
    defer new.deinit(alloc);

    try std.testing.expect(new.isSet(1));
    try std.testing.expect(new.isSet(63));
    try std.testing.expect(new.isSet(70));
}

test "not" {
    const alloc = std.testing.allocator;
    var bm = try dbs.initEmpty(alloc, 128);
    defer bm.deinit(alloc);

    bm.set(1);
    bm.set(63);
    bm.set(70);

    var new = try bm.clone(alloc);
    defer new.deinit(alloc);
    new.toggleAll();

    try std.testing.expect(!new.isSet(1));
    try std.testing.expect(!new.isSet(63));
    try std.testing.expect(!new.isSet(70));
}

test "set intersection small" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);

    bm2.set(0);
    bm2.set(1);

    bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(!bm1.isSet(2));
}

test "set intersection large" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);
    bm1.set(70);

    bm2.set(0);
    bm2.set(1);
    bm2.set(70);

    bm1.setIntersection(bm2);

    try std.testing.expect(!bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(!bm1.isSet(2));
    try std.testing.expect(bm1.isSet(70));
}

test "intersection" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);

    bm2.set(0);
    bm2.set(1);

    var bm3 = try bm1.clone(alloc);
    bm3.setIntersection(bm2);
    defer bm3.deinit(alloc);

    try std.testing.expect(!bm3.isSet(0));
    try std.testing.expect(bm3.isSet(1));
    try std.testing.expect(!bm3.isSet(2));
}

test "set union small" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);

    bm2.set(0);
    bm2.set(1);

    bm1.setUnion(bm2);

    try std.testing.expect(bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(bm1.isSet(2));
}

test "set union large" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);
    bm1.set(70);

    bm2.set(0);
    bm2.set(1);
    bm2.set(70);

    bm1.setUnion(bm2);

    try std.testing.expect(bm1.isSet(0));
    try std.testing.expect(bm1.isSet(1));
    try std.testing.expect(bm1.isSet(2));
    try std.testing.expect(bm1.isSet(70));
}

test "union" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);

    bm2.set(0);
    bm2.set(1);

    var bm3 = try bm1.clone(alloc);
    bm3.setUnion(bm2);
    defer bm3.deinit(alloc);

    try std.testing.expect(bm3.isSet(0));
    try std.testing.expect(bm3.isSet(1));
    try std.testing.expect(bm3.isSet(2));
}

test "eq small" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(0);
    bm1.set(1);

    bm2.set(0);
    bm2.set(1);

    try std.testing.expect(bm1.eql(bm2));
    bm2.set(2);
    try std.testing.expect(!bm1.eql(bm2));
}

test "eq large" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);
    bm1.set(70);

    bm2.set(1);
    bm2.set(2);
    bm2.set(70);

    try std.testing.expect(bm1.eql(bm2));
    bm2.set(0);
    try std.testing.expect(!bm1.eql(bm2));
}

test "replace small" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 16);
    defer bm2.deinit(alloc);

    bm1.set(0);
    bm1.set(1);
    bm1.set(15);

    try std.testing.expect(!bm1.eql(bm2));
    try std.testing.expect(!bm2.isSet(15));
    bm2.unsetAll();
    bm2.setUnion(bm1);
    try std.testing.expect(bm2.isSet(0));
    try std.testing.expect(bm2.isSet(1));
    try std.testing.expect(bm2.isSet(15));
    try std.testing.expect(bm1.eql(bm2));
}

test "replace large" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 128);
    defer bm1.deinit(alloc);
    var bm2 = try dbs.initEmpty(alloc, 128);
    defer bm2.deinit(alloc);

    bm1.set(1);
    bm1.set(2);
    bm1.set(70);

    try std.testing.expect(!bm1.eql(bm2));
    try std.testing.expect(!bm2.isSet(70));
    bm2.unsetAll();
    bm2.setUnion(bm1);
    try std.testing.expect(bm2.isSet(1));
    try std.testing.expect(bm2.isSet(2));
    try std.testing.expect(bm2.isSet(70));
    try std.testing.expect(bm1.eql(bm2));
}

test "fsb" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 16);
    defer bm1.deinit(alloc);

    bm1.set(0);
    try std.testing.expectEqual(0, bm1.findLastSet());
    bm1.set(1);
    try std.testing.expectEqual(1, bm1.findLastSet());
    bm1.set(15);
    try std.testing.expectEqual(15, bm1.findLastSet());
}

test "fsb large" {
    const alloc = std.testing.allocator;
    var bm1 = try dbs.initEmpty(alloc, 70);
    defer bm1.deinit(alloc);

    bm1.set(0);
    try std.testing.expectEqual(0, bm1.findLastSet());
    bm1.set(1);
    try std.testing.expectEqual(1, bm1.findLastSet());
    bm1.set(15);
    try std.testing.expectEqual(15, bm1.findLastSet());
    bm1.set(64);
    try std.testing.expectEqual(64, bm1.findLastSet());
}
