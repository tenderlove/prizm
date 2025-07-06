const std = @import("std");
const cmp = @import("compiler.zig");

const Ruby = struct {
    const TagBits = enum(u3) {
        HEAP, // 000
        INT, // 001
        FLOAT, // 010
        BOOL, // 011
        NULL, // 101
        UNDEF, // 110
    };

    const TRUE: usize = 0b1011;
    const FALSE: usize = 0b0011;
    const NULL: usize = @intFromEnum(TagBits.NULL);
    const UNDEF: usize = @intFromEnum(TagBits.UNDEF);
};

pub const CallCache = struct {
    method_name: []const u8,
    argc: usize,
};

pub const VM = struct {
    const Frame = struct {
        reg_stack: std.ArrayList(usize),
        reg_start: usize,
        self: usize,

        pub fn setReg(self: *Frame, reg: u32, val: usize) void {
            self.reg_stack.items[self.reg_start + reg] = val;
        }

        pub fn getReg(self: *Frame, reg: u32) usize {
            return self.reg_stack.items[self.reg_start + reg];
        }

        pub fn deinit(self: Frame) void {
            self.reg_stack.deinit();
        }
    };

    allocator: std.mem.Allocator,
    top_frame: *Frame,
    strings: std.StringHashMapUnmanaged([]const u8),

    // Looks up a string from the string pool and returns it.
    // If the string isn't found in the string pool, copies it with
    // the vm's allocator, inserts it in to the pool, and returns the copy.
    // All strings in the pool are owned by the VM.
    pub fn getString(self: *VM, str: []const u8) ![]const u8 {
        const value = self.strings.get(str);
        if (value) |v| {
            return v;
        } else {
            const v = try self.allocator.dupe(u8, str);
            try self.strings.put(self.allocator, v, v);
            return v;
        }
    }

    pub fn eval(self: *VM, iseq: *cmp.InstructionSequence) !void {
        std.debug.print("iseq len {d}\n", .{iseq.insns.len});
        for (iseq.insns) |insn_enc| {
            const insn: cmp.InstructionName = @enumFromInt(insn_enc & 0x3F);

            switch (insn) {
                .move => {
                    const out = (insn_enc >> 19) & 0x1FFF;
                    const in = (insn_enc >> 6) & 0x1FFF;
                    try self.move(out, in);
                },
                .loadi => {
                    const out = (insn_enc >> 19) & 0x1FFF;
                    const in = (insn_enc >> 6) & 0x1FFF;
                    self.loadi(out, in);
                },
                .add => {
                    const out = (insn_enc >> (6 + 8 + 8)) & 0xFF;
                    const in1 = (insn_enc >> (6 + 8)) & 0xFF;
                    const in2 = (insn_enc >> 6) & 0x1FFF;
                    self.add(out, in1, in2);
                },
                .getself => {
                    const out = (insn_enc >> 6) & 0x1FFF;
                    self.getself(out);
                },
            }
        }
    }

    pub fn deinit(self: *VM, allocator: std.mem.Allocator) void {
        var it = self.strings.valueIterator();
        while (it.next()) |val| {
            allocator.free(val.*);
        }
        self.strings.deinit(allocator);
        self.top_frame.deinit();
        allocator.destroy(self.top_frame);
        allocator.destroy(self);
    }

    fn move(self: *VM, out: u32, in: u32) !void {
        _ = self;
        _ = out;
        _ = in;
        return error.NotImplementedError;
    }

    fn loadi(self: *VM, out: u32, in: u32) void {
        self.top_frame.setReg(out, in);
    }

    fn getself(self: *VM, out: u32) void {
        self.top_frame.setReg(out, self.top_frame.self);
    }

    fn add(self: *VM, outr: u32, inr1: u32, inr2: u32) void {
        const in1 = self.top_frame.getReg(inr1);
        const in2 = self.top_frame.getReg(inr2);
        const out = in1 + in2;
        std.debug.print("out {d}\n", .{out});
        self.top_frame.setReg(outr, out);
    }
};

pub fn init(allocator: std.mem.Allocator) !*VM {
    const self = try allocator.create(VM);
    const top_frame = try allocator.create(VM.Frame);
    const strings = std.StringHashMapUnmanaged([]const u8){};

    top_frame.* = .{
        .reg_stack = std.ArrayList(usize).init(allocator),
        .reg_start = 0,
        .self = Ruby.NULL,
    };

    try top_frame.reg_stack.appendNTimes(@intCast(Ruby.NULL), 200);

    self.* = .{
        .allocator = allocator,
        .top_frame = top_frame,
        .strings = strings,
    };
    return self;
}
