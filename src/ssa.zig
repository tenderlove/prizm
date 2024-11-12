const std = @import("std");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const Register = struct {
    number: u32,
};

pub const InstructionName = enum {
    add,
    call,
    getlocal,
    getmethod,
    getself,
    loadi,
    move,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    add: struct {
        out: Register,
        in1: Register,
        in2: Register,
    },

    call: struct {
        out: Register,
        funcreg: Register,
        params: std.ArrayList(Register),
    },

    getlocal: struct {
        out: Register,
        in: Register,
    },

    getmethod: struct {
        out: Register,
        recv: Register,
        ccid: usize,
    },

    getself: struct {
        out: Register,
    },

    loadi: struct {
        out: Register,
        val: u32,
    },

    move: struct {
        out: Register,
        in: Register,
    },

    setlocal: struct {
        out: Register,
        in: Register,
    },

    pub fn isJump(self: Instruction) bool {
        _ = self;
        return false;
    }

    pub fn isReturn(self: Instruction) bool {
        _ = self;
        return false;
    }

    pub fn isCall(self: Instruction) bool {
        return switch(self) {
            .call, .add => true,
            else => false
        };
    }

    pub fn deinit(self: Instruction) void {
        switch(self) {
            .call => |x| x.params.deinit(),
            else  => {}
        }
    }

    pub const Common = struct {
        out: Register,
    };

    pub fn encode(self: Instruction) !u32 {
        _ = self;
        return error.NotImplementedError;
        // return switch (self) {
        //     // | out - 13 bit | in1 - 13 bit | insn 6 bit |
        //     .move => |v| v.out.number << 19 | v.in.number << 6 | @intFromEnum(InstructionName.move),
        //     .loadi => |v| v.out.number << 19 | v.val << 6 | @intFromEnum(InstructionName.loadi),
        //     // | out - 8 bit | in1 - 8 bit | in2 - 8 bit | insn 6 bit |
        //     .add => |v| v.out.number << (6 + 8 + 8) | v.in1.number << (6 + 8) | v.in2.number << 6 | @intFromEnum(InstructionName.add),
        //     .getself => |v| v.out.number << 6 | @intFromEnum(InstructionName.getself),
        //     .getmethod => |v| {
        //         v.ccid << (6 + 8) | v.out.number << 6 | @intFromEnum(InstructionName.getself);
        //     },
        //     .call => |v| {
        //         v.ccid << (6 + 8) | v.out.number << 6 | @intFromEnum(InstructionName.getself);
        //     },
        // };
    }

    pub fn printNode(self: *InstructionList.Node) void {
        const node = self.data;

        // Calculate the maximum width of the instruction name at comptime
        // so we can use it to pad the name when printing
        comptime var maxlen: usize = 0;
        comptime for (@typeInfo(InstructionName).@"enum".fields) |field| {
            const name_len = field.name.len;
            if (name_len > maxlen) {
                maxlen = name_len;
            }
        };

        std.debug.print("{[value]s: <[width]}", .{ .value = @tagName(node), .width = maxlen + 1, });

        switch(node) {
            .move => |v| {
                std.debug.print("{d} {d}\n", .{ v.out.number, v.in.number });
            },
            .loadi => |v| {
                std.debug.print("{d} {d}\n", .{ v.out.number, v.val });
            },
            .add => |v| {
                std.debug.print("{d} {d} {d}\n", .{ v.out.number, v.in1.number, v.in2.number });
            },
            .getself => |v| {
                std.debug.print("{d}\n", .{ v.out.number });
            },
            .getmethod => |v| {
                std.debug.print("{d}\n", .{ v.out.number });
            },
            .call => |v| {
                std.debug.print("{d}\n", .{ v.out.number });
            },
        }
    }
};
