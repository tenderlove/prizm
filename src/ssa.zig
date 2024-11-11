const std = @import("std");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const BasicBlock = struct {
    name: u64,
    start: *InstructionList.Node,
    finish: *InstructionList.Node,
    out: ?*BasicBlock,
    out2: ?*BasicBlock,

    fn addInstruction(self: *BasicBlock, insn: *InstructionList.Node) void {
        std.debug.print("adding ", .{});
        Instruction.printNode(insn);
        self.finish = insn;
        std.debug.print("added ", .{});
        Instruction.printNode(self.finish);
    }
};

pub const Register = struct {
    number: u32,
};

pub const InstructionName = enum {
    add,
    call,
    getmethod,
    getself,
    loadi,
    move,
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

    fn isJump(self: Instruction) bool {
        _ = self;
        return false;
    }

    fn isReturn(self: Instruction) bool {
        _ = self;
        return false;
    }

    fn isCall(self: Instruction) bool {
        return switch(self) {
            .call, .add => true,
            else => false
        };
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

pub fn compileCFG(bb: *const BasicBlock) []const u32 {
    _ = bb;
    return &[5]u32{1, 2, 3, 4, 5};
}

pub const CompileError = error {
    EmptyInstructionSequence,
};

pub fn buildCFG(allocator: std.mem.Allocator, insns: InstructionList) !* const BasicBlock {
    var node = insns.first;
    var block_name: usize = 0;

    if (node) |unwrap| {
        var current_block = try allocator.create(BasicBlock);

        current_block.* = .{
            .name = block_name,
            .start = unwrap,
            .finish = unwrap,
            .out = null,
            .out2 = null,
        };
        const first_block = current_block;
        block_name += 1;

        while (node) |n| {
            var finish = node;

            Instruction.printNode(n);

            while (finish) |finish_insn| {
                current_block.addInstruction(finish_insn);

                if (finish_insn.data.isJump()) {
                    break;
                }
                finish = finish_insn.next;
            }

            if (finish) |fin| {
                node = fin.next;
            } else {
                break;
            }
        }
        std.debug.print("finish ", .{});
        Instruction.printNode(first_block.finish);
        return first_block;
    } else {
        return CompileError.EmptyInstructionSequence;
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}

test "empty basic block" {
    const list = InstructionList { };
    try std.testing.expectError(error.EmptyInstructionSequence, buildCFG(std.testing.allocator, list));
}

test "basic block one instruction" {
    var list = InstructionList { };
    var one = InstructionList.Node {
        .data = .{ .getself = .{ .out = .{ .number = 0 }}}
    };
    list.append(&one);
    const bb = try buildCFG(std.testing.allocator, list);
    defer std.testing.allocator.destroy(bb);

    try std.testing.expectEqual(&one, bb.start);
    try std.testing.expectEqual(&one, bb.finish);
    try std.testing.expectEqual(null, bb.out);
    try std.testing.expectEqual(null, bb.out2);
}

test "basic block two instruction" {
    var list = InstructionList { };
    var one = InstructionList.Node {
        .data = .{ .getself = .{ .out = .{ .number = 0 }}}
    };
    list.append(&one);

    var two = InstructionList.Node {
        .data = .{ .getmethod = .{
            .out = .{ .number = 0 },
            .recv = .{ .number = 0 },
            .ccid = 123,
        }}
    };
    list.append(&two);
    const bb = try buildCFG(std.testing.allocator, list);
    defer std.testing.allocator.destroy(bb);

    try std.testing.expectEqual(&one, bb.start);
    try std.testing.expectEqual(&two, bb.finish);
    try std.testing.expectEqual(&two, list.last);
    try std.testing.expectEqual(null, bb.out);
    try std.testing.expectEqual(null, bb.out2);
}
