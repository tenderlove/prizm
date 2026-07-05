const std = @import("std");
const cmp = @import("compiler.zig");
const cfg = @import("cfg.zig");
const Scope = @import("scope.zig").Scope;
const BasicBlock = @import("basic_block.zig").BasicBlock;
const BitMap = std.DynamicBitSetUnmanaged;
const Insn = @import("ir.zig").InstructionListNode;

pub const InstructionList = std.DoublyLinkedList;

pub const Variable = struct {
    id: usize,
    source_name: []const u8,

    pub fn initLocal(alloc: std.mem.Allocator, id: usize, source_name: anytype) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .source_name = source_name };
        return opnd;
    }
};

pub const InstructionName = enum {
    call,
    define_method,
    getparam,
    cond,
    jump,
    leave,
    loadi,
    loadstr,
    loadnil,
    phi,
    tst,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        const Self = @This();

        recv: *Insn,
        name: []const u8,
        params: std.ArrayList(*Insn),
    },

    define_method: struct {
        const Self = @This();
        name: []const u8,
        func: *Scope,
    },

    getparam: struct {
        const Self = @This();
        index: usize,
    },

    cond: struct {
        const Self = @This();
        condition: *Insn,
        truthy: *BasicBlock,
        falsy: *BasicBlock,
    },

    jump: struct {
        const Self = @This();
        target: *BasicBlock,
    },

    leave: struct {
        const Self = @This();
        in: *Insn,
    },

    loadi: struct {
        const Self = @This();
        val: u64,
    },

    loadstr: struct {
        const Self = @This();
        val: []const u8,
    },

    loadnil: struct {
        const Self = @This();
    },

    phi: struct {
        const Self = @This();
        params: std.ArrayList(*Insn),
    },

    tst: struct {
        const Self = @This();
        in: *Insn,
    },

    pub fn isJump(self: Instruction) bool {
        return switch (self) {
            .jump, .cond => true,
            else => false,
        };
    }

    pub fn isReturn(self: Instruction) bool {
        return switch (self) {
            .leave => true,
            else => false,
        };
    }

    pub fn isCall(self: Instruction) bool {
        return switch (self) {
            .call => true,
            else => false,
        };
    }

    pub fn isPhi(self: Instruction) bool {
        return switch (self) {
            .phi => true,
            else => false,
        };
    }

    pub fn deinit(self: *Instruction, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .call => |*x| x.params.deinit(alloc),
            .phi => |*x| x.params.deinit(alloc),
            // .define_method carries a *Scope reference; ownership lives in
            // the parent Scope's `children` list, which handles its deinit.
            else => {},
        }
    }
};

pub const InstructionListNode = struct {
    node: std.DoublyLinkedList.Node,
    number: usize = 0,
    data: Instruction,
};

pub const Use = struct {
    node: std.DoublyLinkedList.Node,
    insn: *Instruction,
};

test "can iterate on ops" {
    var out = Variable{
        .id = 0,
        .data = .{ .temp = .{ .id = 0 } },
    };
    var in = Variable{
        .id = 1,
        .data = .{ .temp = .{ .id = 1 } },
    };
    var insn = Instruction{
        .mov = .{
            .out = &out,
            .in = &in,
        },
    };
    var itr = insn.opIter();
    var count: u32 = 0;
    while (itr.next()) |op| {
        try std.testing.expectEqual(1, op.getGlobalId());
        count += 1;
    }
    try std.testing.expectEqual(1, count);
}

test "can iterate on ops with list" {
    var out = Variable{
        .id = 0,
        .data = .{ .temp = .{ .id = 0 } },
    };
    var recv = Variable{
        .id = 1,
        .data = .{ .temp = .{ .id = 1 } },
    };
    const param1 = Variable{
        .id = 3,
        .data = .{ .temp = .{ .id = 3 } },
    };
    const param2 = Variable{
        .id = 4,
        .data = .{ .temp = .{ .id = 4 } },
    };

    var params: std.ArrayList(*Variable) = .empty;
    defer params.deinit(std.testing.allocator);

    try params.append(std.testing.allocator, @constCast(&param1));
    try params.append(std.testing.allocator, @constCast(&param2));

    var insn = Instruction{
        .call = .{
            .out = &out,
            .recv = &recv,
            .name = "test_method",
            .params = params,
        },
    };
    var itr = insn.opIter();
    const expected_ids = [_]usize{ 1, 3, 4 }; // recv, param1, param2 variable IDs (name is now []const u8)
    var var_count: u32 = 0;
    var total_count: u32 = 0;
    while (itr.next()) |op| {
        try std.testing.expectEqual(expected_ids[var_count], op.getGlobalId());
        var_count += 1;
        total_count += 1;
    }
    try std.testing.expectEqual(3, total_count); // Now 3 instead of 4 since name is not an operand
    try std.testing.expectEqual(3, var_count);
}
