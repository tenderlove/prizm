const std = @import("std");
const Scope = @import("scope.zig").Scope;
const BasicBlock = @import("basic_block.zig").BasicBlock;
const Insn = @import("ir.zig").InstructionListNode;

pub const InstructionList = std.DoublyLinkedList;

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
    loadtrue,
    loadfalse,
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

    loadtrue: struct {
        const Self = @This();
    },

    loadfalse: struct {
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
    id: usize,
    data: Instruction,
    // Forwarding alias
    alias: ?*InstructionListNode = null,

    pub fn resolve(self: *InstructionListNode) *InstructionListNode {
        var cur = self;
        while (cur.alias) |a| cur = a;
        var walker = self;
        while (walker.alias) |a| {
            walker.alias = cur;
            walker = a;
        }
        return cur;
    }
};
