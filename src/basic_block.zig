const std = @import("std");
const Scope = @import("scope.zig").Scope;
const ir = @import("ir.zig");
const Insn = ir.InstructionListNode;
const CFG = @import("cfg.zig").CFG;

pub const BasicBlock = struct {
    name: u64,
    entry: bool,
    reachable: bool = false,
    scope: *Scope,
    insns: ir.InstructionList,
    predecessors: std.ArrayList(*BasicBlock),
    sealed: bool = false,
    filled: bool = false,

    pub const DepthFirstIterator = struct {
        seen: std.AutoHashMap(u64, *BasicBlock),
        work: std.ArrayList(*BasicBlock),

        pub fn next(self: *DepthFirstIterator, alloc: std.mem.Allocator) !?*BasicBlock {
            while (self.work.pop()) |bb| {
                if (!self.seen.contains(bb.name)) {
                    try self.seen.put(bb.name, bb);
                    for (bb.successors()) |maybe| {
                        const succ = maybe orelse continue;
                        try self.work.append(alloc, succ);
                    }
                    return bb;
                }
            }

            return null;
        }

        pub fn deinit(self: *DepthFirstIterator, alloc: std.mem.Allocator) void {
            self.work.deinit(alloc);
            self.seen.deinit();
        }
    };

    pub fn initBlock(alloc: std.mem.Allocator, name: u64, scope: *Scope, entry: bool) !*BasicBlock {
        const block = try alloc.create(BasicBlock);

        block.* = .{
            .name = name,
            .entry = entry,
            .scope = scope,
            .insns = ir.InstructionList{},
            .predecessors = .empty,
        };

        return block;
    }

    pub fn depthFirstIterator(self: *BasicBlock, alloc: std.mem.Allocator) !DepthFirstIterator {
        var worklist: std.ArrayList(*BasicBlock) = .empty;
        try worklist.append(alloc, self);

        return .{
            .seen = std.AutoHashMap(u64, *BasicBlock).init(alloc),
            .work = worklist,
        };
    }

    pub fn insnCount(self: *BasicBlock) usize {
        return self.insns.len();
    }

    pub fn startInsn(self: *BasicBlock) ?*ir.InstructionListNode {
        const node = self.insns.first orelse return null;
        return @fieldParentPtr("node", node);
    }

    pub fn finishInsn(self: *BasicBlock) ?*ir.InstructionListNode {
        const node = self.insns.last orelse return null;
        return @fieldParentPtr("node", node);
    }

    pub fn successors(self: *BasicBlock) [2]?*BasicBlock {
        const terminator = self.finishInsn() orelse @panic("successors() on empty block");
        return switch (terminator.data) {
            .cond => |c| .{ c.truthy, c.falsy },
            .jump => |j| .{ j.target, null },
            .leave => .{ null, null },
            else => @panic("last instruction is not a terminator"),
        };
    }

    pub fn replace(self: *BasicBlock, old: *ir.InstructionListNode, new: *ir.InstructionListNode) void {
        self.insns.insertAfter(&old.node, &new.node);
        self.insns.remove(&old.node);
    }

    pub const IteratorOptions = struct {
        direction: Direction = .forward,

        pub const Direction = enum {
            forward,
            reverse,
        };
    };

    pub fn Iterator(comptime options: IteratorOptions) type {
        return struct {
            const Self = @This();

            current: ?*std.DoublyLinkedList.Node,

            pub fn next(self: *Self) ?*ir.InstructionListNode {
                const node = self.current orelse return null;
                switch (options.direction) {
                    .forward => self.current = node.next,
                    .reverse => self.current = node.prev,
                }
                return @fieldParentPtr("node", node);
            }
        };
    }

    pub fn instructionIter(self: *BasicBlock, comptime options: IteratorOptions) Iterator(options) {
        return .{
            .current = switch (options.direction) {
                .forward => self.insns.first,
                .reverse => self.insns.last,
            },
        };
    }

    pub fn removeInstruction(self: *BasicBlock, mem: std.mem.Allocator, insn: *ir.InstructionListNode) void {
        self.insns.remove(&insn.node);
        // Free anything the instruction payload owns (e.g. .phi/.call params ArrayList).
        insn.data.deinit(mem);
    }

    pub fn fallsThrough(self: *BasicBlock) bool {
        return switch (self.finishInsn().?.data) {
            .jump, .leave => false,
            else => true,
        };
    }

    pub fn addPredecessor(self: *BasicBlock, alloc: std.mem.Allocator, predecessor: *BasicBlock) !void {
        try self.predecessors.append(alloc, predecessor);
    }

    pub fn pushVoidInsn(self: *BasicBlock, insn: ir.Instruction) !void {
        std.debug.assert(!self.filled);
        switch (insn) {
            .jump, .cond, => {},
            else => unreachable,
        }

        const node = try self.scope.makeInsn(insn);
        self.insns.append(&node.node);
    }

    pub fn pushInsn(self: *BasicBlock, insn: ir.Instruction) !*Insn {
        std.debug.assert(!self.filled);
        const node = try self.scope.makeInsn(insn);
        self.insns.append(&node.node);
        return node;
    }

    pub fn deinit(self: *BasicBlock, alloc: std.mem.Allocator) void {
        var it = self.insns.first;
        while (it) |insn| {
            it = insn.next;
            @as(*ir.InstructionListNode, @fieldParentPtr("node", insn)).data.deinit(alloc);
        }
        self.predecessors.deinit(alloc);
        alloc.destroy(self);
    }
};
