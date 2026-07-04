const std = @import("std");
const Scope = @import("scope.zig").Scope;
const ir = @import("ir.zig");
const Var = ir.Variable;
const BitMap = std.DynamicBitSetUnmanaged;
const CFG = @import("cfg.zig").CFG;

pub const BasicBlock = struct {
    name: u64,
    entry: bool,
    reachable: bool = false,
    has_pcopy: bool = false,
    scope: *Scope,
    insns: ir.InstructionList,
    predecessors: std.ArrayList(*BasicBlock),
    killed_set: BitMap,
    redefined_set: BitMap,
    upward_exposed_set: BitMap,
    liveout_set: BitMap,
    livein_set: BitMap,
    dom: BitMap,
    df: BitMap,
    idom: ?u64 = null,

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
            .killed_set = try BitMap.initEmpty(alloc, 0),
            .redefined_set = try BitMap.initEmpty(alloc, 0),
            .upward_exposed_set = try BitMap.initEmpty(alloc, 0),
            .liveout_set = try BitMap.initEmpty(alloc, 0),
            .livein_set = try BitMap.initEmpty(alloc, 0),
            .dom = try BitMap.initEmpty(alloc, 0),
            .df = try BitMap.initEmpty(alloc, 0),
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

    pub fn resetSets(self: *BasicBlock, vars: usize, alloc: std.mem.Allocator) !void {
        try self.killed_set.resize(alloc, vars, false);
        try self.redefined_set.resize(alloc, vars, false);
        try self.upward_exposed_set.resize(alloc, vars, false);
        try self.liveout_set.resize(alloc, vars, false);
        try self.livein_set.resize(alloc, vars, false);

        self.killed_set.unsetAll();
        self.redefined_set.unsetAll();
        self.upward_exposed_set.unsetAll();
        self.liveout_set.unsetAll();
        self.livein_set.unsetAll();
        self.df.unsetAll();
        self.dom.unsetAll();
    }

    pub fn killedVariableCount(self: *BasicBlock) usize {
        return self.killed_set.count();
    }

    pub fn upwardExposedCount(self: *BasicBlock) usize {
        return self.upward_exposed_set.count();
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

    pub fn makeMov(self: *BasicBlock, out: *Var, in: *Var) !*ir.InstructionListNode {
        return try self.scope.makeMov(out, in);
    }

    pub fn serializeCopyGroups(self: *@This()) !void {
        if (!self.has_pcopy) {
            return;
        }

        var iter = self.instructionIter(.{});
        var current_group: usize = 0xFFFFF;

        var copy_group: std.ArrayList(*ir.InstructionListNode) = .empty;
        defer copy_group.deinit(self.scope.allocator);

        var start: ?*ir.InstructionListNode = null;

        while (iter.next()) |insn| {
            if (insn.data.isPMov()) {
                if (insn.data.pmov.group != current_group) {
                    current_group = insn.data.pmov.group;
                    if (start) |snode| {
                        try self.serializeCopyGroup(snode, snode.data.pmov.group);
                    }
                    start = insn;
                }
                try copy_group.append(self.scope.allocator, insn);
            }
        }

        if (start) |snode| {
            try self.serializeCopyGroup(snode, snode.data.pmov.group);
        }
    }

    pub fn serializeCopyGroup(block: *BasicBlock, insn: *ir.InstructionListNode, group: usize) !void {
        // There shouldn't be any cycles, so we should be able to just insert
        // copies.
        var cursor = insn;
        while (cursor.data.pmov.group == group) {
            const src = cursor.data.pmov.in;
            const dst = cursor.data.pmov.out;
            const old = cursor;

            cursor = @fieldParentPtr("node", cursor.node.next.?);

            const copy = try block.makeMov(dst, src);
            block.replace(old, copy);
            if (@as(ir.InstructionName, cursor.data) != ir.InstructionName.pmov) break;
        }
    }

    fn childLo(_: *BasicBlock, child: *BasicBlock, alloc: std.mem.Allocator) !BitMap {
        const ue = child.upward_exposed_set;
        const lo = child.liveout_set;
        const varkill = child.killed_set;

        // Bitwise NOT the kill list
        var notkill = try varkill.clone(alloc);
        notkill.toggleAll();
        defer notkill.deinit(alloc);

        var lonk = try lo.clone(alloc);
        lonk.setIntersection(notkill);
        defer lonk.deinit(alloc);

        var newlo = try ue.clone(alloc);
        newlo.setUnion(lonk);
        return newlo;
    }

    pub fn insertParallelCopy(self: *BasicBlock, mem: std.mem.Allocator, node: *ir.InstructionListNode, dest: *Var, src: *Var, group: usize) !*ir.InstructionListNode {
        self.has_pcopy = true;

        const new_node = try mem.create(ir.InstructionListNode);
        new_node.* = .{ .node = .{}, .data = .{ .pmov = .{ .out = dest, .in = src, .group = group } } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn appendParallelCopy(self: *BasicBlock, mem: std.mem.Allocator, dest: *Var, src: *Var, group: usize) !*ir.InstructionListNode {
        const last_raw = self.insns.last orelse @panic("appendParallelCopy on empty block");
        var node: *ir.InstructionListNode = @fieldParentPtr("node", last_raw);
        if (node.data.isJump()) {
            node = @fieldParentPtr("node", last_raw.prev.?);
        }

        return self.insertParallelCopy(mem, node, dest, src, group);
    }

    // Update the LiveOut set.  If the set changes, returns true
    pub fn updateLiveOut(self: *BasicBlock, alloc: std.mem.Allocator) !bool {
        // Engineering a compiler, 3rd ed, 8.6, "Defining the Data-Flow Problem" (page 419)
        // Figure 8.15:
        //   LiveOut(B) = union over successors S of
        //     UEVar(S) ∪ (LiveOut(S) − VarKill(S))

        var new_liveout = try BitMap.initEmpty(alloc, self.liveout_set.bit_length);
        defer new_liveout.deinit(alloc);

        for (self.successors()) |maybe| {
            const child = maybe orelse continue;
            var contribution = try self.childLo(child, alloc);
            defer contribution.deinit(alloc);
            new_liveout.setUnion(contribution);
        }

        if (self.liveout_set.eql(new_liveout)) return false;
        self.liveout_set.unsetAll();
        self.liveout_set.setUnion(new_liveout);
        return true;
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

    pub fn fillVarSets(self: *BasicBlock) !void {
        var iter = self.instructionIter(.{});

        while (iter.next()) |insn| {
            // Fill the UE set.
            var opiter = insn.data.opIter();
            while (opiter.next()) |op| {
                // Special case: if this operand is the same as the output of this instruction,
                // it should always be considered upward exposed (use before redefine)

                if (!self.killed_set.isSet(op.id)) {
                    self.upward_exposed_set.set(op.id);
                }
            }

            if (insn.data.outVar()) |v| {
                if (self.killed_set.isSet(v.id)) {
                    self.redefined_set.set(v.id);
                }
                self.killed_set.set(v.id);
            }
        }
    }

    fn addInstruction(self: *BasicBlock, insn: *ir.InstructionListNode) void {
        // Set the definition block for the outvar on the instruction.
        // We need this for phi placement / renaming etc
        if (insn.data.getOut()) |outvar| {
            outvar.setDefinitionBlock(self);
        }
        self.finish = insn;
    }

    pub fn removeInstruction(self: *BasicBlock, mem: std.mem.Allocator, insn: *ir.InstructionListNode) void {
        // Remove the instruction from the global instruction list
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

    pub fn hasPhiFor(self: *BasicBlock, opnd: *Var) bool {
        var iter = self.instructionIter(.{});
        while (iter.next()) |insn| {
            switch (insn.data) {
                .putlabel => {}, // Skip putlabel
                .phi => |p| {
                    if (p.out == opnd) return true;
                },
                else => {
                    return false;
                },
            }
        }
        return false;
    }

    pub fn addPhi(self: *BasicBlock, scope: *Scope, opnd: *Var) !void {
        var iter = self.instructionIter(.{});
        while (iter.next()) |insn| {
            switch (insn.data) {
                inline .phi, .putlabel => {}, // Skip phi and putlabel
                else => {
                    if (insn.node.prev) |prev| {
                        _ = try self.insertPhi(scope.arena.allocator(), @fieldParentPtr("node", prev), opnd);
                    } else {
                        unreachable;
                    }
                    return;
                },
            }
        }
        unreachable;
    }

    fn hasJumpTarget(self: *BasicBlock) bool {
        return self.finish.data.isJump();
    }

    fn hasLabeledEntry(self: *BasicBlock) bool {
        return ir.InstructionName.putlabel == @as(ir.InstructionName, self.start.data);
    }

    pub fn instructionCount(self: *BasicBlock) usize {
        return self.insns.len();
    }

    fn jumpTarget(self: *BasicBlock) ir.Label {
        return self.finish.data.jumpTarget();
    }

    pub fn addPredecessor(self: *BasicBlock, alloc: std.mem.Allocator, predecessor: *BasicBlock) !void {
        try self.predecessors.append(alloc, predecessor);
    }

    pub fn uninitializedSet(self: *BasicBlock, mem: std.mem.Allocator) !BitMap {
        if (!self.entry) return error.ArgumentError;

        var uninit = try self.killed_set.clone(mem);
        uninit.toggleAll();
        uninit.setIntersection(self.liveout_set);
        uninit.setUnion(self.upward_exposed_set);

        return uninit;
    }

    pub fn insertPhi(self: *BasicBlock, mem: std.mem.Allocator, node: *ir.InstructionListNode, op: *Var) !*ir.InstructionListNode {
        const new_node = try mem.create(ir.InstructionListNode);
        new_node.* = .{ .node = .{}, .data = .{ .phi = .{ .out = op, .params = .empty } } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn pushVoidInsn(self: *BasicBlock, mem: std.mem.Allocator, insn: ir.Instruction) !void {
        switch (insn) {
            .jump, .cond, .mov, .setlocal => {},
            else => unreachable,
        }

        const node = try mem.create(ir.InstructionListNode);
        node.* = .{ .node = .{}, .data = insn };
        self.insns.append(&node.node);
    }

    pub fn pushInsn(self: *BasicBlock, mem: std.mem.Allocator, insn: ir.Instruction) !*Var {
        const node = try mem.create(ir.InstructionListNode);
        node.* = .{ .node = .{}, .data = insn };
        self.insns.append(&node.node);

        return switch (insn) {
            .jump => unreachable,
            .cond => unreachable,
            .setlocal => unreachable,
            inline else => |payload| payload.out,
        };
    }

    pub fn removePhi(self: *BasicBlock, mem: std.mem.Allocator) void {
        var iter = self.insns.first;
        while (iter) |insn| {
            iter = insn.next;
            const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
            switch (insn_node.data) {
                .putlabel => {},
                .phi => self.removeInstruction(mem, insn_node),
                else => break,
            }
        }
    }

    pub fn deinit(self: *BasicBlock, alloc: std.mem.Allocator) void {
        var it = self.insns.first;
        while (it) |insn| {
            it = insn.next;
            @as(*ir.InstructionListNode, @fieldParentPtr("node", insn)).data.deinit(alloc);
        }
        self.df.deinit(alloc);
        self.dom.deinit(alloc);
        self.killed_set.deinit(alloc);
        self.redefined_set.deinit(alloc);
        self.upward_exposed_set.deinit(alloc);
        self.liveout_set.deinit(alloc);
        self.livein_set.deinit(alloc);
        self.predecessors.deinit(alloc);
        alloc.destroy(self);
    }
};
