const std = @import("std");
const ssa = @import("ssa.zig");
const ir = @import("ir.zig");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const compiler = @import("compiler.zig");
const Scope = compiler.Scope;
const printer = @import("printer.zig");
const assert = @import("std").debug.assert;
const bitmap = @import("utils/bitmap.zig");

pub const CFG = struct {
    arena: std.heap.ArenaAllocator,
    mem: std.mem.Allocator,
    block_name: u32,
    head: *BasicBlock,
    scope: *compiler.Scope,

    pub fn init(mem: std.mem.Allocator, scope: *compiler.Scope) !*CFG {
        // Create an arena
        var arena = std.heap.ArenaAllocator.init(mem);

        const cfg = try arena.allocator().create(CFG);
        const bb = try BasicBlock.initHead(arena.allocator(), 0);

        cfg.* = CFG {
            .arena = arena,
            .mem = mem,
            .block_name = 1,
            .head = bb,
            .scope = scope,
        };

        return cfg;
    }

    pub fn makeBlock(self: *CFG, start: *ir.InstructionList.Node, finish: *ir.InstructionList.Node, entry: bool) !*BasicBlock {
        const block = try BasicBlock.initBlock(self.arena.allocator(),
            self.block_name,
            start,
            finish,
            entry,
            self.scope.nextOpndId());

        self.block_name += 1;

        return block;
    }

    const DepthFirstIterator = struct {
        seen: std.AutoHashMap(u64, *BasicBlock),
        work: std.ArrayList(*BasicBlock),

        pub fn next(self: *DepthFirstIterator) !?*BasicBlock {
            while (self.work.popOrNull()) |bb| {
                if (!self.seen.contains(bb.block.name)) {
                    try self.seen.put(bb.block.name, bb);
                    if (bb.block.out2) |bb2| { try self.work.append(bb2); }
                    if (bb.block.out) |bb1| { try self.work.append(bb1); }
                    return bb;
                }
            }

            return null;
        }

        pub fn deinit(self: *DepthFirstIterator) void {
            self.work.deinit();
            self.seen.deinit();
        }
    };

    pub fn depthFirstIterator(self: *CFG) !DepthFirstIterator {
        var worklist = std.ArrayList(*BasicBlock).init(self.mem);
        try worklist.append(self.head.head.out.?);

        return .{
            .seen = std.AutoHashMap(u64, *BasicBlock).init(self.mem),
            .work = worklist,
        };
    }

    pub fn liveBlockCount(self: *CFG) !usize {
        var dfi = try self.depthFirstIterator();
        defer dfi.deinit();

        var count: usize = 0;
        while (try dfi.next()) |_| {
            count += 1;
        }
        return count;
    }

    pub fn deinit(self: *CFG) void {
        self.arena.deinit();
    }
};

const BasicBlockType = enum {
    head,
    block,
};

pub const BasicBlock = union(BasicBlockType) {
    head: struct {
        name: u64,
        out: ?*BasicBlock = null,
    },

    block: struct {
        name: u64,
        entry: bool,
        start: *ir.InstructionList.Node,
        finish: *ir.InstructionList.Node,
        predecessors: std.ArrayList(*BasicBlock),
        killed_set: *bitmap.BitMap,
        upward_exposed_set: *bitmap.BitMap,
        liveout_set: *bitmap.BitMap,
        out: ?*BasicBlock = null,
        out2: ?*BasicBlock = null,
    },

    fn initHead(alloc: std.mem.Allocator, name: u64) !*BasicBlock {
        const bb = try alloc.create(BasicBlock);

        bb.* = BasicBlock {
            .head = .{
                .name = name,
            }
        };

        return bb;
    }

    fn initBlock(alloc: std.mem.Allocator, name: u64, start: anytype, finish: anytype, entry: bool, vars: usize) !*BasicBlock {
        const block = try alloc.create(BasicBlock);

        block.* = .{
            .block = .{
                .name = name,
                .start = start,
                .finish = finish,
                .entry = entry,
                .killed_set = try bitmap.BitMap.init(alloc, vars),
                .upward_exposed_set = try bitmap.BitMap.init(alloc, vars),
                .liveout_set = try bitmap.BitMap.init(alloc, vars),
                .predecessors = std.ArrayList(*BasicBlock).init(alloc),
            }
        };

        return block;
    }

    pub fn killedVariableCount(self: *BasicBlock) usize {
        return self.block.killed_set.popCount();
    }

    pub fn upwardExposedCount(self: *BasicBlock) usize {
        return self.block.upward_exposed_set.popCount();
    }

    fn childLo(_: *BasicBlock, child: *BasicBlock, alloc: std.mem.Allocator) !*bitmap.BitMap {
        const ue = child.block.upward_exposed_set;
        const lo = child.block.liveout_set;
        const varkill = child.block.killed_set;

        const notkill = try varkill.not(alloc);
        defer alloc.destroy(notkill);

        const lonk = try lo.intersection(notkill, alloc);
        defer alloc.destroy(lonk);

        const newlo = try ue.Union(lonk, alloc);
        return newlo;
    }

    // Update the LiveOut set.  If the set changes, returns true
    pub fn updateLiveOut(self: *BasicBlock, alloc: std.mem.Allocator) !bool {
        // Engineering a compiler, 3rd ed, 8.6, "Defining the Data-Flow Problem" (page 419)
        // Also Figure 8.15
        if (self.block.out) |child1| {
            const newlo = try self.childLo(child1, alloc);
            defer alloc.destroy(newlo);

            if (self.block.out2) |child2| {
                const newlo2 = try self.childLo(child2, alloc);
                defer alloc.destroy(newlo2);

                const bothlo = try newlo.Union(newlo2, alloc);
                defer alloc.destroy(bothlo);

                if (self.block.liveout_set.eq(bothlo)) {
                    return false;
                } else {
                    try self.block.liveout_set.replace(bothlo);
                    return true;
                }
            } else {
                if (self.block.liveout_set.eq(newlo)) {
                    return false;
                } else {
                    try self.block.liveout_set.replace(newlo);
                    return true;
                }
            }
        } else {
            return false;
        }
    }

    const InstructionIter = struct {
        current: ?*ir.InstructionList.Node,
        finish: *ir.InstructionList.Node,
        done: bool,

        pub fn next(self: *InstructionIter) ?*ir.InstructionList.Node {
            if (self.done) return null;

            if (self.current) |node| {
                if (node == self.finish) {
                    self.done = true;
                    return node;
                }

                self.current = node.next;
                return node;
            } else {
                return null;
            }
        }
    };

    pub fn instructionIter(self: *BasicBlock) InstructionIter {
        return .{ .current = self.block.start, .finish = self.block.finish, .done = false };
    }

    pub fn fillVarSets(self: *BasicBlock) !void {
        var iter = self.instructionIter();

        while (iter.next()) |insn| {
            // Full the UE set.
            var opiter = insn.data.opIter();
            while (opiter.next()) |op| {
                // If the operand is a variable, and it _isn't_ part of the kill set,
                // (in other words it hasn't been defined in this BB), then add
                // the operand to the "upward exposed" set.  This means the operand
                // _must_ have been defined in a block that dominates this block.
                if (op.isVariable() and !self.block.killed_set.isBitSet(op.getID())) {
                    try self.block.upward_exposed_set.setBit(op.getID());
                }
            }

            if (insn.data.outVar()) |v| {
                if (v.isVariable()) {
                    try self.block.killed_set.setBit(v.getID());
                }
            }
        }
    }

    fn addInstruction(self: *BasicBlock, insn: *ir.InstructionList.Node) void {
        self.block.finish = insn;
    }

    fn fallsThrough(self: *BasicBlock) bool {
        return switch(self.*) {
            .head => true,
            .block => switch(self.block.finish.data) {
                .jump, .leave => false,
                else => true
            }
        };
    }

    fn hasJumpTarget(self: *BasicBlock) bool {
        if (BasicBlockType.head == @as(BasicBlockType, self.*)) {
            return false;
        } else {
            return self.block.finish.data.isJump();
        }
    }

    fn hasLabeledEntry(self: *BasicBlock) bool {
        if (BasicBlockType.head == @as(BasicBlockType, self.*)) {
            return false;
        } else {
            return ir.InstructionName.putlabel == @as(ir.InstructionName, self.block.start.data);
        }
    }

    fn instructionCount(self: *BasicBlock) u32 {
        if (BasicBlockType.head == @as(BasicBlockType, self.*)) {
            return 0;
        } else {
            var count: u32 = 0;
            var iter = self.instructionIter();

            while(iter.next()) |_| {
                count += 1;
            }

            return count;
        }
    }

    fn jumpTarget(self: *BasicBlock) *ir.Operand {
        if (BasicBlockType.head == @as(BasicBlockType, self.*)) {
            unreachable;
        } else {
            return self.block.finish.data.jumpTarget();
        }
    }

    fn addPredecessor(self: *BasicBlock, predecessor: *BasicBlock) !void {
        try self.block.predecessors.append(predecessor);
    }

    fn addEdge(self: *BasicBlock, child: *BasicBlock) void {
        switch(self.*) {
            .head => {
                if (self.head.out) |_| {
                    unreachable;
                } else {
                    self.head.out = child;
                }
            },
            .block => {
                if (self.block.out) |_| {
                    if (self.block.out2) |_| {
                        unreachable;
                    } else {
                        self.block.out2 = child;
                    }
                } else {
                    self.block.out = child;
                }
            }
        }
    }

    pub fn uninitializedSet(self: *BasicBlock, scope: *Scope, mem: std.mem.Allocator) !*bitmap.BitMap {
        if (!self.block.entry) return error.ArgumentError;

        const uninit = try self.block.killed_set.dup(mem);
        uninit.setNot();
        try uninit.setIntersection(self.block.liveout_set);
        try uninit.setUnion(self.block.upward_exposed_set);

        var biti = uninit.setBitsIterator();
        while (biti.next()) |opnd_id| {
            const op = scope.operands.items[opnd_id];
            // Remove parameters from the uninitialized set. They should
            // be initialized by the caller
            if (op.isParam()) {
                try uninit.unsetBit(opnd_id);
            }
        }
        return uninit;
    }
};

pub const CompileError = error {
    EmptyInstructionSequence,
};

pub fn buildCFG(allocator: std.mem.Allocator, scope: *compiler.Scope) !*CFG {
    const cfg = try CFG.init(allocator, scope);

    var wants_label = std.ArrayList(*BasicBlock).init(allocator);
    defer wants_label.deinit();

    var last_block = cfg.head;

    const insns = scope.insns;
    var node = insns.first;

    // If we don't have any nodes to process, just return the empty CFG.
    if (node == null) {
        return cfg;
    }

    var label_to_block_lut: []?*BasicBlock = try allocator.alloc(?*BasicBlock, scope.label_id);
    @memset(label_to_block_lut, null);
    defer allocator.free(label_to_block_lut);

    var entry = true;

    // For all of our instructions
    while (node) |insn| {
        // Create a new block
        const current_block = try cfg.makeBlock(insn, insn, entry);
        entry = false;

        // Scan through following instructions until we find an instruction
        // that should end the block.
        while (node) |finish_insn| {
            // Add each instruction to the current block
            current_block.addInstruction(finish_insn);

            node = finish_insn.next;

            const insn_node = finish_insn.data;

            // If the last instruction is a jump we should end the block
            if (insn_node.isJump() or insn_node.isReturn()) {
                break;
            }

            // If the next instruction is a label we should end the block
            if (node) |next_insn| {
                if (next_insn.data.isLabel()) {
                    break;
                }
            }
        }

        // If the previous block falls through, then we should add the
        // current block as a outgoing edge, and add the last block
        // as a predecessor to this block.
        if (last_block.fallsThrough()) {
            last_block.addEdge(current_block);
            try current_block.addPredecessor(last_block);
        }

        // If this block has a label at the top, register it so that
        // we can link other blocks to this one
        if (current_block.hasLabeledEntry()) {
            const label_name = current_block.block.start.data.putlabel.name.label.name;
            label_to_block_lut[label_name] = current_block;
        }

        // If this block jumps to a label, register it so that we can link
        // it to the block with the label later.
        if (current_block.hasJumpTarget()) {
            try wants_label.append(current_block);
        }

        last_block = current_block;
    }

    for (wants_label.items) |want_label| {
        const dest_label = want_label.jumpTarget();
        const target = label_to_block_lut[dest_label.label.name].?;
        want_label.addEdge(target);
    }

    // TODO: sweep unreachable blocks?

    // TODO: We could calculate the killed set and the upward exposed set
    // while building the basic blocks, rather than here.  But then we
    // wouldn't have the opportunity to do a peephole optimization step before
    // calculating the VarKilled and UEVars.  Haven't implemented the
    // peephole optimization step yet. Maybe we don't need it and can avoid
    // the extra loops here?
    var iter = try cfg.depthFirstIterator();
    defer iter.deinit();

    while (try iter.next()) |bb| {
        try bb.fillVarSets();
    }


    var changed = true;
    while (changed) {
        var liveout_iter = try cfg.depthFirstIterator();
        defer liveout_iter.deinit();

        changed = false;
        while (try liveout_iter.next()) |bb| {
            if (try bb.updateLiveOut(allocator)) {
                changed = true;
            }
        }
    }

    return cfg;
}

test "empty basic block" {
    const scope = try compiler.Scope.init(std.testing.allocator, 0, null);
    defer scope.deinit();

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(BasicBlockType.head, @as(BasicBlockType, cfg.head.*));
}

test "basic block one instruction" {
    const scope = try compiler.Scope.init(std.testing.allocator, 0, null);
    defer scope.deinit();

    _ = try scope.pushGetself();

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(BasicBlockType.head, @as(BasicBlockType, cfg.head.*));
    const bb = cfg.head.head.out.?;

    try std.testing.expectEqual(scope.insns.first.?, bb.block.start);
    try std.testing.expectEqual(scope.insns.first.?, bb.block.finish);
    try std.testing.expectEqual(null, bb.block.out);
    try std.testing.expectEqual(null, bb.block.out2);
}

test "basic block two instruction" {
    const scope = try compiler.Scope.init(std.testing.allocator, 0, null);
    defer scope.deinit();

    _ = try scope.pushLoadi(123);
    _ = try scope.pushGetself();

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head.head.out.?;

    try std.testing.expectEqual(scope.insns.first.?, bb.block.start);
    try std.testing.expectEqual(scope.insns.last.?, bb.block.finish);
}

test "CFG from compiler" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "5 + 7");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head.head.out.?;
    const start_type: ir.InstructionName = bb.block.start.data;
    try std.testing.expectEqual(ir.InstructionName.loadi, start_type);

    const finish_type: ir.InstructionName = bb.block.finish.data;
    try std.testing.expectEqual(ir.InstructionName.leave, finish_type);
}

test "if statement should have 2 children blocks" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "def foo(x); x ? 1 : 23; end");
    defer scope.deinit();

    // Get the scope for the method
    const method_scope: *compiler.Scope = scope.insns.first.?.data.define_method.func.scope.value;

    const cfg = try buildCFG(allocator, method_scope);
    defer cfg.deinit();

    const block = cfg.head.head.out.?;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.jumpunless,
    }, block);
    try std.testing.expectEqual(block.block.finish, block.block.start);
    try std.testing.expect(block.fallsThrough());
    try std.testing.expectEqual(1, block.instructionCount());

    var child = block.block.out.?;
    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.jump,
    }, child);

    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expect(!child.fallsThrough());

    child = block.block.out2.?;
    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expectEqual(ir.Instruction.putlabel, @as(ir.InstructionName, child.block.start.data));
    try std.testing.expectEqual(ir.Instruction.loadi, @as(ir.InstructionName, child.block.finish.data));

    // Last block via fallthrough then jump
    const last_block = block.block.out.?.block.out.?;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.putlabel,
        ir.Instruction.phi,
        ir.Instruction.leave,
    }, last_block);

    // block via jump then fallthrough
    const last_block2 = block.block.out2.?.block.out.?;
    try std.testing.expectEqual(last_block, last_block2);
}

test "killed operands" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "x = 5");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(1, cfg.liveBlockCount());

    var iter = try cfg.depthFirstIterator();
    defer iter.deinit();

    const bb = (try iter.next()).?;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.mov,
    }, bb);

    try std.testing.expectEqual(2, bb.killedVariableCount());
    try std.testing.expectEqual(0, bb.upwardExposedCount());
}

test "killed operands de-duplicate" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "x = 5; x = 10");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(1, cfg.liveBlockCount());

    var iter = try cfg.depthFirstIterator();
    defer iter.deinit();

    const bb = (try iter.next()).?;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.mov,
        ir.Instruction.loadi,
        ir.Instruction.mov,
    }, bb);

    try std.testing.expectEqual(3, bb.killedVariableCount());
    try std.testing.expectEqual(0, bb.upwardExposedCount());
}

test "upward exposed bits get set" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "def foo(x); x ? x + 1 : 5; end");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func.scope.value;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();
    //printer.printCFG(allocator, method_scope, std.debug);

    const bb = (try findBBWithInsn(methodcfg, ir.InstructionName.call)).?;
    // One for x
    try std.testing.expectEqual(1, bb.upwardExposedCount());
    // one for loadi, and return value of call
    try std.testing.expectEqual(2, bb.killedVariableCount());
}

test "complex loop with if" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code =
\\ def foo y, z
\\   while y
\\     if y < 3
\\       y + 1
\\     else
\\       if y > 5
\\         y + 1
\\       else
\\         z + 1
\\       end
\\     end
\\   end
\\   y
\\ end
;
    const scope = try compileScope(allocator, machine, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func.scope.value;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();
}

test "live out passes through if statement" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code =
\\ def foo y, z
\\   x = z + 5
\\   if y < 123
\\     y = 1
\\   else
\\     y = 2
\\   end
\\   x + y
\\ end
;
    const scope = try compileScope(allocator, machine, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func.scope.value;

    const opnd = try method_scope.getLocalName("x");

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();

    var iter = try methodcfg.depthFirstIterator();
    defer iter.deinit();

    while (try iter.next()) |bb| {
        if (bb.block.out) |_| {
            try std.testing.expect(bb.block.liveout_set.isBitSet(opnd.local.id));
        }
    }
}

fn findBBWithInsn(cfg: *CFG, name: ir.InstructionName) !?*BasicBlock {
    var iter = try cfg.depthFirstIterator();
    defer iter.deinit();

    while (try iter.next()) |bb| {
        var insni = bb.instructionIter();
        while (insni.next()) |insn| {
            if (name == @as(ir.InstructionName, insn.data)) {
                return bb;
            }
        }
    }

    return null;
}

fn findInsn(cfg: *CFG, name: ir.InstructionName) !?*ir.InstructionList.Node {
    var iter = try cfg.depthFirstIterator();
    defer iter.deinit();

    while (try iter.next()) |bb| {
        var insni = bb.instructionIter();
        while (insni.next()) |insn| {
            if (name == @as(ir.InstructionName, insn.data)) {
                return insn;
            }
        }
    }

    return null;
}

fn compileScope(allocator: std.mem.Allocator, machine: *vm.VM, code: []const u8) !*compiler.Scope {
    const parser = try prism.Prism.newParserCtx(allocator);
    defer parser.deinit();
    parser.init(code, code.len, null);
    const root = parser.parse();
    defer parser.nodeDestroy(root);

    var scope_node = try prism.pmNewScopeNode(root);
    const cc = try compiler.init(allocator, machine, parser);
    defer cc.deinit(allocator);
    return try cc.compile(&scope_node);
}

fn expectInstructionList(expected: []const ir.InstructionName, block: *BasicBlock) !void {
    var insn: ?*ir.InstructionList.Node = block.block.start;
    for (expected) |expected_insn| {
        try std.testing.expectEqual(expected_insn, @as(ir.InstructionName, insn.?.data));
        insn = insn.?.next;
    }
}
