const std = @import("std");
const ir = @import("ir.zig");
const Var = ir.Variable;
const prism = @import("prism.zig");
const compiler = @import("compiler.zig");
const Scope = @import("scope.zig").Scope;
const Globals = @import("globals.zig").Globals;
const printer = @import("printer.zig");
const BasicBlock = @import("basic_block.zig").BasicBlock;
const assert = @import("std").debug.assert;
const BitMap = std.DynamicBitSetUnmanaged;
const bitmatrix = @import("utils/bitmatrix.zig");
const BitMatrix = bitmatrix.BitMatrix;
const SSADestructor = @import("cfg/ssa_destructor.zig").SSADestructor;
const RegisterAllocator = @import("register_allocator.zig").RegisterAllocator;
const RegisterMapping = @import("register_allocator.zig").RegisterMapping;

pub const CFG = struct {
    pub const State = enum {
        start,
        analyzed,
        phi_placed,
        renamed,
        registers_allocated,
        phi_isolated,
        phi_copies_inserted,
        copy_groups_serialized,
        renamed_variables_removed,
        phi_removed,
    };

    mem: std.mem.Allocator,
    head: *BasicBlock,
    blocks: []const *BasicBlock,
    dom_tree: ?*BitMatrix = null,
    globals: BitMap,
    ssa_destructor: *SSADestructor,
    scope: *Scope,
    state: State = .start,

    pub fn build(mem: std.mem.Allocator, scope: *Scope) !*CFG {
        return try scope.cfg(mem);
    }

    pub fn init(mem: std.mem.Allocator, scope: *Scope, head: *BasicBlock, blocks: []const *BasicBlock) !*CFG {
        const cfg = try mem.create(CFG);

        cfg.* = CFG{
            .mem = mem,
            .head = head,
            .blocks = blocks,
            .scope = scope,
            .globals = try BitMap.initEmpty(mem, 0),
            .ssa_destructor = try SSADestructor.init(mem),
        };

        return cfg;
    }

    pub fn opndCount(self: @This()) usize {
        return self.scope.varCount();
    }

    pub fn depthFirstIterator(self: *CFG, alloc: std.mem.Allocator) !BasicBlock.DepthFirstIterator {
        return try self.head.depthFirstIterator(alloc);
    }

    // Fill in VarKilled and UE var sets for all BBs
    fn fillVarSets(self: *CFG) !void {
        var iter = try self.depthFirstIterator(self.mem);
        defer iter.deinit(self.mem);

        while (try iter.next(self.mem)) |bb| {
            try bb.fillVarSets();
        }
    }

    // Fill in dominance frontier sets on all BBs
    fn fillDominanceFrontiers(_: *CFG, bbs: []?*BasicBlock) !void {
        for (bbs) |maybebb| {
            if (maybebb) |bb| {
                if (bb.entry) continue;

                const idom = bb.idom.?;

                if (bb.predecessors.items.len > 1) {
                    for (bb.predecessors.items) |pred| {
                        var runner = pred;

                        while (runner.name != idom) {
                            runner.df.set(bb.name);
                            const runner_idom = runner.idom.?;
                            runner = bbs[runner_idom].?;
                            assert(runner.name == runner_idom);
                        }
                    }
                }
            }
        }
    }

    // Fill in dominator sets on all BBs
    fn fillDominators(self: *CFG) !void {
        var blocklist: []?*BasicBlock = try self.mem.alloc(?*BasicBlock, self.blockCount());
        @memset(blocklist, null);
        defer self.mem.free(blocklist);

        const list = try self.reversePostorderBlocks(self.mem);
        defer self.mem.free(list);

        // Setup initial dominator bitmaps
        for (list) |maybebb| {
            if (maybebb) |bb| {
                blocklist[bb.name] = bb;

                try bb.df.resize(self.mem, self.blockCount(), false);
                try bb.dom.resize(self.mem, self.blockCount(), false);

                if (bb.entry) {
                    // Entry blocks dominate themselves, and nothing else
                    // dominates an entry block.
                    bb.dom.set(bb.name);
                } else {
                    // Default blocks to "everything dominates this block"
                    bb.dom.toggleAll();
                }
            }
        }

        // Calculate dominators
        var changed = true;
        while (changed) {
            changed = false;

            for (list) |maybebb| {
                if (maybebb) |bb| {
                    if (bb.entry) continue;

                    var temp = try BitMap.initEmpty(self.mem, self.blockCount());
                    defer temp.deinit(self.mem);

                    temp.set(bb.name);

                    var intersect = try BitMap.initEmpty(self.mem, self.blockCount());
                    intersect.toggleAll();
                    defer intersect.deinit(self.mem);

                    for (bb.predecessors.items) |pred| {
                        intersect.setIntersection(pred.dom);
                    }

                    temp.setUnion(intersect);

                    if (!temp.eql(bb.dom)) {
                        bb.dom.unsetAll();
                        bb.dom.setUnion(temp);
                        changed = true;
                    }
                }
            }
        }

        const dom_tree = try BitMatrix.init(self.mem, self.blockCount(), self.blockCount());

        // Set idom on each bb
        for (list) |maybebb| {
            if (maybebb) |bb| {
                if (bb.entry) continue;

                assert(bb.dom.isSet(bb.name));
                // Unset ourselves real quick
                bb.dom.unset(bb.name);

                bb.idom = bb.dom.findLastSet();
                dom_tree.set(bb.idom.?, bb.name);

                // Set ourselves back
                bb.dom.set(bb.name);
            }
        }

        if (self.dom_tree) |dt| {
            dt.deinit(self.mem);
        }
        self.dom_tree = dom_tree;

        try self.fillDominanceFrontiers(blocklist);
    }

    // Fill LiveOut information to all BBs
    pub fn fillLiveOut(self: *CFG) !void {
        var changed = true;
        while (changed) {
            var liveout_iter = try self.depthFirstIterator(self.mem);
            defer liveout_iter.deinit(self.mem);

            changed = false;
            while (try liveout_iter.next(self.mem)) |bb| {
                if (try bb.updateLiveOut(self.mem)) {
                    changed = true;
                }
            }
        }
    }

    fn fillLiveIn(self: *CFG) !void {
        for (self.blocks) |blk| {
            blk.livein_set.unsetAll();
            blk.livein_set.setUnion(blk.upward_exposed_set);

            // Bitwise NOT the "not defined" list
            var not_def = try blk.killed_set.clone(self.mem);
            defer not_def.deinit(self.mem);
            not_def.toggleAll();

            not_def.setIntersection(blk.liveout_set);
            blk.livein_set.setUnion(not_def);
        }
    }

    pub fn sweepUnreachableBlocks(alloc: std.mem.Allocator, head: *BasicBlock, blocks: std.ArrayList(*BasicBlock)) ![]const *BasicBlock {
        for (blocks.items) |block| {
            block.reachable = false;
        }

        var iter = try head.depthFirstIterator(alloc);
        defer iter.dealloc(alloc);

        while (try iter.next(alloc)) |bb| {
            bb.reachable = true;
        }

        var live_blocks: std.ArrayList(*BasicBlock) = .empty;
        defer live_blocks.deinit(alloc);

        for (blocks.items) |block| {
            if (block.reachable) {
                try live_blocks.append(alloc, block);
            }
        }
        return try live_blocks.toOwnedSlice(alloc);
    }

    // Analyze the CFG.
    pub fn analyze(self: *CFG) !void {
        for (self.blocks) |block| {
            assert(block.reachable);
            try block.resetSets(self.scope.nextVarId(), self.mem);
        }

        try self.fillVarSets();
        try self.fillDominators();
        try self.fillLiveOut();
        try self.fillLiveIn();
        self.state = .analyzed;
    }

    pub fn blockList(self: *CFG) []const *BasicBlock {
        return self.blocks;
    }

    pub fn blockCount(self: *CFG) usize {
        return self.blocks.len;
    }

    pub fn isSSA(self: CFG) !bool {
        const allocator = self.mem;
        var seen_opnd = try BitMap.initEmpty(allocator, self.opndCount());
        defer seen_opnd.deinit(allocator);

        for (self.blocks) |block| {
            assert(block.reachable);

            var iter = block.instructionIter(.{});
            while (iter.next()) |insn| {
                if (insn.data.outVar()) |v| {
                    if (seen_opnd.isSet(v.id)) {
                        return false;
                    }
                    seen_opnd.set(v.id);
                }
            }
        }

        return true;
    }

    pub fn replace(self: *CFG, block: *BasicBlock, old: *ir.InstructionListNode, new: *ir.InstructionListNode) void {
        block.replace(self.scope, old, new);
    }

    pub fn placePhis(self: *CFG) !void {
        const opnd_count = self.opndCount();
        try self.globals.resize(self.mem, opnd_count, false);
        var globals = self.globals;

        const all_blocks = self.blockList();

        // Create a matrix where each row in the matrix maps to a particular
        // operand id, and the bits in the row indicate which block number
        // defined that operand.
        const block_set = try BitMatrix.init(self.mem, opnd_count, all_blocks.len);
        defer block_set.deinit(self.mem);

        // Find "global" variables.  Global variables are variables that
        // live between basic blocks.
        // Engineering a Compiler: Figure 9.11
        for (all_blocks) |block| {
            assert(block.reachable);

            // Upward exposed variables must cross between BBs
            globals.setUnion(block.upward_exposed_set);
            globals.setUnion(block.redefined_set);
            var iter = block.killed_set.iterator(.{});
            while (iter.next()) |operand_num| {
                block_set.set(operand_num, block.name);
            }
        }

        // Keeps track of what blocks have phi assignments for particular
        // variables.  This way we can avoid doing a linear scan of
        // the instructions in a block.
        const phi_map = try BitMatrix.init(self.mem, opnd_count, all_blocks.len);
        defer phi_map.deinit(self.mem);

        var global_iter = globals.iterator(.{});
        var worklist: std.ArrayList(*BasicBlock) = .empty;
        defer worklist.deinit(self.mem);

        while (global_iter.next()) |operand_num| {
            // We only want to process a block once per global name
            // This bitmap keeps track of whether or not we've processed
            // a particular block for this name.
            var block_seen = try BitMap.initEmpty(self.mem, all_blocks.len);
            defer block_seen.deinit(self.mem);

            // Get all blocks that define this variable
            const blocks = block_set.getColumn(operand_num);

            const opnd = self.scope.getVariableById(operand_num);

            // Add each block to our worklist
            var biter = blocks.iterator(.{});
            while (biter.next()) |i| {
                const block = all_blocks[i];
                block_seen.set(block.name);
                try worklist.append(self.mem, block);
            }

            while (worklist.pop()) |block| {
                var dfiter = block.df.iterator(.{});
                while (dfiter.next()) |dfi| {
                    const dfblock = all_blocks[dfi];
                    if (!phi_map.isSet(operand_num, dfblock.name)) {
                        // If it's in the liveIn set, we need a phi
                        if (dfblock.livein_set.isSet(operand_num)) {
                            try dfblock.addPhi(self.scope, opnd);
                            phi_map.set(operand_num, dfblock.name);
                        }

                        if (!block_seen.isSet(dfblock.name)) {
                            try worklist.append(self.mem, dfblock);
                            block_seen.set(dfblock.name);
                        }
                    }
                }
            }
        }
        self.state = .phi_placed;
    }

    const Renamer = struct {
        counters: []u64,
        stacks: []std.ArrayList(*Var),
        seen: std.AutoHashMap(usize, usize),

        pub fn init(mem: std.mem.Allocator, global_count: usize) !Renamer {
            const counters: []u64 = try mem.alloc(u64, global_count);
            @memset(counters, 0);
            var stacks: []std.ArrayList(*Var) = try mem.alloc(std.ArrayList(*Var), global_count);

            for (0..global_count) |i| {
                stacks[i] = .empty;
            }

            return .{
                .counters = counters,
                .stacks = stacks,
                .seen = std.AutoHashMap(usize, usize).init(mem),
            };
        }

        pub fn deinit(self: *Renamer, mem: std.mem.Allocator) void {
            mem.free(self.counters);
            for (self.stacks) |*stack| {
                stack.deinit(mem);
            }
            self.seen.deinit();
            mem.free(self.stacks);
        }

        pub fn fillPhiParams(self: *Renamer, mem: std.mem.Allocator, _: *BasicBlock, variable_source_bb: *BasicBlock) !void {
            var iter = variable_source_bb.instructionIter(.{});
            while (iter.next()) |insn| {
                switch (insn.data) {
                    .putlabel => {}, // Skip putlabel
                    .phi => |*p| {
                        const v = insn.data.getOut().?.getVar();
                        if (self.stackTop(v)) |op| {
                            try p.params.append(mem, op);
                        }
                    },
                    // Quit after we've passed phi's
                    else => {
                        return;
                    },
                }
            }
        }

        pub fn rename(self: *Renamer, cfg: *CFG, bb: *BasicBlock) !void {
            var iter = bb.instructionIter(.{});
            var pushed: std.ArrayList(*Var) = .empty;
            defer pushed.deinit(cfg.mem);

            while (iter.next()) |insn| {
                switch (insn.data) {
                    .putlabel => {}, // Skip putlabel
                    .phi => |p| {
                        try pushed.append(cfg.mem, p.out);
                        insn.data.setOut(try self.newName(cfg.mem, p.out, bb, cfg.scope));
                    },
                    else => {
                        var should_append = false;
                        // Rename operands
                        var itr = insn.data.opIter();
                        while (itr.next()) |op| {
                            if (cfg.globals.isSet(op.id)) {
                                insn.data.replaceOpnd(op, self.stackTop(op).?);
                                should_append = true;
                            }
                        }

                        // Rename output variable
                        if (insn.data.getOut()) |out| {
                            if (cfg.globals.isSet(out.id)) {
                                try pushed.append(cfg.mem, out);
                                const newname = try self.newName(cfg.mem, out, bb, cfg.scope);
                                insn.data.setOut(newname);
                                should_append = true;
                            }
                        }
                    },
                }
            }

            // For each successor, fill in the phi parameters
            if (bb.fall_through_dest) |succ| {
                try self.fillPhiParams(cfg.mem, bb, succ);
            }

            if (bb.jump_dest) |succ| {
                try self.fillPhiParams(cfg.mem, bb, succ);
            }

            // For each successor in the dominator tree
            //   rename the successor
            const dt = cfg.dom_tree.?;
            var bititer = dt.getColumn(bb.name).iterator(.{});
            while (bititer.next()) |child_id| {
                try self.rename(cfg, cfg.blocks[child_id]);
            }

            // Pop any pushed variables
            while (pushed.pop()) |op| {
                self.stackPop(op);
            }
        }

        fn stackPop(self: *Renamer, variable: *Var) void {
            if (self.seen.get(variable.id)) |idx| {
                _ = self.stacks[idx].pop();
            } else {
                unreachable;
            }
        }

        fn stackTop(self: *Renamer, variable: *const Var) ?*Var {
            if (self.seen.get(variable.id)) |idx| {
                const stack = self.stacks[idx];
                return stack.items[stack.items.len - 1];
            } else {
                return null;
            }
        }

        fn newName(self: *Renamer, mem: std.mem.Allocator, variable: *Var, bb: *BasicBlock, scope: *Scope) !*Var {
            const idx = try self.getStackIndex(variable);
            const i = self.counters[idx];
            self.counters[idx] = i + 1;
            const new_opnd = try scope.newDefinition(variable, bb, i);
            try self.stacks[idx].append(mem, new_opnd);
            return new_opnd;
        }

        fn getStackIndex(self: *Renamer, variable: *const Var) !usize {
            if (self.seen.get(variable.id)) |idx| {
                return idx;
            } else {
                const idx = self.seen.count();
                try self.seen.put(variable.id, idx);
                return idx;
            }
        }
    };

    pub fn rename(self: *CFG) !void {
        const global_count = self.globals.count();
        var renamer = try Renamer.init(self.mem, global_count);
        defer renamer.deinit(self.mem);
        try renamer.rename(self, self.head);
        self.state = .renamed;
    }

    fn allocateRegisters(self: *CFG) !void {
        const ra = try RegisterAllocator.allocateRegisters(self.mem, self);
        defer ra.deinit();
        self.state = .registers_allocated;
    }

    pub fn isolatePhi(self: *CFG) !void {
        try self.ssa_destructor.isolatePhi(self);
        self.state = .phi_isolated;
    }

    pub fn insertPhiCopies(self: *CFG) !void {
        try self.ssa_destructor.insertPhiCopies(self);
        self.state = .phi_copies_inserted;
    }

    pub fn serializeCopyGroups(self: *CFG) !void {
        try self.ssa_destructor.serializeCopyGroups(self);
        self.state = .copy_groups_serialized;
    }

    pub fn removeRenamedVariables(self: *CFG) !void {
        try self.ssa_destructor.renameAllVariables(self);
        self.state = .renamed_variables_removed;
    }

    pub fn removeUselessCopies(self: *CFG) void {
        for (self.blocks) |bb| {
            assert(bb.reachable);

            var iter: ?*std.DoublyLinkedList.Node = &bb.start.node;

            while (iter) |insn| {
                const next_insn = insn.next; // Save next before potentially removing current
                const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);

                switch (insn_node.data) {
                    .getparam => {
                        bb.removeInstruction(insn_node, self.scope);
                    },
                    .mov => |mov| {
                        if (mov.out == mov.in) {
                            bb.removeInstruction(insn_node, self.scope);
                        }
                    },
                    .setparam => |sp| {
                        if (sp.out == sp.in) {
                            bb.removeInstruction(insn_node, self.scope);
                        }
                    },
                    else => {},
                }

                if (insn == &bb.finish.node) break;
                iter = next_insn;
            }
        }
    }

    pub fn removePhi(self: *CFG) !void {
        for (self.blocks) |bb| {
            if (!bb.reachable) continue;
            bb.removePhi(self.scope.allocator);
        }
        self.state = .phi_removed;
    }

    pub fn destructSSA(self: *CFG) !void {
        if (self.state != .renamed) return error.CompilationError;

        while (self.state != .phi_removed) {
            _ = try self.nextCompileStep();
        }
    }

    fn traverseReversePostorder(blk: ?*BasicBlock, seen: *BitMap, list: []?*BasicBlock, counter: *usize) !void {
        if (blk) |bb| {
            if (seen.isSet(bb.name)) return;
            seen.set(bb.name);
            try traverseReversePostorder(bb.fall_through_dest, seen, list, counter);
            try traverseReversePostorder(bb.jump_dest, seen, list, counter);

            // Decrement the counter so we set the block in reverse post order
            counter.* -= 1;
            list[counter.*] = bb;
        }
    }

    pub fn reversePostorderBlocks(self: *CFG, mem: std.mem.Allocator) ![]?*BasicBlock {
        const blocklist: []?*BasicBlock = try mem.alloc(?*BasicBlock, self.blockCount());
        @memset(blocklist, null);

        var seen = try BitMap.initEmpty(mem, self.blockCount());
        defer seen.deinit(mem);
        var counter = self.blockCount();

        try traverseReversePostorder(self.head, &seen, blocklist, &counter);

        return blocklist;
    }

    pub fn liveBlockCount(self: *CFG) !usize {
        var dfi = try self.depthFirstIterator(self.mem);
        defer dfi.deinit(self.mem);

        var count: usize = 0;
        while (try dfi.next(self.mem)) |_| {
            count += 1;
        }
        return count;
    }

    pub fn deinit(self: *CFG) void {
        self.ssa_destructor.deinit();
        for (self.blocks) |blk| {
            blk.deinit(self.mem);
        }
        if (self.dom_tree) |dt| {
            dt.deinit(self.mem);
        }
        self.globals.deinit(self.mem);
        self.mem.free(self.blocks);
        self.mem.destroy(self);
    }

    pub fn nextCompileStep(self: *CFG) !CFG.State {
        switch (self.state) {
            .start => {
                try self.analyze();
            },
            .analyzed => {
                try self.placePhis();
            },
            .phi_placed => {
                try self.rename();
            },
            .renamed => {
                try self.allocateRegisters();
            },
            .registers_allocated => {
                try self.isolatePhi();
            },
            .phi_isolated => {
                try self.insertPhiCopies();
            },
            .phi_copies_inserted => {
                try self.serializeCopyGroups();
            },
            .copy_groups_serialized => {
                try self.removeRenamedVariables();
            },
            .renamed_variables_removed => {
                try self.removePhi();
            },
            .phi_removed => {},
        }
        return self.state;
    }

    pub fn compileUntil(self: *CFG, state: CFG.State) !void {
        while (self.state != state) {
            _ = try self.nextCompileStep();
        }
    }
};

pub const CompileError = error{
    EmptyInstructionSequence,
};

fn buildCFG(mem: std.mem.Allocator, scope: *Scope) !*CFG {
    return try scope.cfg(mem);
}

test "basic block one instruction" {
    const scope = try Scope.init(std.testing.allocator, 0, "empty", null);
    defer scope.deinit();

    _ = try scope.pushLoadi(null, 123);

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;

    try std.testing.expectEqual(1, bb.instructionCount());
}

test "CFG from compiler" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals, "5 + 7");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;
    const start_type: ir.InstructionName = bb.startInsn().?.data;
    try std.testing.expectEqual(ir.InstructionName.getparam, start_type);

    const finish_type: ir.InstructionName = bb.finishInsn().?.data;
    try std.testing.expectEqual(ir.InstructionName.leave, finish_type);
}

test "no uninitialized in ternary" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals, "x ? 1 : 23");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    var uninitialized = try cfg.head.uninitializedSet(allocator);
    defer uninitialized.deinit(allocator);

    try std.testing.expectEqual(0, uninitialized.count());
}

test "if statement should have 2 children blocks" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals, "def foo(x); x ? 1 : 23; end");
    defer scope.deinit();

    // Get the scope for the method
    const scopes = scope.childScopes();

    const method_scope = scopes.items[0];

    const cfg = try buildCFG(allocator, method_scope);
    defer cfg.deinit();

    const block = cfg.head;

    try expectInstructionList(&[_]ir.InstructionName{
        ir.Instruction.getparam, // self
        ir.Instruction.mov,
        ir.Instruction.getparam, // x
        ir.Instruction.mov,
        ir.Instruction.jumpunless,
    }, block);
    try std.testing.expectEqual(5, block.instructionCount());
    try std.testing.expect(block.fallsThrough());

    var child = block.fall_through_dest.?;
    try expectInstructionList(&[_]ir.InstructionName{
        ir.Instruction.loadi,
        ir.Instruction.jump,
    }, child);

    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expect(!child.fallsThrough());

    child = block.jump_dest.?;
    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expectEqual(ir.Instruction.putlabel, @as(ir.InstructionName, child.startInsn().?.data));
    try std.testing.expectEqual(ir.Instruction.loadi, @as(ir.InstructionName, child.finishInsn().?.data));

    // Last block via fallthrough then jump
    const last_block = block.fall_through_dest.?.jump_dest.?;

    try expectInstructionList(&[_]ir.InstructionName{
        ir.Instruction.putlabel,
        ir.Instruction.phi,
        ir.Instruction.leave,
    }, last_block);

    // block via jump then fallthrough
    const last_block2 = block.jump_dest.?.fall_through_dest.?;
    try std.testing.expectEqual(last_block, last_block2);
}

test "killed operands" {
    const alloc = std.testing.allocator;

    const globals = try Globals.init(alloc);
    defer globals.deinit(alloc);

    const scope = try compileScope(alloc, globals, "x = 5");
    defer scope.deinit();

    const cfg = try buildCFG(alloc, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(1, cfg.liveBlockCount());

    var iter = try cfg.depthFirstIterator(alloc);
    defer iter.deinit(alloc);

    const bb = (try iter.next(alloc)).?;

    try expectInstructionList(&[_]ir.InstructionName{
        ir.Instruction.getparam,
        ir.Instruction.mov,
        ir.Instruction.loadi,
    }, bb);

    try std.testing.expectEqual(4, bb.killedVariableCount());
    try std.testing.expectEqual(0, bb.upwardExposedCount());
}

test "killed operands de-duplicate" {
    const alloc = std.testing.allocator;

    const globals = try Globals.init(alloc);
    defer globals.deinit(alloc);

    const scope = try compileScope(alloc, globals, "x = 5; x = 10");
    defer scope.deinit();

    const cfg = try buildCFG(alloc, scope);
    defer cfg.deinit();

    try std.testing.expectEqual(1, cfg.liveBlockCount());

    var iter = try cfg.depthFirstIterator(alloc);
    defer iter.deinit(alloc);

    const bb = (try iter.next(alloc)).?;

    try expectInstructionList(&[_]ir.InstructionName{
        ir.Instruction.getparam,
        ir.Instruction.mov,
        ir.Instruction.loadi,
        ir.Instruction.loadi,
    }, bb);

    try std.testing.expectEqual(4, bb.killedVariableCount());
    try std.testing.expectEqual(0, bb.upwardExposedCount());
}

test "upward exposed bits get set" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals, "def foo(x); x ? x + 1 : 5; end");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();
    //printer.printCFG(allocator, method_scope, std.debug);

    const bb = (try findBBWithInsn(methodcfg, ir.InstructionName.call)).?;
    // One for x
    try std.testing.expectEqual(1, bb.upwardExposedCount());
    // setparam for x, loadi, setparam for 1, and return value of call
    try std.testing.expectEqual(5, bb.killedVariableCount());
}

test "complex loop with if" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

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
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();
}

test "live out passes through if statement" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

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
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;

    const opnd = try method_scope.getLocalName("x");

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();

    var iter = try methodcfg.depthFirstIterator(allocator);
    defer iter.deinit(allocator);

    while (try iter.next(allocator)) |bb| {
        if (bb.fall_through_dest) |_| {
            try std.testing.expect(bb.liveout_set.isSet(opnd.id));
        }
    }
}

test "jumps targets get predecessors" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code =
        \\ def foo y
        \\   if y < 3
        \\     y + 1
        \\   else
        \\     y + 4
        \\   end
        \\ end
    ;
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();

    // We should have 4 blocks
    const blocks = methodcfg.blockList();

    try std.testing.expectEqual(4, blocks.len);
    try std.testing.expectEqual(0, blocks[0].predecessors.items.len);
    try std.testing.expectEqual(1, blocks[1].predecessors.items.len);
    try std.testing.expectEqual(1, blocks[2].predecessors.items.len);
    try std.testing.expectEqual(2, blocks[3].predecessors.items.len);

    // Block 1 should point at block 0
    try std.testing.expectEqual(blocks[0], blocks[1].predecessors.items[0]);

    // Block 2 should point at block 0
    try std.testing.expectEqual(blocks[0], blocks[2].predecessors.items[0]);

    // Block 3 should point at block 1 and 2
    const preds = blocks[3].predecessors.items;

    try std.testing.expect(preds[0] == blocks[1] or preds[1] == blocks[1]);
    try std.testing.expect(preds[0] == blocks[2] or preds[1] == blocks[2]);
}

test "blocks have dominators" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code =
        \\ def foo y
        \\   if y < 3
        \\     y + 1
        \\   else
        \\     y + 4
        \\   end
        \\ end
    ;
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const insn = (try findInsn(cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;

    const methodcfg = try buildCFG(allocator, method_scope);
    defer methodcfg.deinit();

    const blocks = methodcfg.blockList();

    // We should have 4 blocks
    try std.testing.expectEqual(4, blocks.len);
    try std.testing.expect(blocks[0].entry);

    // Entry dominates itself
    try std.testing.expectEqual(1, blocks[0].dom.count());
    try std.testing.expect(blocks[0].dom.isSet(0));
    try std.testing.expectEqual(null, blocks[0].idom);

    // BB1 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.count());
    try std.testing.expect(blocks[1].dom.isSet(0));
    try std.testing.expect(blocks[1].dom.isSet(1));
    try std.testing.expectEqual(0, blocks[1].idom.?);

    // BB2 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.count());
    try std.testing.expect(blocks[2].dom.isSet(0));
    try std.testing.expect(blocks[2].dom.isSet(2));
    try std.testing.expectEqual(0, blocks[1].idom.?);

    // BB3 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.count());
    try std.testing.expect(blocks[3].dom.isSet(0));
    try std.testing.expect(blocks[3].dom.isSet(3));
    try std.testing.expectEqual(0, blocks[1].idom.?);
}

test "while loop dominators" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code =
        \\ a = 1
        \\ while a
        \\   a = a + 1
        \\   if a > 100
        \\     z = a - 1
        \\   else
        \\     z = a + 1
        \\   end
        \\   puts z
        \\ end
        \\ puts a
    ;
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();

    // We should have 7 blocks
    try std.testing.expectEqual(7, blocks.len);
    try std.testing.expectEqual(6, blocks[6].name);
    try std.testing.expect(blocks[6].dom.isSet(6));
}

test "method definitions" {
    const mem = std.testing.allocator;

    const globals = try Globals.init(mem);
    defer globals.deinit(mem);

    const code =
        \\ def foo
        \\   1234
        \\ end
        \\ foo
    ;
    const scope = try compileScope(mem, globals, code);
    defer scope.deinit();

    const children = scope.childScopes();
    try std.testing.expectEqual(1, children.items.len);

    // Get a CFG for the foo method
    const cfg = try CFG.build(mem, children.items[0]);
    try cfg.compileUntil(.phi_removed);
    defer cfg.deinit();

    const blocks = cfg.blockList();

    try std.testing.expectEqual(1, blocks.len);
}

test "dead code removal" {
    const mem = std.testing.allocator;

    const globals = try Globals.init(mem);
    defer globals.deinit(mem);

    const code =
        \\ def foo
        \\   return 1234
        \\   puts 456
        \\ end
    ;
    const scope = try compileScope(mem, globals, code);
    defer scope.deinit();

    const children = scope.childScopes();
    try std.testing.expectEqual(1, children.items.len);

    const method_scope = children.items[0];
    // Get a CFG for the foo method (includes setparam instructions now)
    try std.testing.expectEqual(10, method_scope.insnCount());

    const cfg = try CFG.build(mem, method_scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();
    // CFG.build now automatically sweeps unreachable blocks, so we should only have 1 reachable block
    try std.testing.expectEqual(1, blocks.len);

    // Number all instructions
    method_scope.numberAllInstructions();

    try std.testing.expectEqual(4, method_scope.insnCount());
}

test "dominance frontiers" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();

    // CFG.build now automatically sweeps unreachable blocks, so we have 8 blocks instead of 9
    try std.testing.expectEqual(8, blocks.len);
    try std.testing.expectEqual(6, blocks[6].name);
    try std.testing.expect(blocks[1].df.isSet(1));
    try std.testing.expect(blocks[2].df.isSet(7));
    try std.testing.expect(blocks[3].df.isSet(7));
    try std.testing.expect(blocks[4].df.isSet(6));
    try std.testing.expect(blocks[5].df.isSet(6));
    try std.testing.expect(blocks[6].df.isSet(7));
    try std.testing.expect(blocks[7].df.isSet(1));

    // All remaining blocks should be reachable since unreachable ones were swept
    for (0..blocks.len) |i| {
        try std.testing.expect(blocks[i].reachable);
    }
    try std.testing.expectEqual(0, blocks[0].df.count());
}

test "dominator tree" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const dt = cfg.dom_tree.?;

    // Block 0 should point to 1
    try std.testing.expect(dt.isSet(0, 1));
    try std.testing.expectEqual(1, dt.getColumn(0).count());

    try std.testing.expect(dt.isSet(1, 2));
    try std.testing.expect(dt.isSet(1, 3));
    try std.testing.expect(dt.isSet(1, 7));
    try std.testing.expectEqual(3, dt.getColumn(1).count());

    try std.testing.expectEqual(0, dt.getColumn(2).count());

    try std.testing.expect(dt.isSet(3, 4));
    try std.testing.expect(dt.isSet(3, 5));
    try std.testing.expect(dt.isSet(3, 6));
    try std.testing.expectEqual(3, dt.getColumn(3).count());

    try std.testing.expectEqual(0, dt.getColumn(4).count());
    try std.testing.expectEqual(0, dt.getColumn(5).count());
    try std.testing.expectEqual(0, dt.getColumn(6).count());
    try std.testing.expectEqual(0, dt.getColumn(7).count());
}

test "rename" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    try std.testing.expect(!(try cfg.isSSA()));
    try cfg.rename();

    // After renaming, all assignments should be unique
    try std.testing.expect(try cfg.isSSA());
}

test "rename with calls" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const code =
        \\ b = 6
        \\ c = 7
        \\ b += 123
        \\ b + c
    ;
    const scope = try compileScope(allocator, globals, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    try std.testing.expect(!(try cfg.isSSA()));
    try cfg.rename();

    // After renaming, all assignments should be unique
    try std.testing.expect(try cfg.isSSA());
}

fn complexExampleCFG() []const u8 {
    const code =
        \\ i = 1
        \\ while true
        \\   a = i
        \\   c = i + 3
        \\ 
        \\   if c > 5
        \\     b = 15
        \\     c = i + 5
        \\     d = 16
        \\   else
        \\     a = 789
        \\     d = 222
        \\ 
        \\     if c < 2
        \\       d = 111
        \\     else
        \\       c = 15
        \\     end
        \\ 
        \\     b = 888
        \\   end
        \\ 
        \\   y = a + b
        \\   z = c + d
        \\   i = i + 1
        \\ end
    ;
    return code;
}

fn findBBWithInsn(cfg: *CFG, name: ir.InstructionName) !?*BasicBlock {
    var iter = try cfg.depthFirstIterator(cfg.mem);
    defer iter.deinit(cfg.mem);

    while (try iter.next(cfg.mem)) |bb| {
        var insni = bb.instructionIter(.{});
        while (insni.next()) |insn| {
            if (name == @as(ir.InstructionName, insn.data)) {
                return bb;
            }
        }
    }

    return null;
}

fn findInsn(cfg: *CFG, name: ir.InstructionName) !?*ir.InstructionListNode {
    var iter = try cfg.depthFirstIterator(cfg.mem);
    defer iter.deinit(cfg.mem);

    while (try iter.next(cfg.mem)) |bb| {
        var insni = bb.instructionIter(.{});
        while (insni.next()) |insn| {
            if (name == @as(ir.InstructionName, insn.data)) {
                return insn;
            }
        }
    }

    return null;
}

fn compileScope(allocator: std.mem.Allocator, globals: *Globals, code: []const u8) !*Scope {
    const parser = try prism.Prism.newParserCtx(allocator);
    defer parser.deinit();
    parser.init(code, code.len, null);
    const root = parser.parse();
    defer parser.nodeDestroy(root);

    var scope_node = try prism.pmNewScopeNode(root);
    const cc = try compiler.init(allocator, globals, parser);
    defer cc.deinit(allocator);
    return try cc.compile(&scope_node);
}

fn expectInstructionList(expected: []const ir.InstructionName, block: *BasicBlock) !void {
    var insn: ?*ir.InstructionListNode = block.startInsn();
    for (expected) |expected_insn| {
        try std.testing.expectEqual(expected_insn, @as(ir.InstructionName, insn.?.data));
        if (insn.?.node.next) |n| {
            insn = @fieldParentPtr("node", n);
        } else {
            insn = null;
        }
    }
}
