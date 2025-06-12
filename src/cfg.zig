const std = @import("std");
const ir = @import("ir.zig");
const Operand = ir.Operand;
const Op = ir.Operand;
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const compiler = @import("compiler.zig");
const Scope = @import("scope.zig").Scope;
const printer = @import("printer.zig");
const assert = @import("std").debug.assert;
const bitmap = @import("utils/bitmap.zig");
const BitMap = bitmap.BitMap;
const bitmatrix = @import("utils/bitmatrix.zig");
const BitMatrix = bitmatrix.BitMatrix;
const SSADestructor = @import("cfg/ssa_destructor.zig").SSADestructor;

pub const CFG = struct {
    pub const State = enum {
        start,
        analyzed,
        phi_placed,
        renamed,
        phi_isolated,
        phi_copies_inserted,
        copy_groups_serialized,
        renamed_variables_removed,
        phi_removed,
    };

    arena: std.heap.ArenaAllocator,
    mem: std.mem.Allocator,
    head: *BasicBlock,
    blocks: []const *BasicBlock,
    dom_tree: ?*BitMatrix = null,
    globals: ?*BitMap = null,
    ssa_destructor: *SSADestructor,
    scope: *Scope,
    state: State = .start,

    pub fn init(mem: std.mem.Allocator, arena: std.heap.ArenaAllocator, scope: *Scope, head: *BasicBlock, blocks: []const *BasicBlock) !*CFG {
        const cfg = try mem.create(CFG);

        cfg.* = CFG {
            .arena = arena,
            .mem = mem,
            .head = head,
            .blocks = blocks,
            .scope = scope,
            .ssa_destructor = try SSADestructor.init(mem),
        };

        return cfg;
    }

    pub fn opndCount(self: @This()) usize {
        return self.scope.opndCount();
    }

    const DepthFirstIterator = struct {
        seen: std.AutoHashMap(u64, *BasicBlock),
        work: std.ArrayList(*BasicBlock),

        pub fn next(self: *DepthFirstIterator) !?*BasicBlock {
            while (self.work.pop()) |bb| {
                if (!self.seen.contains(bb.name)) {
                    try self.seen.put(bb.name, bb);
                    if (bb.jump_dest) |bb2| { try self.work.append(bb2); }
                    if (bb.fall_through_dest) |bb1| { try self.work.append(bb1); }
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
        try worklist.append(self.head);

        return .{
            .seen = std.AutoHashMap(u64, *BasicBlock).init(self.mem),
            .work = worklist,
        };
    }

    // Fill in VarKilled and UE var sets for all BBs
    fn fillVarSets(self: *CFG) !void {
        var iter = try self.depthFirstIterator();
        defer iter.deinit();

        while (try iter.next()) |bb| {
            bb.reachable = true;
            try bb.fillVarSets();
        }
    }

    // Fill in dominance frontier sets on all BBs
    fn fillDominanceFrontiers(_: *CFG, bbs: []?* BasicBlock) !void {
        for (bbs) |maybebb| {
            if (maybebb) |bb| {
                if (bb.entry) continue;

                const idom = bb.idom.?;

                if (bb.predecessors.items.len > 1) {
                    for (bb.predecessors.items) |pred| {
                        var runner = pred;

                        while (runner.name != idom) {
                            runner.df.?.set(bb.name);
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

                bb.df = try BitMap.initEmpty(self.arena.allocator(), self.blockCount());

                if (bb.entry) {
                    bb.dom = try BitMap.initEmpty(self.arena.allocator(), self.blockCount());
                    // Entry blocks dominate themselves, and nothing else
                    // dominates an entry block.
                    bb.dom.?.set(bb.name);
                } else {
                    // Default blocks to "everything dominates this block"
                    bb.dom = try BitMap.initEmpty(self.arena.allocator(), self.blockCount());
                    bb.dom.?.toggleAll();
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

                    const temp = try BitMap.initEmpty(self.mem, self.blockCount());
                    defer self.mem.destroy(temp);

                    temp.set(bb.name);

                    const intersect = try BitMap.initEmpty(self.mem, self.blockCount());
                    intersect.toggleAll();
                    defer self.mem.destroy(intersect);

                    for (bb.predecessors.items) |pred| {
                        intersect.setIntersection(pred.dom.?);
                    }

                    temp.setUnion(intersect);

                    if (!temp.eq(bb.dom.?)) {
                        try bb.dom.?.replace(temp);
                        changed = true;
                    }
                }
            }
        }

        const dom_tree = try BitMatrix.init(self.arena.allocator(), self.blockCount(), self.blockCount());

        // Set idom on each bb
        for (list) |maybebb| {
            if (maybebb) |bb| {
                if (bb.entry) continue;

                assert(bb.dom.?.isSet(bb.name));
                // Unset ourselves real quick
                bb.dom.?.unset(bb.name);

                bb.idom = bb.dom.?.fsb();
                dom_tree.set(bb.idom.?, bb.name);

                // Set ourselves back
                bb.dom.?.set(bb.name);
            }
        }

        self.dom_tree = dom_tree;

        try self.fillDominanceFrontiers(blocklist);
    }

    // Fill LiveOut information to all BBs
    pub fn fillLiveOut(self: *CFG) !void {
        var changed = true;
        while (changed) {
            var liveout_iter = try self.depthFirstIterator();
            defer liveout_iter.deinit();

            changed = false;
            while (try liveout_iter.next()) |bb| {
                if (try bb.updateLiveOut(self.mem)) {
                    changed = true;
                }
            }
        }
    }

    fn fillLiveIn(self: *CFG) !void {
        for (self.blocks) |blk| {
            try blk.livein_set.replace(blk.upward_exposed_set);

            // Bitwise NOT the "not defined" list
            var not_def = try blk.killed_set.clone(self.mem);
            defer not_def.deinit(self.mem);
            not_def.toggleAll();

            not_def.setIntersection(blk.liveout_set);
            blk.livein_set.setUnion(not_def);
        }
    }

    // Analyze the CFG.
    pub fn analyze(self: *CFG) !void {
        for (self.blocks) |block| {
            block.reachable = false;
            try block.resetSets(self.scope.nextOpndId(), self.arena.allocator());
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

    pub fn makeMov(self: *CFG, out: *Op, in: *Op) !*ir.InstructionList.Node {
        return try self.scope.makeMov(out, in);
    }

    pub fn isSSA(self: CFG) !bool {
        const allocator = self.mem;
        const seen_opnd = try BitMap.initEmpty(allocator, self.opndCount());
        defer seen_opnd.deinit(allocator);

        for (self.blocks) |block| {
            if (!block.reachable) continue;

            var iter = block.instructionIter();
            while (iter.next()) |insn| {
                if (insn.data.outVar()) |v| {
                    if (seen_opnd.isSet(v.getID())) {
                        return false;
                    }
                    seen_opnd.set(v.getID());
                }
            }
        }

        return true;
    }

    pub fn replace(self: *CFG, block: *BasicBlock, old: *ir.InstructionList.Node, new: *ir.InstructionList.Node) void {
        block.replace(self.scope, old, new);
    }

    pub fn placePhis(self: *CFG) !void {
        const opnd_count = self.opndCount();
        const globals = try BitMap.initEmpty(self.arena.allocator(), opnd_count);

        const all_blocks = self.blockList();

        // Create a matrix where each row in the matrix maps to a particular
        // operand id, and the bits in the row indicate which block number
        // defined that operand.
        const block_set = try BitMatrix.init(self.arena.allocator(), opnd_count, all_blocks.len);

        // Find "global" variables.  Global variables are variables that
        // live between basic blocks.
        // Engineering a Compiler: Figure 9.11
        for (all_blocks) |block| {
            if (block.reachable) {
                // Upward exposed variables must cross between BBs
                globals.setUnion(block.upward_exposed_set);
                var iter = block.killed_set.iterator(.{});
                while (iter.next()) |operand_num| {
                    block_set.set(operand_num, block.name);
                }
            }
        }

        // Keeps track of what blocks have phi assignments for particular
        // variables.  This way we can avoid doing a linear scan of
        // the instructions in a block.
        const phi_map = try BitMatrix.init(self.arena.allocator(), opnd_count, all_blocks.len);

        var global_iter = globals.iterator(.{});
        var worklist = std.ArrayList(*BasicBlock).init(self.mem);
        defer worklist.deinit();

        while (global_iter.next()) |operand_num| {
            // We only want to process a block once per global name
            // This bitmap keeps track of whether or not we've processed
            // a particular block for this name.
            const block_seen = try BitMap.initEmpty(self.mem, all_blocks.len);
            defer block_seen.deinit(self.mem);

            // Get all blocks that define this variable
            const blocks = block_set.getColumn(operand_num);

            const opnd = self.scope.getOperandById(operand_num);

            // Add each block to our worklist
            var biter = blocks.iterator(.{});
            while (biter.next()) |i| {
                const block = all_blocks[i];
                block_seen.set(block.name);
                try worklist.append(block);
            }

            while (worklist.pop()) |block| {
                var dfiter = block.df.?.iterator(.{});
                while (dfiter.next()) |dfi| {
                    const dfblock = all_blocks[dfi];
                    if (!phi_map.isSet(operand_num, dfblock.name)) {
                        // If it's in the liveIn set, we need a phi
                        if (dfblock.livein_set.isSet(operand_num)) {
                            try dfblock.addPhi(self.scope, opnd);
                            phi_map.set(operand_num, dfblock.name);
                        }

                        if (!block_seen.isSet(dfblock.name)) {
                            try worklist.append(dfblock);
                            block_seen.set(dfblock.name);
                        }
                    }
                }
            }
        }
        self.globals = globals;
        self.state = .phi_placed;
    }

    const Renamer = struct {
        counters: []u64,
        stacks: []std.ArrayList(*Operand),
        seen: std.AutoHashMap(usize, usize),

        pub fn init(mem: std.mem.Allocator, global_count: usize) !Renamer {
            const counters: []u64 = try mem.alloc(u64, global_count);
            @memset(counters, 0);
            var stacks: []std.ArrayList(*Operand) = try mem.alloc(std.ArrayList(*Operand), global_count);

            for (0..global_count) |i| {
                stacks[i] = std.ArrayList(*Operand).init(mem);
            }

            return .{
                .counters = counters,
                .stacks = stacks,
                .seen = std.AutoHashMap(usize, usize).init(mem),
            };
        }

        pub fn deinit(self: *Renamer, mem: std.mem.Allocator) void {
            mem.free(self.counters);
            for (self.stacks) |stack| {
                stack.deinit();
            }
            self.seen.deinit();
            mem.free(self.stacks);
        }

        pub fn fillPhiParams(self: *Renamer, _: *BasicBlock, variable_source_bb: *BasicBlock) !void {
            var iter = variable_source_bb.instructionIter();
            while (iter.next()) |insn| {
                switch(insn.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => |*p| {
                        const v = insn.data.getOut().?.getVar();
                        if (self.stackTop(v)) |op| {
                            try p.params.append(op);
                        }
                    },
                    // Quit after we've passed phi's
                    else => { return; }
                }
            }
        }

        pub fn rename(self: *Renamer, cfg: *CFG, bb: *BasicBlock) !void {
            var iter = bb.instructionIter();
            var pushed = std.ArrayList(*Operand).init(cfg.mem);
            defer pushed.deinit();

            while (iter.next()) |insn| {
                switch(insn.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => |p| {
                        try pushed.append(p.out);
                        insn.data.setOut(try self.newName(p.out, bb, cfg.scope));
                    },
                    else => {
                        var should_append = false;
                        // Rename operands
                        var itr = insn.data.opIter();
                        while (itr.next()) |op| {
                            if (cfg.globals.?.isSet(op.getID())) {
                                insn.data.replaceOpnd(op, self.stackTop(op).?);
                                should_append = true;
                            }
                        }

                        // Rename output variable
                        if (insn.data.getOut()) |out| {
                            if (cfg.globals.?.isSet(out.getID())) {
                                try pushed.append(out);
                                const newname = try self.newName(out, bb, cfg.scope);
                                insn.data.setOut(newname);
                                should_append = true;
                            }
                        }
                    }
                }
            }

            // For each successor, fill in the phi parameters
            if (bb.fall_through_dest) |succ| {
                try self.fillPhiParams(bb, succ);
            }

            if (bb.jump_dest) |succ| {
                try self.fillPhiParams(bb, succ);
            }

            // For each successor in the dominator tree
            //   rename the successor
            const dt = cfg.dom_tree.?;
            var bititer = dt.getColumn(bb.name).iterator(.{});
            while (bititer.next()) |child_id| {
                try self.rename(cfg, cfg.blocks[child_id]);
            }

            // Pop any pushed variables
            while(pushed.pop()) |op| {
                self.stackPop(op);
            }
        }

        fn stackPop(self: *Renamer, variable: *Operand) void {
            if (self.seen.get(variable.getID())) |idx| {
                _ = self.stacks[idx].pop();
            } else {
                unreachable;
            }
        }

        fn stackTop(self: *Renamer, variable: *const Operand) ?*Operand {
            if (self.seen.get(variable.getID())) |idx| {
                const stack = self.stacks[idx];
                return stack.items[stack.items.len - 1];
            } else {
                return null;
            }
        }

        fn newName(self: *Renamer, variable: *Operand, bb: *BasicBlock, scope: *Scope) !*Operand {
            const idx = try self.getStackIndex(variable);
            const i = self.counters[idx];
            self.counters[idx] = i + 1;
            const new_opnd = try scope.newDefinition(variable, bb, i);
            try self.stacks[idx].append(new_opnd);
            return new_opnd;
        }

        fn getStackIndex(self: *Renamer, variable: *const Operand) !usize {
            if (self.seen.get(variable.getID())) |idx| {
                return idx;
            } else {
                const idx = self.seen.count();
                try self.seen.put(variable.getID(), idx);
                return idx;
            }
        }
    };

    pub fn rename(self: *CFG) !void {
        const global_count = self.globals.?.count();
        var renamer = try Renamer.init(self.mem, global_count);
        defer renamer.deinit(self.mem);
        try renamer.rename(self, self.head);
        self.state = .renamed;
    }

    fn isolatePhi(self: *CFG) !void {
        try self.ssa_destructor.isolatePhi(self);
        self.state = .phi_isolated;
    }

    fn insertPhiCopies(self: *CFG) !void {
        try self.ssa_destructor.insertPhiCopies(self);
        self.state = .phi_copies_inserted;
    }

    fn serializeCopyGroups(self: *CFG) !void {
        try self.ssa_destructor.serializeCopyGroups(self);
        self.state = .copy_groups_serialized;
    }

    fn removeRenamedVariables(self: *CFG) !void {
        try self.ssa_destructor.renameAllVariables(self);
        self.state = .renamed_variables_removed;
    }

    fn removePhi(self: *CFG) !void {
        try self.ssa_destructor.eliminatePhi(self);
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

        const seen = try BitMap.initEmpty(mem, self.blockCount());
        defer seen.deinit(mem);
        var counter = self.blockCount();

        try traverseReversePostorder(self.head, seen, blocklist, &counter);

        return blocklist;
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
        self.mem.free(self.blocks);
        self.arena.deinit();
        self.ssa_destructor.deinit();
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
            .phi_removed => { },
        }
        return self.state;
    }

    pub fn compileUntil(self: *CFG, state: CFG.State) !void {
        while (self.state != state) {
            _ = try self.nextCompileStep();
        }
    }

    pub fn build(allocator: std.mem.Allocator, scope: *Scope) !*CFG {
        var builder = CFGBuilder { .scope = scope };
        return try builder.build(allocator, scope);
    }
};

pub const BasicBlock = struct {
    name: u64,
    entry: bool,
    reachable: bool = false,
    start: *ir.InstructionList.Node,
    finish: *ir.InstructionList.Node,
    predecessors: std.ArrayList(*BasicBlock),
    killed_set: *BitMap,
    upward_exposed_set: *BitMap,
    liveout_set: *BitMap,
    livein_set: *BitMap,
    dom: ?*BitMap = null,
    df: ?*BitMap = null,
    fall_through_dest: ?*BasicBlock = null,
    jump_dest: ?*BasicBlock = null,
    idom: ?u64 = null,

    fn initBlock(alloc: std.mem.Allocator, name: u64, start: anytype, finish: anytype, entry: bool) !*BasicBlock {
        const block = try alloc.create(BasicBlock);

        block.* = .{
            .name = name,
            .start = start,
            .finish = finish,
            .entry = entry,
            .killed_set = @constCast(&BitMap.Null),
            .upward_exposed_set = @constCast(&BitMap.Null),
            .liveout_set = @constCast(&BitMap.Null),
            .livein_set = @constCast(&BitMap.Null),
            .predecessors = std.ArrayList(*BasicBlock).init(alloc),
        };

        return block;
    }

    pub fn resetSets(self: *BasicBlock, vars: usize, alloc: std.mem.Allocator) !void {
        self.killed_set.deinit(alloc);
        self.upward_exposed_set.deinit(alloc);
        self.liveout_set.deinit(alloc);
        self.livein_set.deinit(alloc);

        self.killed_set = try BitMap.initEmpty(alloc, vars);
        self.upward_exposed_set = try BitMap.initEmpty(alloc, vars);
        self.liveout_set = try BitMap.initEmpty(alloc, vars);
        self.livein_set = try BitMap.initEmpty(alloc, vars);
    }

    pub fn killedVariableCount(self: *BasicBlock) usize {
        return self.killed_set.count();
    }

    pub fn upwardExposedCount(self: *BasicBlock) usize {
        return self.upward_exposed_set.count();
    }

    pub fn startInsn(self: *BasicBlock) ?*ir.InstructionList.Node {
        return self.start;
    }

    pub fn finishInsn(self: *BasicBlock) ?*ir.InstructionList.Node {
        return self.finish;
    }

    pub fn replace(self: *BasicBlock, scope: *Scope, old: *ir.InstructionList.Node, new: *ir.InstructionList.Node) void {
        scope.insns.insertAfter(old, new);
        scope.insns.remove(old);
        if (self.start == old) {
            self.start = new;
        }
        if (self.finish == old) {
            self.finish = new;
        }
    }

    fn childLo(_: *BasicBlock, child: *BasicBlock, alloc: std.mem.Allocator) !*BitMap {
        const ue = child.upward_exposed_set;
        const lo = child.liveout_set;
        const varkill = child.killed_set;

        // Bitwise NOT the kill list
        var notkill = try varkill.clone(alloc);
        notkill.toggleAll();
        defer alloc.destroy(notkill);

        var lonk = try lo.clone(alloc);
        lonk.setIntersection(notkill);
        defer alloc.destroy(lonk);

        var newlo = try ue.clone(alloc);
        newlo.setUnion(lonk);
        return newlo;
    }

    pub fn insertParallelCopy(self: *BasicBlock, scope: *Scope, node: *ir.InstructionList.Node, dest: *Op, src: *Op, group: usize) !*ir.InstructionList.Node {
        const ret = try scope.insertParallelCopy(node, dest, src, self, group);

        if (node == self.finish) {
            self.finish = ret;
        }

        if (node == self.start) {
            self.start = ret;
        }

        return ret;
    }

    pub fn appendParallelCopy(self: *BasicBlock, scope: *Scope, dest: *Op, src: *Op, group: usize) !*ir.InstructionList.Node {
        var node = self.finish;

        if (self.finish.data.isJump()) {
            node = self.finish.prev.?;
        }

        return self.insertParallelCopy(scope, node, dest, src, group);
    }

    // Update the LiveOut set.  If the set changes, returns true
    pub fn updateLiveOut(self: *BasicBlock, alloc: std.mem.Allocator) !bool {
        // Engineering a compiler, 3rd ed, 8.6, "Defining the Data-Flow Problem" (page 419)
        // Also Figure 8.15
        if (self.fall_through_dest) |child1| {
            const newlo = try self.childLo(child1, alloc);
            defer alloc.destroy(newlo);

            if (self.jump_dest) |child2| {
                const newlo2 = try self.childLo(child2, alloc);
                defer alloc.destroy(newlo2);

                var bothlo = try newlo.clone(alloc);
                bothlo.setUnion(newlo2);
                defer alloc.destroy(bothlo);

                if (self.liveout_set.eq(bothlo)) {
                    return false;
                } else {
                    try self.liveout_set.replace(bothlo);
                    return true;
                }
            } else {
                if (self.liveout_set.eq(newlo)) {
                    return false;
                } else {
                    try self.liveout_set.replace(newlo);
                    return true;
                }
            }
        } else {
            if (self.jump_dest) |child2| {
                const newlo = try self.childLo(child2, alloc);
                defer alloc.destroy(newlo);

                if (self.liveout_set.eq(newlo)) {
                    return false;
                } else {
                    try self.liveout_set.replace(newlo);
                    return true;
                }
            } else {
                return false;
            }
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
        return .{ .current = self.start, .finish = self.finish, .done = false };
    }

    pub fn fillVarSets(self: *BasicBlock) !void {
        var iter = self.instructionIter();

        while (iter.next()) |insn| {
            // Fill the UE set.
            var opiter = insn.data.opIter();
            while (opiter.next()) |op| {
                // If the operand is a variable, and it _isn't_ part of the kill set,
                // (in other words it hasn't been defined in this BB), then add
                // the operand to the "upward exposed" set.  This means the operand
                // _must_ have been defined in a block that dominates this block.
                if (op.isVariable() and !self.killed_set.isSet(op.getID())) {
                    self.upward_exposed_set.set(op.getID());
                }
            }

            if (insn.data.outVar()) |v| {
                if (v.isVariable()) {
                    self.killed_set.set(v.getID());
                }
            }
        }
    }

    fn addInstruction(self: *BasicBlock, insn: *ir.InstructionList.Node) void {
        // Set the definition block for the outvar on the instruction.
        // We need this for phi placement / renaming etc
        if (insn.data.getOut()) |outvar| {
            outvar.setDefinitionBlock(self);
        }
        self.finish = insn;
    }

    fn fallsThrough(self: *BasicBlock) bool {
        return switch(self.finish.data) {
            .jump, .leave => false,
            else => true
        };
    }

    pub fn hasPhiFor(self: *BasicBlock, opnd: *ir.Operand) bool {
        var iter = self.instructionIter();
        while (iter.next()) |insn| {
            switch(insn.data) {
                .putlabel => { }, // Skip putlabel
                .phi => |p| {
                    if (p.out == opnd) return true;
                },
                else => { return false; }
            }
        }
        return false;
    }

    pub fn addPhi(self: *BasicBlock, scope: *Scope, opnd: *ir.Operand) !void {
        var iter = self.instructionIter();
        while (iter.next()) |insn| {
            switch(insn.data) {
                inline .phi, .putlabel => { }, // Skip phi and putlabel
                else => {
                    if (insn.prev) |prev| {
                        const phi_insn = try scope.insertPhi(prev, opnd);
                        if (insn == self.start) {
                            self.start = phi_insn;
                        }
                    } else {
                        unreachable;
                    }
                    return;
                }
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

    fn instructionCount(self: *BasicBlock) u32 {
        var count: u32 = 0;
        var iter = self.instructionIter();

        while(iter.next()) |_| {
            count += 1;
        }

        return count;
    }

    fn jumpTarget(self: *BasicBlock) *ir.Operand {
        return self.finish.data.jumpTarget();
    }

    fn addPredecessor(self: *BasicBlock, predecessor: *BasicBlock) !void {
        try self.predecessors.append(predecessor);
    }

    fn setJumpDest(self: *BasicBlock, child: *BasicBlock) void {
        if (self.jump_dest) |_| {
            unreachable;
        } else {
            self.jump_dest = child;
        }
    }

    fn setFallThroughDest(self: *BasicBlock, child: *BasicBlock) void {
        if (self.fall_through_dest) |_| {
            unreachable;
        } else {
            self.fall_through_dest = child;
        }
    }

    pub fn uninitializedSet(self: *BasicBlock, scope: *Scope, mem: std.mem.Allocator) !*BitMap {
        if (!self.entry) return error.ArgumentError;
        if (self.killed_set.isNullBlock()) {
            return self.killed_set;
        }

        const uninit = try self.killed_set.clone(mem);
        uninit.toggleAll();
        uninit.setIntersection(self.liveout_set);
        uninit.setUnion(self.upward_exposed_set);

        var biti = uninit.iterator(.{});
        while (biti.next()) |opnd_id| {
            const op = scope.getOperandById(opnd_id);
            // Remove parameters from the uninitialized set. They should
            // be initialized by the caller
            if (op.isParam()) {
                uninit.unset(opnd_id);
            }
        }
        return uninit;
    }
};

pub const CompileError = error {
    EmptyInstructionSequence,
};

const CFGBuilder = struct {
    block_name: u32 = 0,
    scope: *Scope,

    fn makeBlock(self: *CFGBuilder, mem: std.mem.Allocator, start: *ir.InstructionList.Node, finish: *ir.InstructionList.Node, entry: bool) !*BasicBlock {
        const block = try BasicBlock.initBlock(mem,
            self.block_name,
            start,
            finish,
            entry);

        self.block_name += 1;

        return block;
    }

    fn build(self: *CFGBuilder, allocator: std.mem.Allocator, scope: *Scope) !*CFG {
        const insns = scope.insns;
        var node = insns.first;

        // If we don't have any nodes to process, just return the empty CFG.
        if (node == null) {
            return CompileError.EmptyInstructionSequence;
        }

        var arena = std.heap.ArenaAllocator.init(allocator);

        var wants_label = std.ArrayList(*BasicBlock).init(allocator);
        defer wants_label.deinit();

        var all_blocks = std.ArrayList(*BasicBlock).init(allocator);
        defer all_blocks.deinit();

        var last_block: ?*BasicBlock = null;
        var head: ?*BasicBlock = null;

        var label_to_block_lut: []?*BasicBlock = try allocator.alloc(?*BasicBlock, scope.label_id);
        @memset(label_to_block_lut, null);
        defer allocator.free(label_to_block_lut);

        var entry = true;

        // For all of our instructions
        while (node) |insn| {
            // Create a new block
            const current_block = try self.makeBlock(arena.allocator(), insn, insn, entry);
            try all_blocks.append(current_block);
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
            if (current_block.entry) {
                head = current_block;
            } else {
                if (last_block.?.fallsThrough()) {
                    last_block.?.setFallThroughDest(current_block);
                    try current_block.addPredecessor(last_block.?);
                }
            }

            // If this block has a label at the top, register it so that
            // we can link other blocks to this one
            if (current_block.hasLabeledEntry()) {
                const label_name = current_block.start.data.putlabel.name.label.name;
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
            want_label.setJumpDest(target);
            try target.addPredecessor(want_label);
        }

        const cfg = try CFG.init(allocator, arena, scope, head.?, try all_blocks.toOwnedSlice());

        // TODO: sweep unreachable blocks?

        // TODO: We could calculate the killed set and the upward exposed set
        // while building the basic blocks, rather than here.  But then we
        // wouldn't have the opportunity to do a peephole optimization step before
        // calculating the VarKilled and UEVars.  Haven't implemented the
        // peephole optimization step yet. Maybe we don't need it and can avoid
        // the extra loops here?

        return cfg;
    }
};

fn buildCFG(allocator: std.mem.Allocator, scope: *Scope) !*CFG {
    const cfg = try CFG.build(allocator, scope);
    _ = try cfg.nextCompileStep();
    _ = try cfg.nextCompileStep();
    return cfg;
}

test "empty basic block" {
    const scope = try Scope.init(std.testing.allocator, 0, "empty", null);
    defer scope.deinit();

    try std.testing.expectError(CompileError.EmptyInstructionSequence, buildCFG(std.testing.allocator, scope));
}

test "basic block one instruction" {
    const scope = try Scope.init(std.testing.allocator, 0, "empty", null);
    defer scope.deinit();

    _ = try scope.pushGetself();

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;

    try std.testing.expectEqual(scope.insns.first.?, bb.start);
    try std.testing.expectEqual(scope.insns.first.?, bb.finish);
    try std.testing.expectEqual(null, bb.fall_through_dest);
    try std.testing.expectEqual(null, bb.jump_dest);
}

test "basic block two instruction" {
    const scope = try Scope.init(std.testing.allocator, 0, "empty", null);
    defer scope.deinit();

    _ = try scope.pushLoadi(null, 123);
    _ = try scope.pushGetself();

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;

    try std.testing.expectEqual(scope.insns.first.?, bb.start);
    try std.testing.expectEqual(scope.insns.last.?, bb.finish);
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

    const bb = cfg.head;
    const start_type: ir.InstructionName = bb.start.data;
    try std.testing.expectEqual(ir.InstructionName.loadi, start_type);

    const finish_type: ir.InstructionName = bb.finish.data;
    try std.testing.expectEqual(ir.InstructionName.leave, finish_type);
}

test "no uninitialized in ternary" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "x ? 1 : 23");
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const uninitialized = try cfg.head.uninitializedSet(scope, allocator);
    defer uninitialized.deinit(allocator);

    try std.testing.expectEqual(0, uninitialized.count());
}

test "if statement should have 2 children blocks" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "def foo(x); x ? 1 : 23; end");
    defer scope.deinit();

    // Get the scope for the method
    const method_scope: *Scope = scope.insns.first.?.data.define_method.func.scope.value;

    const cfg = try buildCFG(allocator, method_scope);
    defer cfg.deinit();

    const block = cfg.head;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.jumpunless,
    }, block);
    try std.testing.expectEqual(block.finish, block.start);
    try std.testing.expect(block.fallsThrough());
    try std.testing.expectEqual(1, block.instructionCount());

    var child = block.fall_through_dest.?;
    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.jump,
    }, child);

    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expect(!child.fallsThrough());

    child = block.jump_dest.?;
    try std.testing.expectEqual(2, child.instructionCount());
    try std.testing.expectEqual(ir.Instruction.putlabel, @as(ir.InstructionName, child.start.data));
    try std.testing.expectEqual(ir.Instruction.loadi, @as(ir.InstructionName, child.finish.data));

    // Last block via fallthrough then jump
    const last_block = block.fall_through_dest.?.jump_dest.?;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.putlabel,
        ir.Instruction.phi,
        ir.Instruction.leave,
    }, last_block);

    // block via jump then fallthrough
    const last_block2 = block.jump_dest.?.fall_through_dest.?;
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
    }, bb);

    try std.testing.expectEqual(1, bb.killedVariableCount());
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
        ir.Instruction.loadi,
    }, bb);

    try std.testing.expectEqual(1, bb.killedVariableCount());
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
        if (bb.fall_through_dest) |_| {
            try std.testing.expect(bb.liveout_set.isSet(opnd.local.id));
        }
    }
}

test "jumps targets get predecessors" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code =
\\ def foo y
\\   if y < 3
\\     y + 1
\\   else
\\     y + 4
\\   end
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

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code =
\\ def foo y
\\   if y < 3
\\     y + 1
\\   else
\\     y + 4
\\   end
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

    const blocks = methodcfg.blockList();

    // We should have 4 blocks
    try std.testing.expectEqual(4, blocks.len);
    try std.testing.expect(blocks[0].entry);

    // Entry dominates itself
    try std.testing.expectEqual(1, blocks[0].dom.?.count());
    try std.testing.expect(blocks[0].dom.?.isSet(0));
    try std.testing.expectEqual(null, blocks[0].idom);

    // BB1 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.?.count());
    try std.testing.expect(blocks[1].dom.?.isSet(0));
    try std.testing.expect(blocks[1].dom.?.isSet(1));
    try std.testing.expectEqual(0, blocks[1].idom.?);

    // BB2 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.?.count());
    try std.testing.expect(blocks[2].dom.?.isSet(0));
    try std.testing.expect(blocks[2].dom.?.isSet(2));
    try std.testing.expectEqual(0, blocks[1].idom.?);

    // BB3 dominated by self and BB0
    try std.testing.expectEqual(2, blocks[1].dom.?.count());
    try std.testing.expect(blocks[3].dom.?.isSet(0));
    try std.testing.expect(blocks[3].dom.?.isSet(3));
    try std.testing.expectEqual(0, blocks[1].idom.?);
}

test "while loop dominators" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

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
    const scope = try compileScope(allocator, machine, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();

    // We should have 7 blocks
    try std.testing.expectEqual(7, blocks.len);
    try std.testing.expectEqual(6, blocks[6].name);
    try std.testing.expect(blocks[6].dom.?.isSet(6));
}

test "dominance frontiers" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, machine, code);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();

    try std.testing.expectEqual(9, blocks.len);
    try std.testing.expectEqual(6, blocks[6].name);
    try std.testing.expect(blocks[1].df.?.isSet(1));
    try std.testing.expect(blocks[2].df.?.isSet(7));
    try std.testing.expect(blocks[3].df.?.isSet(7));
    try std.testing.expect(blocks[4].df.?.isSet(6));
    try std.testing.expect(blocks[5].df.?.isSet(6));
    try std.testing.expect(blocks[6].df.?.isSet(7));
    try std.testing.expect(blocks[7].df.?.isSet(1));

    for (0..blocks.len) |i| {
        if (i == 8) {
            try std.testing.expect(!blocks[i].reachable);
        } else {
            try std.testing.expect(blocks[i].reachable);
        }
    }
    try std.testing.expectEqual(0, blocks[0].df.?.count());
}

test "dominator tree" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, machine, code);
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

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const code = complexExampleCFG();
    const scope = try compileScope(allocator, machine, code);
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

fn compileScope(allocator: std.mem.Allocator, machine: *vm.VM, code: []const u8) !*Scope {
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
    var insn: ?*ir.InstructionList.Node = block.start;
    for (expected) |expected_insn| {
        try std.testing.expectEqual(expected_insn, @as(ir.InstructionName, insn.?.data));
        insn = insn.?.next;
    }
}
