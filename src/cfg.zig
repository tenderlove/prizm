const std = @import("std");
const ir = @import("ir.zig");
const prism = @import("prism.zig");
const Compiler = @import("compiler.zig").Compiler;
const Scope = @import("scope.zig").Scope;
const Globals = @import("globals.zig").Globals;
const BasicBlock = @import("basic_block.zig").BasicBlock;
pub const CFG = struct {
    mem: std.mem.Allocator,
    head: *BasicBlock,
    blocks: []const *BasicBlock,
    scope: *Scope,

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
        };

        return cfg;
    }

    pub fn depthFirstIterator(self: *CFG, alloc: std.mem.Allocator) !BasicBlock.DepthFirstIterator {
        return try self.head.depthFirstIterator(alloc);
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

    pub fn blockList(self: *CFG) []const *BasicBlock {
        return self.blocks;
    }

    pub fn blockCount(self: *CFG) usize {
        return self.blocks.len;
    }

    pub fn deinit(self: *CFG) void {
        for (self.blocks) |blk| {
            blk.deinit(self.mem);
        }
        self.mem.free(self.blocks);
        self.mem.destroy(self);
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

    const val = try scope.pushLoadi(123);
    _ = try scope.pushLeave(val);  // auto-fills the block

    // scope.cfg asserts every block is sealed + filled. Fill happens on the
    // terminator push; seal has no automatic trigger, so do it here.
    try scope.sealBlock(scope.currentBlock());

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;

    try std.testing.expectEqual(2, bb.insnCount());
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

// `if` without an else branch. In Ruby this evaluates to nil on the false
// path, so compileIfNode must synthesize a `loadnil` in the else arm rather
// than dereferencing node.*.subsequent (which is null). Currently
// compileIfNode does `@ptrCast(node.*.subsequent)` unconditionally and will
// crash. Skip until we handle the missing-else case.
test "if without else evaluates to nil on false path" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ x = if false then 1 end
    );
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    // We should have 4 blocks
    const blocks = cfg.blockList();
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

// A `return` inside a branch terminates that branch's current block with
// `.leave`. compileIfNode currently pushes an unconditional `jump if_exit`
// after each branch unconditionally — so a returning branch ends up with
// TWO terminators AND if_exit gets a spurious predecessor for a control
// flow edge that can never fire. Skip until pushJump becomes a no-op
// after a terminator (or compileNode signals early termination).
test "return in a branch doesn't emit a jump after leave" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ def foo x
        \\   if x
        \\     return 42
        \\   else
        \\     99
        \\   end
        \\ end
    );
    defer scope.deinit();

    const outer_cfg = try buildCFG(allocator, scope);
    defer outer_cfg.deinit();
    const insn = (try findInsn(outer_cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;
    const method_cfg = try buildCFG(allocator, method_scope);
    defer method_cfg.deinit();

    // compileIfNode creates blocks in this order: entry, then, else, if_exit.
    const blocks = method_cfg.blockList();
    try std.testing.expectEqual(4, blocks.len);

    const entry_block = blocks[0];
    const then_block = blocks[1];
    const else_block = blocks[2];
    const if_exit = blocks[3];

    // The `then` block must end in `.leave` (from `return 42`) — the fix is
    // that pushJump no-ops on an already-terminated block, so no spurious
    // jump gets appended after the leave.
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, then_block.finishInsn().?.data),
    );

    // The else side reached the merge with a normal jump.
    try std.testing.expectEqual(
        ir.InstructionName.jump,
        @as(ir.InstructionName, else_block.finishInsn().?.data),
    );

    // if_exit has ONE predecessor: only the else side actually falls through.
    // The then side terminated with leave, so it does NOT contribute an edge.
    try std.testing.expectEqual(1, if_exit.predecessors.items.len);
    try std.testing.expectEqual(else_block, if_exit.predecessors.items[0]);

    // Entry still splits into both branches — cond edges are eager, they
    // don't retroactively vanish just because one arm ended up terminating.
    try std.testing.expectEqual(
        ir.InstructionName.cond,
        @as(ir.InstructionName, entry_block.finishInsn().?.data),
    );
}

// Both branches of an `if` return. compileIfNode currently leaves
// `current_block` pointing at `else_entry` (which already ends in `.leave`
// from the else-side return) and returns `truthy` — the `.leave` instruction
// pushed by the then-side `compileReturnNode`.
//
// The caller (compileScopeNode) then does `pushLeave(last_op)` on
// `else_entry`, appending a *second* `.leave` after the existing one AND
// wrapping a terminator insn as an SSA value (`leave(leave)`).
//
// Skip until compileIfNode moves current_block to `if_exit` (and returns a
// legitimate non-terminator value there — e.g., a fresh loadnil) when both
// arms terminate.
test "if with return in both branches doesn't double-terminate else" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ def foo x
        \\   if x
        \\     return 1
        \\   else
        \\     return 2
        \\   end
        \\ end
    );
    defer scope.deinit();

    const outer_cfg = try buildCFG(allocator, scope);
    defer outer_cfg.deinit();
    const insn = (try findInsn(outer_cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;
    const method_cfg = try buildCFG(allocator, method_scope);
    defer method_cfg.deinit();

    const blocks = method_cfg.blockList();
    try std.testing.expectEqual(4, blocks.len);

    const then_block = blocks[1];
    const else_block = blocks[2];

    // The then arm is `loadi 1; leave` — nothing after the leave.
    try std.testing.expectEqual(2, then_block.insnCount());
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, then_block.finishInsn().?.data),
    );

    // Same for the else arm. Currently fails: else_entry gets a third
    // instruction, a second `.leave` from compileScopeNode's pushLeave.
    try std.testing.expectEqual(2, else_block.insnCount());
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, else_block.finishInsn().?.data),
    );

    // Every `.leave.in` must be a real SSA value — never a terminator.
    // Currently fails: the scope-level pushLeave receives `truthy` (a leave
    // insn) as its operand, so we get `leave(leave(loadi 1))`.
    for (blocks) |bb| {
        var it = bb.instructionIter(.{});
        while (it.next()) |i| {
            if (i.data == .leave) {
                try std.testing.expect(i.data.leave.in.data != .leave);
                try std.testing.expect(i.data.leave.in.data != .jump);
                try std.testing.expect(i.data.leave.in.data != .cond);
            }
        }
    }
}

// Code after an `if` whose branches both `return` must not land in the
// already-terminated else block. compileIfNode should park current_block on
// `if_exit` even in the both-terminated case so downstream compilation
// emits into an unreachable-but-well-formed merge block rather than
// scribbling past a terminator.
test "code after both-branch-return if lands in if_exit" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ def foo x
        \\   if x
        \\     return 1
        \\   else
        \\     return 2
        \\   end
        \\   99
        \\ end
    );
    defer scope.deinit();

    const outer_cfg = try buildCFG(allocator, scope);
    defer outer_cfg.deinit();
    const insn = (try findInsn(outer_cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;
    const method_cfg = try buildCFG(allocator, method_scope);
    defer method_cfg.deinit();

    const blocks = method_cfg.blockList();
    try std.testing.expectEqual(4, blocks.len);

    const else_block = blocks[2];
    const if_exit = blocks[3];

    // Else arm is exactly `loadi 2; leave`. The `99` must NOT land here.
    try std.testing.expectEqual(2, else_block.insnCount());

    // The trailing `99` (loadi 99) and scope-final `leave` must land in
    // if_exit. If compileIfNode failed to move current_block there, this
    // block would be empty.
    try std.testing.expect(if_exit.insnCount() >= 2);
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, if_exit.finishInsn().?.data),
    );
}

// When an inner `if` has `return` in both arms, the outer `if`'s then-arm
// never falls through. In an ideal IR, the outer merge (outer_if_exit)
// would end up with a single reachable predecessor (the outer else side)
// and no phi, since only one arm actually reached it.
//
// Because compileIfNode's both-terminated case parks current_block on a
// fresh `inner_if_exit` with a `loadnil` in it, the outer sees
// `isTerminated() == false` on that block, pushes a `jump outer_if_exit`,
// and outer_if_exit ends up with a phi whose then-arm operand traces back
// to a value in an unreachable block (inner_if_exit has 0 preds).
//
// Semantically this is fine — inner_if_exit is dead, so its jump/phi
// contribution is dead too, and an unreachable-block sweep would prune it.
// But the emitted CFG is uglier than it needs to be. Test documents the
// current behavior so a future propagation fix is a visible diff.
test "nested if with both-branch returns leaves outer merge with dead pred" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ def foo x, y
        \\   if x
        \\     if y
        \\       return 1
        \\     else
        \\       return 2
        \\     end
        \\   else
        \\     3
        \\   end
        \\ end
    );
    defer scope.deinit();

    const outer_cfg = try buildCFG(allocator, scope);
    defer outer_cfg.deinit();
    const insn = (try findInsn(outer_cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;
    const method_cfg = try buildCFG(allocator, method_scope);
    defer method_cfg.deinit();

    const blocks = method_cfg.blockList();
    // entry, outer_then, outer_else, outer_if_exit, inner_then, inner_else,
    // inner_if_exit — 7 total. Inner blocks are created inside outer_then's
    // compileNode call, so ordering is:
    //   0 entry
    //   1 outer_then
    //   2 outer_else
    //   3 outer_if_exit
    //   4 inner_then
    //   5 inner_else
    //   6 inner_if_exit
    try std.testing.expectEqual(7, blocks.len);

    const outer_if_exit = blocks[3];
    const inner_then = blocks[4];
    const inner_else = blocks[5];
    const inner_if_exit = blocks[6];

    // Both inner arms end in `.leave` — the returns.
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, inner_then.finishInsn().?.data),
    );
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, inner_else.finishInsn().?.data),
    );

    // inner_if_exit is orphan: 0 predecessors (both inner arms terminated).
    try std.testing.expectEqual(0, inner_if_exit.predecessors.items.len);

    // But outer sees inner_if_exit as non-terminated (its terminator is a
    // jump to outer_if_exit, added by outer's compileIfNode) — so
    // outer_if_exit gets 2 predecessors and a phi.
    try std.testing.expectEqual(
        ir.InstructionName.jump,
        @as(ir.InstructionName, inner_if_exit.finishInsn().?.data),
    );
    try std.testing.expectEqual(2, outer_if_exit.predecessors.items.len);

    // outer_if_exit's first insn is a phi — documenting the dead-operand
    // situation. A future fix would either propagate termination up (so
    // outer_if_exit has 1 pred and no phi) or run a sweep that prunes
    // unreachable blocks and simplifies trivial phis.
    try std.testing.expectEqual(
        ir.InstructionName.phi,
        @as(ir.InstructionName, outer_if_exit.startInsn().?.data),
    );
}

// Regular `while`: `next` jumps to the loop header (the block that
// evaluates the condition), so on the next iteration the predicate runs
// again. Body block ordering:
//   0 entry
//   1 header    ← 3 preds: entry, `next`'s block, natural body tail
//   2 body
//   3 exit
//   4 next-if then (contains next)
//   5 next-if else (empty, jumps to if_exit)
//   6 next-if if_exit (contains `puts i`)
test "next jumps to the loop header" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ i = 10
        \\ while i > 0
        \\   i -= 1
        \\   next if i == 5
        \\   puts i
        \\ end
    );
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();
    try std.testing.expectEqual(7, blocks.len);

    const header = blocks[1];
    const then_of_next_if = blocks[4];
    const if_exit_of_next_if = blocks[6];

    // Header receives 3 edges: pre-loop, natural body-tail, `next`.
    try std.testing.expectEqual(3, header.predecessors.items.len);

    // The block containing `next` ends in exactly one terminator: a jump
    // to the header.
    try std.testing.expectEqual(
        ir.InstructionName.jump,
        @as(ir.InstructionName, then_of_next_if.finishInsn().?.data),
    );
    try std.testing.expectEqual(header, then_of_next_if.finishInsn().?.data.jump.target);

    // `puts i` lives in the if_exit block (reached only when `next` wasn't
    // taken), NOT in the `next` block.
    var found_puts_in_then = false;
    var then_iter = then_of_next_if.instructionIter(.{});
    while (then_iter.next()) |insn| {
        if (insn.data == .call and std.mem.eql(u8, insn.data.call.name, "puts")) {
            found_puts_in_then = true;
        }
    }
    try std.testing.expect(!found_puts_in_then);

    var found_puts_in_if_exit = false;
    var exit_iter = if_exit_of_next_if.instructionIter(.{});
    while (exit_iter.next()) |insn| {
        if (insn.data == .call and std.mem.eql(u8, insn.data.call.name, "puts")) {
            found_puts_in_if_exit = true;
        }
    }
    try std.testing.expect(found_puts_in_if_exit);
}

// `break VALUE` routes VALUE into the loop's exit block via a phi.
//   0 entry (i = 0)
//   1 header
//   2 body
//   3 exit    ← 2 preds: header (nil), then_of_if (loadi 42)
//   4 then    (`break 42`)
//   5 else    (empty)
//   6 if_exit
test "break with value flows into the loop exit via a phi" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ i = 0
        \\ result = while i < 10
        \\   break 42 if i == 5
        \\   i += 1
        \\ end
    );
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();
    const exit = blocks[3];

    // exit reached from two predecessors: cond-false (header) with nil,
    // and the break site with loadi(42).
    try std.testing.expectEqual(2, exit.predecessors.items.len);

    // exit starts with a phi merging the two contributions.
    try std.testing.expectEqual(
        ir.InstructionName.phi,
        @as(ir.InstructionName, exit.startInsn().?.data),
    );
    try std.testing.expectEqual(2, exit.startInsn().?.data.phi.params.items.len);
}

// Bare `break` (no value) — both phi operands are `loadnil` instructions,
// but they're two DIFFERENT insns (one seeded on the cond-false edge, one
// pushed at the break site). Braun's tryRemoveTrivialPhi compares by pointer
// identity, not semantic equality, so the phi survives. A future value-
// numbering / constant-folding pass would collapse it.
test "bare break emits a phi of two loadnil operands" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ i = 0
        \\ while i < 10
        \\   break if i == 5
        \\   i += 1
        \\ end
    );
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope);
    defer cfg.deinit();

    const blocks = cfg.blockList();
    const exit = blocks[3];

    try std.testing.expectEqual(2, exit.predecessors.items.len);

    // Phi survives — two operands, both loadnil.
    const first = exit.startInsn().?;
    try std.testing.expectEqual(
        ir.InstructionName.phi,
        @as(ir.InstructionName, first.data),
    );
    try std.testing.expectEqual(2, first.data.phi.params.items.len);
    for (first.data.phi.params.items) |p| {
        try std.testing.expectEqual(
            ir.InstructionName.loadnil,
            @as(ir.InstructionName, p.data),
        );
    }
}

// Both arms of the `if` read the same variable `y`, which is defined once
// (`y = x`) in the entry block. Braun's construction reads back to entry
// from both arm entries, so `truthy == falsy == getparam(1)`. compileIfNode
// pushes phi(gp1, gp1) at if_exit — a textbook trivial phi. After canonicalize
// runs, the phi should be marked as aliased to gp1, and any consumer of that
// phi (here, the scope-final `leave`) should reference gp1 directly with no
// phi in the operand chain.
test "trivial phi from same-value branches is aliased away" {
    const allocator = std.testing.allocator;

    const globals = try Globals.init(allocator);
    defer globals.deinit(allocator);

    const scope = try compileScope(allocator, globals,
        \\ def foo x
        \\   y = x
        \\   if x
        \\     y
        \\   else
        \\     y
        \\   end
        \\ end
    );
    defer scope.deinit();

    const outer_cfg = try buildCFG(allocator, scope);
    defer outer_cfg.deinit();
    const insn = (try findInsn(outer_cfg, ir.InstructionName.define_method)).?;
    const method_scope = insn.data.define_method.func;
    const method_cfg = try buildCFG(allocator, method_scope);
    defer method_cfg.deinit();

    const blocks = method_cfg.blockList();
    const if_exit = blocks[3];

    // The phi is still in the block (dead-code sweep is future work), but
    // it's been marked as aliased.
    const first = if_exit.startInsn().?;
    try std.testing.expectEqual(
        ir.InstructionName.phi,
        @as(ir.InstructionName, first.data),
    );
    try std.testing.expect(first.alias != null);

    // The scope-final `leave` references gp1 (Ruby's `x` parameter — self
    // is param 0, so `x` is index 1) directly, NOT the phi.
    const leave_insn = if_exit.finishInsn().?;
    try std.testing.expectEqual(
        ir.InstructionName.leave,
        @as(ir.InstructionName, leave_insn.data),
    );
    try std.testing.expectEqual(
        ir.InstructionName.getparam,
        @as(ir.InstructionName, leave_insn.data.leave.in.data),
    );
    try std.testing.expectEqual(1, leave_insn.data.leave.in.data.getparam.index);
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
    const cc = try Compiler.init(allocator, globals, parser);
    defer cc.deinit(allocator);
    return try cc.compile(&scope_node);
}
