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

    pub fn replace(self: *CFG, block: *BasicBlock, old: *ir.InstructionListNode, new: *ir.InstructionListNode) void {
        block.replace(self.scope, old, new);
    }    pub fn liveBlockCount(self: *CFG) !usize {
        var dfi = try self.depthFirstIterator(self.mem);
        defer dfi.deinit(self.mem);

        var count: usize = 0;
        while (try dfi.next(self.mem)) |_| {
            count += 1;
        }
        return count;
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

    _ = try scope.pushLoadi(123);

    const cfg = try buildCFG(std.testing.allocator, scope);
    defer cfg.deinit();

    const bb = cfg.head;

    try std.testing.expectEqual(1, bb.insnCount());
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
    return error.SkipZigTest;
    // const allocator = std.testing.allocator;
    //
    // const globals = try Globals.init(allocator);
    // defer globals.deinit(allocator);
    //
    // const scope = try compileScope(allocator, globals,
    //     \\ x = if false then 1 end
    // );
    // defer scope.deinit();
    //
    // const cfg = try buildCFG(allocator, scope);
    // defer cfg.deinit();
    //
    // Expected shape:
    //   pre       → if_entry
    //   if_entry  → then_entry, else_entry   (brif)
    //   then_entry → if_exit                 (loadi 1, jump)
    //   else_entry → if_exit                 (loadnil, jump)
    //   if_exit                              (phi from then_entry, else_entry)
    //
    // Assertions to add once compileIfNode handles missing-else:
    //   - if_exit has 2 predecessors.
    //   - if_exit starts with a phi.
    //   - one phi input traces back to a loadi(1), the other to a loadnil.
}

// A `return` inside a branch terminates that branch's current block with
// `.leave`. compileIfNode currently pushes an unconditional `jump if_exit`
// after each branch unconditionally — so a returning branch ends up with
// TWO terminators AND if_exit gets a spurious predecessor for a control
// flow edge that can never fire. Skip until pushJump becomes a no-op
// after a terminator (or compileNode signals early termination).
test "return in a branch doesn't emit a jump after leave" {
    return error.SkipZigTest;
    // const allocator = std.testing.allocator;
    //
    // const globals = try Globals.init(allocator);
    // defer globals.deinit(allocator);
    //
    // const scope = try compileScope(allocator, globals,
    //     \\ def foo x
    //     \\   if x
    //     \\     return 42
    //     \\   else
    //     \\     99
    //     \\   end
    //     \\ end
    // );
    // defer scope.deinit();
    //
    // const insn = (try findInsn(try buildCFG(allocator, scope),
    //                            ir.InstructionName.define_method)).?;
    // const method_scope = insn.data.define_method.func;
    // const method_cfg = try buildCFG(allocator, method_scope);
    // defer method_cfg.deinit();
    //
    // Assertions to add:
    //   - Find the block whose terminator is `.leave`. It must have exactly
    //     one terminator instruction (leave), not leave-then-jump.
    //   - if_exit has ONE predecessor (the else side only), not two.
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

