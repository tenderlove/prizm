const std = @import("std");
const ir = @import("ir.zig");
const prism = @import("prism.zig");
const compiler = @import("compiler.zig");
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

    pub fn opndCount(self: @This()) usize {
        return self.scope.varCount();
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
