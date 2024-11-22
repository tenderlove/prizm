const std = @import("std");
const ssa = @import("ssa.zig");
const ir = @import("ir.zig");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const compiler = @import("compiler.zig");
const printer = @import("printer.zig");
const assert = @import("std").debug.assert;

pub const CFG = struct {
    arena: std.heap.ArenaAllocator,
    block_name: u32,
    head: *BasicBlock,

    pub fn init(mem: std.mem.Allocator) !*CFG {
        // Create an arena
        var arena = std.heap.ArenaAllocator.init(mem);

        const cfg = try arena.allocator().create(CFG);
        const bb = try arena.allocator().create(BasicBlock);

        bb.* = BasicBlock {
            .head = .{
                .name = 0,
            }
        };

        cfg.* = CFG {
            .arena = arena,
            .block_name = 1,
            .head = bb,
        };

        return cfg;
    }

    pub fn makeBlock(self: *CFG, start: *ir.InstructionList.Node, finish: *ir.InstructionList.Node) !*BasicBlock {
        const block = try self.arena.allocator().create(BasicBlock);

        block.* = .{
            .block = .{
                .name = self.block_name,
                .start = start,
                .finish = finish,
            }
        };

        self.block_name += 1;

        return block;
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
        start: *ir.InstructionList.Node,
        finish: *ir.InstructionList.Node,
        out: ?*BasicBlock = null,
        out2: ?*BasicBlock = null,
    },

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
            var count: u32 = 1;

            var cursor: ?*ir.InstructionList.Node = self.block.start;

            while(cursor) |link| {
                if (cursor == self.block.finish) {
                    break;
                }
                count += 1;
                cursor = link.next;
            }

            return count;
        }
    }

    fn jumpTarget(self: *BasicBlock) ir.Operand {
        if (BasicBlockType.head == @as(BasicBlockType, self.*)) {
            unreachable;
        } else {
            return self.block.finish.data.jumpTarget();
        }
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
};

pub const CompileError = error {
    EmptyInstructionSequence,
};

pub fn buildCFG(allocator: std.mem.Allocator, scope: *compiler.Scope) !*CFG {
    const cfg = try CFG.init(allocator);

    var wants_label = std.ArrayList(*BasicBlock).init(allocator);
    defer wants_label.deinit();

    var has_label = std.ArrayList(*BasicBlock).init(allocator);
    defer has_label.deinit();

    //var all_bbs = std.ArrayList(*BasicBlock).init(allocator);
    //defer all_bbs.deinit();

    var last_block = cfg.head;

    const insns = scope.insns;
    var node = insns.first;

    // For all of our instructions
    while (node) |insn| {
        // Create a new block
        const current_block = try cfg.makeBlock(insn, insn);

        // Scan through following instructions until we find an instruction
        // that should end the block.
        while (node) |finish_insn| {
            // Add each instruction to the current block
            current_block.addInstruction(finish_insn);

            node = finish_insn.next;

            const insn_node = finish_insn.data;

            // If the last instruction is a jump we should end the block
            if (insn_node.isJump() or insn_node.isCall() or insn_node.isReturn()) {
                std.debug.print("end the block\n", .{ });
                break;
            }

            // If the next instruction is a label we should end the block
            if (node) |next_insn| {
                if (next_insn.data.isLabel()) {
                    std.debug.print("it's a label\n", .{ });
                    break;
                }
            }
        }

        ////try all_bbs.append(current_block);

        // If the previous block falls through, then we should add the
        // current block as a outgoing edge, and add the last block
        // as a predecessor to this block.
        if (last_block.fallsThrough()) {
            last_block.addEdge(current_block);
            //current_block.addPredecessor(last_block);
        }

        // If this block has a label at the top, register it so that
        // we can link other blocks to this one
        if (current_block.hasLabeledEntry()) {
            assert(current_block.block.start.data.putlabel.name.label.name == has_label.items.len);
            try has_label.append(current_block);
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
        const target = has_label.items[dest_label.label.name];
        want_label.addEdge(target);
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
    var start_type: ir.InstructionName = bb.block.start.data;
    try std.testing.expectEqual(ir.InstructionName.loadi, start_type);

    var finish_type: ir.InstructionName = bb.block.finish.data;
    try std.testing.expectEqual(ir.InstructionName.call, finish_type);

    const following = bb.block.out.?;
    start_type = following.block.start.data;
    try std.testing.expectEqual(ir.InstructionName.leave, start_type);

    finish_type = following.block.finish.data;
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

    try printer.printIR(allocator, method_scope, std.debug);
    const cfg = try buildCFG(allocator, method_scope);
    defer cfg.deinit();

    const block = cfg.head.head.out.?;

    try std.testing.expectEqual(ir.Instruction.jumpunless, @as(ir.InstructionName, block.block.start.data));
    try std.testing.expectEqual(ir.Instruction.jumpunless, @as(ir.InstructionName, block.block.finish.data));
    try std.testing.expectEqual(block.block.finish, block.block.start);
    try std.testing.expect(block.fallsThrough());

    var child = block.block.out.?;
    try std.testing.expectEqual(ir.Instruction.loadi, @as(ir.InstructionName, child.block.start.data));
    try std.testing.expectEqual(ir.Instruction.jump, @as(ir.InstructionName, child.block.finish.data));
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
