const std = @import("std");
const ssa = @import("ssa.zig");
const ir = @import("ir.zig");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const compiler = @import("compiler.zig");

pub const BasicBlock = struct {
    name: u64,
    start: *ir.InstructionList.Node,
    finish: *ir.InstructionList.Node,
    out: ?*BasicBlock,
    out2: ?*BasicBlock,

    fn addInstruction(self: *BasicBlock, insn: *ir.InstructionList.Node) void {
        self.finish = insn;
    }
};

pub const CompileError = error {
    EmptyInstructionSequence,
};

pub fn buildCFG(allocator: std.mem.Allocator, insns: ir.InstructionList) !* const BasicBlock {
    var node = insns.first;
    var block_name: usize = 0;

    if (node) |unwrap| {
        var current_block = try allocator.create(BasicBlock);

        current_block.* = .{
            .name = block_name,
            .start = unwrap,
            .finish = unwrap,
            .out = null,
            .out2 = null,
        };
        const first_block = current_block;
        block_name += 1;

        while (node) |_| {
            var finish = node;

            while (finish) |finish_insn| {
                current_block.addInstruction(finish_insn);

                if (finish_insn.data.isJump()) {
                    break;
                }
                finish = finish_insn.next;
            }

            if (finish) |fin| {
                node = fin.next;
            } else {
                break;
            }
        }
        return first_block;
    } else {
        return CompileError.EmptyInstructionSequence;
    }
}

test "empty basic block" {
    const list = ir.InstructionList { };
    try std.testing.expectError(error.EmptyInstructionSequence, buildCFG(std.testing.allocator, list));
}

test "basic block one instruction" {
    var list = ir.InstructionList { };
    var one = ir.InstructionList.Node {
        .data = .{ .getself = .{ .out = .{ .temp = .{ .name = 0 }}} }
    };
    list.append(&one);
    const bb = try buildCFG(std.testing.allocator, list);
    defer std.testing.allocator.destroy(bb);

    try std.testing.expectEqual(&one, bb.start);
    try std.testing.expectEqual(&one, bb.finish);
    try std.testing.expectEqual(null, bb.out);
    try std.testing.expectEqual(null, bb.out2);
}

test "basic block two instruction" {
    var list = ir.InstructionList { };
    var one = ir.InstructionList.Node {
        .data = .{ .getself = .{ .out = .{ .temp = .{ .name = 0 }}} }
    };
    list.append(&one);

    var two = ir.InstructionList.Node {
        .data = .{ .call = .{
            .out = .{ .temp = .{ .name = 0 } },
            .recv = .{ .temp = .{ .name = 0 } },
            .name = "hello",
            .params = std.ArrayList(ir.Name).init(std.testing.allocator),
        }}
    };
    list.append(&two);
    const bb = try buildCFG(std.testing.allocator, list);
    defer std.testing.allocator.destroy(bb);

    try std.testing.expectEqual(&one, bb.start);
    try std.testing.expectEqual(&two, bb.finish);
    try std.testing.expectEqual(&two, list.last);
    try std.testing.expectEqual(null, bb.out);
    try std.testing.expectEqual(null, bb.out2);
}

test "CFG from compiler" {
    const allocator = std.testing.allocator;

    const parser = try prism.Prism.newParserCtx(allocator);
    defer parser.deinit();
    const code = "5 + 7";
    parser.init(code, code.len, null);
    const root = parser.parse();
    defer parser.nodeDestroy(root);

    var scope_node = try prism.pmNewScopeNode(root);

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    // Compile the parse tree
    const cc = try compiler.init(allocator, machine, parser);
    defer cc.deinit(allocator);
    const scope = try cc.compile(&scope_node);
    defer scope.deinit();

    const cfg = try buildCFG(allocator, scope.insns);
    defer allocator.destroy(cfg);
    const start_type: ir.InstructionName = cfg.start.data;
    try std.testing.expectEqual(ir.InstructionName.loadi, start_type);

    const finish_type: ir.InstructionName = cfg.finish.data;
    try std.testing.expectEqual(ir.InstructionName.call, finish_type);
}
