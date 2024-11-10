const std = @import("std");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const ssa = @import("ssa.zig");

const c = @cImport({
    @cInclude("prism.h");
});

pub const InstructionSequence = struct {
    insns: []const u32,
    ccs: std.ArrayList(vm.CallCache),
};

const Scope = struct {
    reg_number: u32,
    scope_node: *const prism.pm_scope_node_t,
    insns: ssa.InstructionList,
    ccs: std.ArrayList(vm.CallCache),

    pub fn pushInsn(self: *Scope, allocator: std.mem.Allocator, insn: ssa.Instruction) !ssa.Register {
        const node = try allocator.create(ssa.InstructionList.Node);
        node.*.data = insn;
        self.insns.append(node);

        return switch (insn) {
            inline else => |payload| payload.out,
        };
    }

    pub fn pushCallCache(self: *Scope, name: []const u8, argc: usize) !usize {
        const ccid = self.ccs.items.len;
        try self.ccs.append(.{ .method_name = name, .argc = argc }); 
        return ccid;
    }
};

const ScopeList = std.SinglyLinkedList(Scope);

pub const Compiler = struct {
    parser: [*c]const c.pm_parser_t,
    scopes: ScopeList,
    allocator: std.mem.Allocator,
    vm: *vm.VM,

    pub fn compile(cc: *Compiler, node: *prism.pm_scope_node_t) error{NotImplementedError, OutOfMemory}!*InstructionSequence {
        return compileScopeNode(cc, node);
    }

    pub fn compileNode(cc: *Compiler, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!ssa.Register {
        return switch (node.*.type) {
            c.PM_CALL_NODE => try cc.compileCallNode(@ptrCast(node)),
            c.PM_INTEGER_NODE => try cc.compileIntegerNode(@ptrCast(node)),
            c.PM_SCOPE_NODE => return error.NotImplementedError,
            c.PM_STATEMENTS_NODE => try cc.compileStatementsNode(@ptrCast(node)),
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            }
        };
    }

    fn newRegister(cc: *Compiler) !ssa.Register {
        const reg = cc.scopes.first.?.data.reg_number;
        cc.scopes.first.?.data.reg_number += 1;
        return .{ .number = reg, };
    }

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t) !ssa.Register {
        if (node != null) {
            return try cc.compileNode(node);
        } else {
            const outreg = try cc.newRegister();
            return try cc.pushInsn(.{ .getself = .{ .out = outreg, } });
        }
    }

    fn compileCallNode(cc: *Compiler, node: *const c.pm_call_node_t) !ssa.Register {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, node.*.name);

        const method_name = constant.*.start[0..(constant.*.length)];

        const recv_op = try cc.compileRecv(node.*.receiver);

        const arg_size = node.*.arguments.*.arguments.size;
        const args = node.*.arguments.*.arguments.nodes[0..arg_size];

        var params = std.ArrayList(ssa.Register).init(cc.allocator);

        for (args) |arg| {
            try params.append(try cc.compileNode(arg));
        }

        // Get a pooled string that's owned by the VM
        const name = try cc.vm.getString(method_name);

        // Add an inline cache record
        const ccid = try cc.scopes.first.?.data.pushCallCache(name, arg_size);

        return try cc.pushInsn(.{
            .call = .{
                .out = try cc.newRegister(),
                .funcreg = try cc.pushInsn(.{
                    .getmethod = .{
                        .out = try cc.newRegister(),
                        .recv = recv_op,
                        .ccid = ccid,
                    }
                }),

                .params = params
            }
        });
    }

    fn compileScopeNode(cc: *Compiler, node: *prism.pm_scope_node_t) !*InstructionSequence {
        if (node.*.parameters != null) {
            return error.NotImplementedError;
        }

        const scope = try cc.allocator.create(ScopeList.Node);

        scope.* = ScopeList.Node {
            .data = .{
                .reg_number = 0,
                .scope_node = node,
                .insns = ssa.InstructionList { },
                .ccs = std.ArrayList(vm.CallCache).init(cc.allocator),
            }
        };
        cc.scopes.prepend(scope);

        _ = try cc.compileNode(node.*.body);

        const cfg = try ssa.buildCFG(scope.data.insns);
        const iseq = try cc.allocator.create(InstructionSequence);
        iseq.* = .{
            .insns = ssa.compileCFG(cfg),
            .ccs = scope.data.ccs,
        };
        return iseq;
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !ssa.Register {
        if (node.*.value.values == null) {
            const outreg = try cc.newRegister();

            return try cc.pushInsn(.{
                .loadi = .{
                    .out = outreg,
                    .val = node.*.value.value,
                }
            });
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileStatementsNode(cc: *Compiler, node: [*c]const c.pm_statements_node_t) !ssa.Register {
        const body = &node.*.body;
        if (body.*.size > 0) {
            const list = body.*.nodes[0..body.*.size - 1];
            for (list, 0..body.*.size - 1) |item, i| {
                _ = item;
                _ = i;
                return error.NotImplementedError;
            }
            std.debug.print("len {d}\n", .{body.*.size});
            return try cc.compileNode(body.*.nodes[body.*.size - 1]);
        } else {
            return error.NotImplementedError;
        }
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    fn pushInsn(self: *Compiler, insn: ssa.Instruction) !ssa.Register {
        return try self.scopes.first.?.data.pushInsn(self.allocator, insn);
    }
};

pub fn init(allocator: std.mem.Allocator, m: *vm.VM, parser: [*c]const c.pm_parser_t) !*Compiler {
    const cc = try allocator.create(Compiler);
    cc.* = Compiler {
        .parser = parser,
        .vm = m,
        .allocator = allocator,
        .scopes = ScopeList { },
    };
    return cc;
}
