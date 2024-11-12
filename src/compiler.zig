const std = @import("std");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const ssa = @import("ssa.zig");

const c = @cImport({
    @cInclude("prism.h");
});

const Scope = struct {
    reg_number: u32,
    insns: ssa.InstructionList,
    parent: ?*Scope,
    children: std.ArrayList(Scope),
    ccs: std.ArrayList(vm.CallCache),
    allocator: std.mem.Allocator,

    pub fn newRegister(self: *Scope) ssa.Register {
        const reg = self.reg_number;
        self.reg_number += 1;
        return .{ .number = reg, };
    }

    pub fn pushInsn(self: *Scope, insn: ssa.Instruction) !ssa.Register {
        const node = try self.allocator.create(ssa.InstructionList.Node);
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

    pub fn deinit(self: *Scope) void {
        self.ccs.deinit();
        var it = self.insns.first;
        while (it) |insn| {
            it = insn.next;
            insn.data.deinit();
            self.allocator.destroy(insn);
        }
        self.allocator.destroy(self);
    }
};

pub const Compiler = struct {
    parser: *const c.pm_parser_t,
    scope: ?*Scope,
    allocator: std.mem.Allocator,
    vm: *vm.VM,

    pub fn compile(cc: *Compiler, node: *prism.pm_scope_node_t) error{EmptyInstructionSequence, NotImplementedError, OutOfMemory}!*Scope {
        return compileScopeNode(cc, node);
    }

    pub fn compileNode(cc: *Compiler, node: *const c.pm_node_t) error{NotImplementedError, OutOfMemory}!ssa.Register {
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
        return cc.scope.?.newRegister();
    }

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t) !ssa.Register {
        if (node) |n| {
            return try cc.compileNode(n);
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
        const ccid = try cc.scope.?.pushCallCache(name, arg_size);

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

    fn compileScopeNode(cc: *Compiler, node: *prism.pm_scope_node_t) !*Scope {
        if (node.*.parameters != null) {
            return error.NotImplementedError;
        }

        const scope = try cc.allocator.create(Scope);

        scope.* = Scope {
            .reg_number = 0,
            .insns = ssa.InstructionList { },
            .parent = cc.scope,
            .children = std.ArrayList(Scope).init(cc.allocator),
            .ccs = std.ArrayList(vm.CallCache).init(cc.allocator),
            .allocator = cc.allocator,
        };

        cc.scope = scope;

        if (node.*.body) |body| {
            _ = try cc.compileNode(body);
        }

        cc.scope = scope.parent;

        return scope;
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
        return try self.scope.?.pushInsn(insn);
    }
};

pub fn init(allocator: std.mem.Allocator, m: *vm.VM, parser: *prism.Prism) !*Compiler {
    const cc = try allocator.create(Compiler);
    cc.* = Compiler {
        .parser = parser.parser,
        .vm = m,
        .allocator = allocator,
        .scope = null,
    };
    return cc;
}

test "compile math" {
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
    const cc = try init(allocator, machine, parser);
    defer cc.deinit(allocator);
    const scope = try cc.compile(&scope_node);
    defer scope.deinit();
}
