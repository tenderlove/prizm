const std = @import("std");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const ir = @import("ir.zig");

const c = @cImport({
    @cInclude("prism.h");
});

const Scope = struct {
    tmpname: u32,
    insns: ir.InstructionList,
    parent: ?*Scope,
    children: std.ArrayList(Scope),
    locals: std.StringHashMapUnmanaged(LocalInfo),
    ccs: std.ArrayList(vm.CallCache),
    allocator: std.mem.Allocator,

    const LocalInfo = struct {
        name: []const u8,
        irname: ir.Name,
    };

    pub fn getLocalName(self: *Scope, name: []const u8) !ir.Name {
        const info = self.locals.get(name);
        if (info) |v| {
            return v.irname;
        } else {
            const lname: ir.Name = .{
                .local = .{
                    .name = self.tmpname,
                }
            };
            const li: LocalInfo = .{
                .name = name,
                .irname = lname,
            };
            self.tmpname += 1;
            try self.locals.put(self.allocator, name, li);
            return lname;
        }
    }

    fn newTempName(self: *Scope) ir.Name {
        const name = self.tmpname;
        self.tmpname += 1;
        return .{ .temp = .{ .name = name } };
    }

    fn newLabel(self: *Scope) ir.Name {
        const name = self.tmpname;
        self.tmpname += 1;
        return .{ .label = .{ .name = name } };
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        const node = try self.allocator.create(ir.InstructionList.Node);
        node.*.data = insn;
        self.insns.append(node);
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !ir.Name {
        const node = try self.allocator.create(ir.InstructionList.Node);
        node.*.data = insn;
        self.insns.append(node);

        return switch (insn) {
            .label => unreachable,
            .jumpunless => unreachable,
            inline else => |payload| payload.out
        };
    }

    pub fn pushCall(self: *Scope, recv: ir.Name, name: []const u8, params: std.ArrayList(ir.Name)) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .call = .{
            .out = outreg,
            .recv = recv,
            .name = name,
            .params = params,
        } });
    }

    pub fn pushGetLocal(self: *Scope, in: ir.Name) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .getlocal = .{ .out = outreg, .in = in } });
    }

    pub fn pushGetself(self: *Scope) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .getself = .{ .out = outreg } });
    }

    pub fn pushJumpUnless(self: *Scope, in: ir.Name, label: ir.Name) !void {
        try self.pushVoidInsn(.{ .jumpunless = .{ .in = in, .label = label } });
    }

    pub fn pushLabel(self: *Scope, name: ir.Name) !void {
        try self.pushVoidInsn(.{ .label = .{ .name = name } });
    }

    pub fn pushLeave(self: *Scope, in: ir.Name) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .leave = .{ .out = outreg, .in = in } });
    }

    pub fn pushLoadi(self: *Scope, val: u64) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .loadi = .{ .out = outreg, .val = val } });
    }

    pub fn pushLoadNil(self: *Scope) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .loadnil = .{ .out = outreg } });
    }

    pub fn pushPhi(self: *Scope, a: ir.Name, b: ir.Name) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .phi = .{ .out = outreg, .a = a, .b = b } });
    }

    pub fn pushSetLocal(self: *Scope, name: ir.Name, val: ir.Name) !ir.Name {
        const outreg = self.newTempName();
        return try self.pushInsn(.{ .setlocal = .{ .out = outreg, .name = name, .val = val } });
    }

    pub fn init(alloc: std.mem.Allocator, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        scope.* = Scope {
            .tmpname = 0,
            .insns = ir.InstructionList { },
            .parent = parent,
            .locals = std.StringHashMapUnmanaged(Scope.LocalInfo){},
            .children = std.ArrayList(Scope).init(alloc),
            .ccs = std.ArrayList(vm.CallCache).init(alloc),
            .allocator = alloc,
        };

        return scope;
    }

    pub fn deinit(self: *Scope) void {
        self.ccs.deinit();
        var it = self.insns.first;
        while (it) |insn| {
            it = insn.next;
            insn.data.deinit();
            self.allocator.destroy(insn);
        }
        self.locals.deinit(self.allocator);
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

    pub fn compileNode(cc: *Compiler, node: *const c.pm_node_t) error{NotImplementedError, OutOfMemory}!ir.Name {
        std.debug.print("compiling type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
        return switch (node.*.type) {
            c.PM_CALL_NODE => try cc.compileCallNode(@ptrCast(node)),
            c.PM_ELSE_NODE => try cc.compileElseNode(@ptrCast(node)),
            c.PM_IF_NODE => try cc.compileIfNode(@ptrCast(node)),
            c.PM_INTEGER_NODE => try cc.compileIntegerNode(@ptrCast(node)),
            c.PM_LOCAL_VARIABLE_READ_NODE => try cc.compileLocalVariableReadNode(@ptrCast(node)),
            c.PM_LOCAL_VARIABLE_WRITE_NODE => try cc.compileLocalVariableWriteNode(@ptrCast(node)),
            c.PM_RETURN_NODE => try cc.compileReturnNode(@ptrCast(node)),
            c.PM_SCOPE_NODE => return error.NotImplementedError,
            c.PM_STATEMENTS_NODE => try cc.compileStatementsNode(@ptrCast(node)),
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            }
        };
    }

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t) !ir.Name {
        if (node) |n| {
            return try cc.compileNode(n);
        } else {
            return try cc.pushGetself();
        }
    }

    fn compileCallNode(cc: *Compiler, node: *const c.pm_call_node_t) !ir.Name {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, node.*.name);

        const method_name = constant.*.start[0..(constant.*.length)];

        const recv_op = try cc.compileRecv(node.*.receiver);

        const arg_size = node.*.arguments.*.arguments.size;
        const args = node.*.arguments.*.arguments.nodes[0..arg_size];

        var params = std.ArrayList(ir.Name).init(cc.allocator);

        for (args) |arg| {
            try params.append(try cc.compileNode(arg));
        }

        // Get a pooled string that's owned by the VM
        const name = try cc.vm.getString(method_name);

        return try cc.pushCall(recv_op, name, params);
    }

    fn compileElseNode(cc: *Compiler, node: *const c.pm_else_node_t) !ir.Name {
        if (node.*.statements) |stmt| {
            return cc.compileNode(@ptrCast(stmt));
        } else {
            return try cc.pushLoadNil();
        }
    }

    fn compileScopeNode(cc: *Compiler, node: *prism.pm_scope_node_t) !*Scope {
        if (node.*.parameters != null) {
            return error.NotImplementedError;
        }

        const scope = try Scope.init(cc.allocator, cc.scope);

        cc.scope = scope;

        if (node.*.body) |body| {
            _ = try cc.compileNode(body);
        }

        cc.scope = scope.parent;

        return scope;
    }

    fn compileIfNode(cc: *Compiler, node: *const c.pm_if_node_t) !ir.Name {
        const then_label = try cc.newLabel();
        // const else_label = cc.newLabel();
        const end_label = try cc.newLabel();

        // If predicate is false, jump to then label
        try cc.compilePredicate(node.*.predicate, then_label);

        // Compile the true branch and get a return value
        const true_branch = try cc.compileNode(@ptrCast(node.*.statements));

        // Push the then label so the false case has a place to jump
        try cc.pushLabel(then_label);

        const false_branch = try cc.compileNode(@ptrCast(node.*.subsequent));
        try cc.pushLabel(end_label);

        // If anyone cares about the return value of this if statement, then
        // we know for sure we need a phi node here.  Caller might ignore it,
        // but that's up to them.
        return try cc.pushPhi(true_branch, false_branch);
    }

    fn compilePredicate(cc: *Compiler, node: *const c.pm_node_t, then_label: ir.Name) !void {
        while (true) {
            switch (node.*.type) {
                c.PM_CALL_NODE => {
                    const val = try cc.compileNode(node);
                    return try cc.pushJumpUnless(val, then_label);
                },
                else => {
                    std.debug.print("unknown cond type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                    return error.NotImplementedError;
                }
            }
        }
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !ir.Name {
        if (node.*.value.values == null) {
            return try cc.pushLoadi(node.*.value.value);
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableReadNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t) !ir.Name {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const inreg = try cc.scope.?.getLocalName(lvar_name);
        return try cc.pushGetLocal(inreg);
    }

    fn compileLocalVariableWriteNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t) !ir.Name {
        const inreg = try cc.compileNode(node.*.value);
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const name = try cc.scope.?.getLocalName(lvar_name);

        return try cc.pushSetLocal(name, inreg);
    }

    fn compileReturnNode(cc: *Compiler, node: *const c.pm_return_node_t) !ir.Name {
        const arguments = node.*.arguments;

        if (arguments) |arg| {
            const arg_size = arg.*.arguments.size;
            const args = arg.*.arguments.nodes[0..arg_size];

            if (arg_size > 1) {
                // need to make a new array
                return error.NotImplementedError;
            } else {
                const inreg = try cc.compileNode(args[0]);
                return try cc.pushLeave(inreg);
            }
        } else {
            return try cc.pushLeave(try cc.pushLoadNil());
        }
    }

    fn compileStatementsNode(cc: *Compiler, node: *const c.pm_statements_node_t) !ir.Name {
        const body = &node.*.body;
        const list = body.*.nodes[0..body.*.size];
        var reg: ?ir.Name = null;
        for (list) |item| {
            reg = try cc.compileNode(item);
        }
        return reg orelse error.NotImplementedError;
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    fn newLabel(self: *Compiler) !ir.Name {
        return self.scope.?.newLabel();
    }

    fn pushCall(self: *Compiler, recv: ir.Name, name: []const u8, params: std.ArrayList(ir.Name)) !ir.Name {
        return try self.scope.?.pushCall(recv, name, params);
    }

    fn pushGetLocal(self: *Compiler, in: ir.Name) !ir.Name {
        return try self.scope.?.pushGetLocal(in);
    }

    fn pushGetself(self: *Compiler) !ir.Name {
        return try self.scope.?.pushGetself();
    }

    fn pushJumpUnless(self: *Compiler, in: ir.Name, label: ir.Name) !void {
        return try self.scope.?.pushJumpUnless(in, label);
    }

    fn pushLabel(self: *Compiler, label: ir.Name) !void {
        try self.scope.?.pushLabel(label);
    }

    fn pushLeave(self: *Compiler, in: ir.Name) !ir.Name {
        return try self.scope.?.pushLeave(in);
    }

    fn pushLoadi(self: *Compiler, val: u64) !ir.Name {
        return try self.scope.?.pushLoadi(val);
    }

    fn pushLoadNil(self: *Compiler) !ir.Name {
        return try self.scope.?.pushLoadNil();
    }

    fn pushPhi(self: *Compiler, a: ir.Name, b: ir.Name) !ir.Name {
        return try self.scope.?.pushPhi(a, b);
    }

    fn pushSetLocal(self: *Compiler, name: ir.Name, val: ir.Name) !ir.Name {
        return try self.scope.?.pushSetLocal(name, val);
    }

    fn stringFromId(cc: *Compiler, id: c.pm_constant_id_t) []const u8 {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, id);
        return constant.*.start[0..(constant.*.length)];
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

fn compileScope(allocator: std.mem.Allocator, machine: *vm.VM, code: []const u8) !*Scope {
    const parser = try prism.Prism.newParserCtx(allocator);
    defer parser.deinit();
    parser.init(code, code.len, null);
    const root = parser.parse();
    defer parser.nodeDestroy(root);

    var scope_node = try prism.pmNewScopeNode(root);
    const cc = try init(allocator, machine, parser);
    defer cc.deinit(allocator);
    return try cc.compile(&scope_node);
}

test "compile math" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "5 + 7");
    defer scope.deinit();

    try std.testing.expectEqual(null, scope.parent);
    const insn = scope.insns.first;
    try std.testing.expect(insn != null);
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);
}

fn expectInstructionType(expected: ir.InstructionName, actual: ir.InstructionName) !void {
    try std.testing.expectEqual(expected, actual);
}

test "compile local set" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "foo = 5; foo");
    defer scope.deinit();

    var insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.getlocal, insn.?.data);
}

test "compile local get w/ return" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "foo = 5; return foo");
    defer scope.deinit();

    var insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.getlocal, insn.?.data);
}

test "pushing instruction adds value" {
    const allocator = std.testing.allocator;

    const scope = try Scope.init(allocator, null);
    defer scope.deinit();

    _ = try scope.pushLoadi(123);
    try std.testing.expectEqual(1, scope.insns.len);

    const insn = scope.insns.first.?;
    try std.testing.expectEqual(123, insn.data.loadi.val);
}

test "compile local get w/ nil return" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "foo = 5; return");
    defer scope.deinit();

    var insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.loadnil, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.leave, insn.?.data);
}

test "compile ternary statement" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "5 < 7 ? 123 : 456");
    defer scope.deinit();

    var insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.call, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.jumpunless, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.label, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.label, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ir.Instruction.phi, insn.?.data);
}
