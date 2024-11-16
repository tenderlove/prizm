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
    locals: std.StringHashMapUnmanaged(LocalInfo),
    ccs: std.ArrayList(vm.CallCache),
    allocator: std.mem.Allocator,

    const LocalInfo = struct {
        name: []const u8,
        reg: ssa.Register,
    };

    pub fn registerLocal(self: *Scope, name: []const u8, reg: ssa.Register) !void {
        const info = self.locals.get(name);
        if (info == null) {
            try self.locals.put(self.allocator, name, .{ .name = name, .reg = reg });
        }
    }

    pub fn getLocalRegister(self: *Scope, name: []const u8) ?ssa.Register {
        const info = self.locals.get(name);
        if (info) |v| {
            return v.reg;
        } else {
            return null;
        }
    }

    fn newRegister(self: *Scope) ssa.Register {
        const reg = self.reg_number;
        self.reg_number += 1;
        return .{ .number = reg, };
    }

    fn pushInsn(self: *Scope, insn: ssa.Instruction) !ssa.Register {
        const node = try self.allocator.create(ssa.InstructionList.Node);
        node.*.data = insn;
        self.insns.append(node);

        return switch (insn) {
            inline else => |payload| blk: {
                var reg = payload.out;
                reg.origin = node;
                break :blk reg;
            },
        };
    }

    pub fn pushCall(self: *Scope, func: ssa.Register, params: std.ArrayList(ssa.Register)) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .call = .{
            .out = outreg,
            .funcreg = func,
            .params = params,
        } });
    }

    pub fn pushGetLocal(self: *Scope, in: ssa.Register) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .getlocal = .{ .out = outreg, .in = in } });
    }

    pub fn pushGetMethod(self: *Scope, recv: ssa.Register, ccid: usize) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .getmethod = .{
            .out = outreg,
            .recv = recv,
            .ccid = ccid,
        } });
    }

    pub fn pushGetself(self: *Scope) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .getself = .{ .out = outreg } });
    }

    pub fn pushLeave(self: *Scope, in: ssa.Register) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .leave = .{ .out = outreg, .in = in } });
    }

    pub fn pushLoadi(self: *Scope, val: u64) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .loadi = .{ .out = outreg, .val = val } });
    }

    pub fn pushLoadNil(self: *Scope) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .loadnil = .{ .out = outreg } });
    }

    pub fn pushSetLocal(self: *Scope, in: ssa.Register) !ssa.Register {
        const outreg = self.newRegister();
        return try self.pushInsn(.{ .setlocal = .{ .out = outreg, .in = in } });
    }

    pub fn pushCallCache(self: *Scope, name: []const u8, argc: usize) !usize {
        const ccid = self.ccs.items.len;
        try self.ccs.append(.{ .method_name = name, .argc = argc }); 
        return ccid;
    }

    pub fn init(alloc: std.mem.Allocator, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        scope.* = Scope {
            .reg_number = 0,
            .insns = ssa.InstructionList { },
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

    pub fn compileNode(cc: *Compiler, node: *const c.pm_node_t) error{NotImplementedError, OutOfMemory}!ssa.Register {
        std.debug.print("compiling type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
        return switch (node.*.type) {
            c.PM_CALL_NODE => try cc.compileCallNode(@ptrCast(node)),
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

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t) !ssa.Register {
        if (node) |n| {
            return try cc.compileNode(n);
        } else {
            return try cc.pushGetself();
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

        const func = try cc.pushGetMethod(recv_op, ccid);
        return try cc.pushCall(func, params);
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

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !ssa.Register {
        if (node.*.value.values == null) {
            return try cc.pushLoadi(node.*.value.value);
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableReadNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t) !ssa.Register {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const inreg = cc.scope.?.getLocalRegister(lvar_name);

        if (inreg) |v| {
            return try cc.pushGetLocal(v);
        } else {
            // Probably getupvalue (from scope above us)
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableWriteNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t) !ssa.Register {
        const inreg = try cc.compileNode(node.*.value);

        const outreg = try cc.pushSetLocal(inreg);
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));

        try cc.scope.?.registerLocal(lvar_name, outreg);

        return outreg;
    }

    fn compileReturnNode(cc: *Compiler, node: *const c.pm_return_node_t) !ssa.Register {
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

    fn compileStatementsNode(cc: *Compiler, node: *const c.pm_statements_node_t) !ssa.Register {
        const body = &node.*.body;
        const list = body.*.nodes[0..body.*.size];
        var reg: ?ssa.Register = null;
        for (list) |item| {
            reg = try cc.compileNode(item);
        }
        return reg orelse error.NotImplementedError;
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    fn pushCall(self: *Compiler, func: ssa.Register, params: std.ArrayList(ssa.Register)) !ssa.Register {
        return try self.scope.?.pushCall(func, params);
    }

    fn pushGetLocal(self: *Compiler, in: ssa.Register) !ssa.Register {
        return try self.scope.?.pushGetLocal(in);
    }

    fn pushGetMethod(self: *Compiler, recv: ssa.Register, ccid: usize) !ssa.Register {
        return try self.scope.?.pushGetMethod(recv, ccid);
    }

    fn pushGetself(self: *Compiler) !ssa.Register {
        return try self.scope.?.pushGetself();
    }

    fn pushLeave(self: *Compiler, in: ssa.Register) !ssa.Register {
        return try self.scope.?.pushLeave(in);
    }

    fn pushLoadi(self: *Compiler, val: u64) !ssa.Register {
        return try self.scope.?.pushLoadi(val);
    }

    fn pushLoadNil(self: *Compiler) !ssa.Register {
        return try self.scope.?.pushLoadNil();
    }

    fn pushSetLocal(self: *Compiler, in: ssa.Register) !ssa.Register {
        return try self.scope.?.pushSetLocal(in);
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
    try expectInstructionType(ssa.Instruction.loadi, insn.?.data);
}

fn expectInstructionType(expected: ssa.InstructionName, actual: ssa.InstructionName) !void {
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
    try expectInstructionType(ssa.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.getlocal, insn.?.data);
}

test "compile local get w/ return" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileScope(allocator, machine, "foo = 5; return foo");
    defer scope.deinit();

    var insn = scope.insns.first;
    try expectInstructionType(ssa.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.getlocal, insn.?.data);
}

test "pushing instruction links origin to outreg" {
    const allocator = std.testing.allocator;

    const scope = try Scope.init(allocator, null);
    defer scope.deinit();

    const reg = try scope.pushLoadi(123);
    try std.testing.expectEqual(1, scope.insns.len);

    const insn = scope.insns.first.?;
    try std.testing.expectEqual(insn, reg.origin.?);
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
    try expectInstructionType(ssa.Instruction.loadi, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.setlocal, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.loadnil, insn.?.data);

    insn = insn.?.next;
    try expectInstructionType(ssa.Instruction.leave, insn.?.data);
}
