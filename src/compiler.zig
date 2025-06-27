const std = @import("std");
const prism = @import("prism.zig");
const vm = @import("vm.zig");
const ir = @import("ir.zig");
const cfg = @import("cfg.zig");
const BasicBlock = cfg.BasicBlock;
const Var = ir.Variable;
const Scope = @import("scope.zig").Scope;

const c = @cImport({
    @cInclude("prism.h");
});

pub const Compiler = struct {
    parser: *c.pm_parser_t,
    scope: ?*Scope,
    allocator: std.mem.Allocator,
    vm: *vm.VM,
    scope_ids: u32 = 0,

    pub fn compile(cc: *Compiler, node: *prism.pm_scope_node_t) error{EmptyInstructionSequence, NotImplementedError, OutOfMemory}!*Scope {
        return compileScopeNode(cc, node, null, false);
    }

    pub fn compileNode(cc: *Compiler, node: *const c.pm_node_t, op: ?*Var, popped: bool) error{NotImplementedError, OutOfMemory}!?*ir.Variable {
        // std.debug.print("--> compiling type {s} {} op: {any}\n", .{c.pm_node_type_to_str(node.*.type), popped, op});
        const opnd = switch (node.*.type) {
            c.PM_BEGIN_NODE => try cc.compileBeginNode(@ptrCast(node), op, popped),
            c.PM_DEF_NODE => try cc.compileDefNode(@ptrCast(node), op, popped),
            c.PM_CALL_NODE => try cc.compileCallNode(@ptrCast(node), op, popped),
            c.PM_ELSE_NODE => try cc.compileElseNode(@ptrCast(node), op, popped),
            c.PM_IF_NODE => try cc.compileIfNode(@ptrCast(node), op, popped),
            c.PM_INTEGER_NODE => try cc.compileIntegerNode(@ptrCast(node), op, popped),
            c.PM_LOCAL_VARIABLE_READ_NODE => try cc.compileLocalVariableReadNode(@ptrCast(node), op, popped),
            c.PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE => try cc.compileLocalVariableOperatorWriteNode(@ptrCast(node), op, popped),
            c.PM_LOCAL_VARIABLE_WRITE_NODE => try cc.compileLocalVariableWriteNode(@ptrCast(node), op, popped),
            c.PM_RETURN_NODE => try cc.compileReturnNode(@ptrCast(node), op, popped),
            c.PM_REQUIRED_PARAMETER_NODE => try cc.compileRequiredParameterNode(@ptrCast(node), op, popped),
            c.PM_SCOPE_NODE => return error.NotImplementedError,
            c.PM_STATEMENTS_NODE => try cc.compileStatementsNode(@ptrCast(node), op, popped),
            c.PM_WHILE_NODE => try cc.compileWhileNode(@ptrCast(node), op, popped),
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            }
        };
        // std.debug.print("<-- compiling type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
        return opnd;
    }

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        if (node) |n| {
            return try cc.compileNode(n, out, popped);
        } else {
            return try cc.pushGetself();
        }
    }

    fn compileBeginNode(cc: *Compiler, node: *const c.pm_begin_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        if (node.*.ensure_clause) |_| {
            return error.NotImplementedError;
        }

        if (node.*.rescue_clause) |_| {
            return error.NotImplementedError;
        }

        return if (node.*.statements) |stmt| try cc.compileNode(@ptrCast(stmt), out, popped) else try cc.pushLoadNil(out);
    }

    fn compileDefNode(cc: *Compiler, node: *const c.pm_def_node_t, out: ?*Var, _: bool) !*ir.Variable {
        const method_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const scope_node = try prism.pmNewScopeNode(@ptrCast(node));
        const method_scope = try cc.compileScopeNode(&scope_node, out, false);

        return try cc.pushDefineMethod(method_name, method_scope);
    }

    fn compileCallNode(cc: *Compiler, node: *const c.pm_call_node_t, out: ?*Var, _: bool) !*Var {
        const method_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const recv_op = try cc.compileRecv(node.*.receiver, null, false);

        var params = std.ArrayList(*Var).init(cc.scope.?.arena.allocator());

        if (node.*.arguments) |argnode| {
            const arg_size = argnode.*.arguments.size;
            const args = argnode.*.arguments.nodes[0..arg_size];

            for (args) |arg| {
                try params.append((try cc.compileNode(arg, null, false)).?);
            }
        }

        // Get a pooled string that's owned by the VM
        const name = try cc.vm.getString(method_name);
        return try cc.pushCall(out, recv_op.?, name, params);
    }

    fn compileElseNode(cc: *Compiler, node: *const c.pm_else_node_t, op: ?*Var, popped: bool) !?*ir.Variable {
        if (node.*.statements) |stmt| {
            return cc.compileNode(@ptrCast(stmt), op, popped);
        } else {
            return try cc.pushLoadNil(op);
        }
    }

    fn compileScopeNode(cc: *Compiler, node: *const prism.pm_scope_node_t, out: ?*Var, popped: bool) !*Scope {
        const locals = node.locals;
        var optionals_list: ?*const c.pm_node_list_t = null;
        var requireds_list: ?*const c.pm_node_list_t = null;
        var keywords_list: ?*const c.pm_node_list_t = null;
        var posts_list: ?*const c.pm_node_list_t = null;

        const parameters_node: ?*const c.pm_parameters_node_t = if (node.*.parameters) |params|
            switch(c.PM_NODE_TYPE(params)) {
                c.PM_PARAMETERS_NODE => @ptrCast(params),
                else => {
                    std.debug.print("unknown parameter type {s}\n", .{c.pm_node_type_to_str(params.*.type)});
                    return error.NotImplementedError;
                }
            }
        else
            null;

        const ast_node = node.*.ast_node;
        const scope_name = switch(c.PM_NODE_TYPE(ast_node)) {
            c.PM_PROGRAM_NODE => "main",
            c.PM_DEF_NODE => blk: {
                const cast: *const c.pm_def_node_t = @ptrCast(ast_node);
                const name = try cc.vm.getString(cc.stringFromId(cast.*.name));
                break :blk name;
            },
            else => {
                std.debug.print("can't get name for {s}\n", .{c.pm_node_type_to_str(ast_node.*.type)});
                return error.NotImplementedError;
            }
        };

        const scope = try Scope.init(cc.allocator, cc.scope_ids, scope_name, cc.scope);
        cc.scope_ids += 1;

        scope.local_storage = locals.size;
        cc.scope = scope;

        if (parameters_node) |params| {
            requireds_list = &params.*.requireds;
            optionals_list = &params.*.optionals;
            keywords_list = &params.*.keywords;
            posts_list = &params.*.posts;

            if (requireds_list) |listptr| {
                scope.param_size = listptr.*.size;
                const list = listptr.*.nodes[0..scope.param_size];
                for (list, 0..) |param, i| {
                    const opnd = (try cc.compileNode(@ptrCast(param), null, popped)).?;
                    _ = try scope.pushGetParam(opnd, i);
                }
            }
        }

        const last_op = if (node.*.body) |body|
            (try cc.compileNode(@ptrCast(body), out, popped)).?
        else
            try cc.pushLoadNil(out);

        _ = try cc.pushLeave(last_op);

        cc.scope = scope.parent;

        return scope;
    }

    fn compileIfNode(cc: *Compiler, node: *const c.pm_if_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        const then_label = cc.newLabel();
        // const else_label = cc.newLabel();
        const end_label = cc.newLabel();

        // If predicate is false, jump to then label
        const predicate = try cc.compilePredicate(node.*.predicate, then_label, JumpType.jump_unless, null, false);

        switch (predicate) {
            .always_true => {
                // Compile the true branch and get a return value
                return try cc.compileNode(@ptrCast(node.*.statements), out, popped);
            },
            .always_false => {
                // Compile the true branch and get a return value
                return try cc.compileNode(@ptrCast(node.*.subsequent), out, popped);
            },
            .unknown => {
                // If someone cares about the return value (in other words,
                // we're _not_ popped), but the output variable is null,
                // then create a new temp and assign it.
                const ret = if (!popped and out == null) try cc.newTemp() else out;

                // Compile the true branch and get a return value
                _ = try cc.compileNode(@ptrCast(node.*.statements), ret, popped);

                // Jump to the end of the if statement
                try cc.pushJump(end_label);

                // Push the then label so the false case has a place to jump
                try cc.pushLabel(then_label);

                _ = try cc.compileNode(@ptrCast(node.*.subsequent), ret, popped);

                try cc.pushLabel(end_label);

                return ret;
            }
        }
    }

    const PredicateType = enum {
        always_true,
        always_false,
        unknown,
    };

    const JumpType = enum {
        jump_if,
        jump_unless
    };

    fn compilePredicate(cc: *Compiler, node: *const c.pm_node_t, label: ir.Label, jump_type: JumpType, out: ?*Var, popped: bool) !PredicateType {
        while (true) {
            switch (node.*.type) {
                c.PM_CALL_NODE, c.PM_LOCAL_VARIABLE_READ_NODE => {
                    const val = try cc.compileNode(node, out, popped);
                    switch(jump_type) {
                        .jump_if => try cc.pushJumpIf(val.?, label),
                        .jump_unless => try cc.pushJumpUnless(val.?, label),
                    }
                    return PredicateType.unknown;
                },
                c.PM_INTEGER_NODE, c.PM_TRUE_NODE => {
                    return PredicateType.always_true;
                },
                c.PM_NIL_NODE, c.PM_FALSE_NODE => {
                    return PredicateType.always_false;
                },
                else => {
                    std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                    return error.NotImplementedError;
                }
            }
        }
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        if (node.*.value.values == null) {
            if (popped) {
                return null;
            } else {
                return try cc.pushLoadi(out, node.*.value.value);
            }
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableReadNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t, _: ?*Var, _: bool) !*Var {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        return try cc.scope.?.getLocalName(lvar_name);
    }

    fn compileLocalVariableOperatorWriteNode(cc: *Compiler, node: *const c.pm_local_variable_operator_write_node_t, _: ?*Var, _: bool) !*ir.Variable {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const recv = try cc.scope.?.getLocalName(lvar_name);

        const op = cc.stringFromId(node.*.binary_operator);
        var params = std.ArrayList(*ir.Variable).init(cc.allocator);
        try params.append((try cc.compileNode(node.*.value, null, false)).?);

        if (op.len == 1) {
            switch (op[0]) {
                '+' => _ = try cc.pushCall(recv, recv, op, params),
                else => return error.NotImplementedError
            }
            return recv;
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableWriteNode(cc: *Compiler, node: *const c.pm_local_variable_write_node_t, _: ?*Var, _: bool) !*ir.Variable {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        const name = try cc.scope.?.getLocalName(lvar_name);
        const out = try cc.compileNode(node.*.value, name, false);
        if (out != name) {
            return try cc.pushMov(name, out.?);
        } else {
            return name;
        }
    }

    fn compileReturnNode(cc: *Compiler, node: *const c.pm_return_node_t, _: ?*Var, _: bool) !*ir.Variable {
        const arguments = node.*.arguments;

        if (arguments) |arg| {
            const arg_size = arg.*.arguments.size;
            const args = arg.*.arguments.nodes[0..arg_size];

            if (arg_size > 1) {
                // need to make a new array
                return error.NotImplementedError;
            } else {
                const inreg = try cc.compileNode(args[0], null, false);
                try cc.pushLeave(inreg.?);
                return inreg.?;
            }
        } else {
            const nil = try cc.pushLoadNil(null);
            try cc.pushLeave(nil);
            return nil;
        }
    }

    fn compileRequiredParameterNode(cc: *Compiler, node: *const c.pm_required_parameter_node, _: ?*Var, _: bool) !*ir.Variable {
        const lvar_name = try cc.vm.getString(cc.stringFromId(node.*.name));
        return try cc.scope.?.getLocalName(lvar_name);
    }

    fn compileStatementsNode(cc: *Compiler, node: *const c.pm_statements_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        const body = &node.*.body;
        const list = body.*.nodes[0..body.*.size];

        if (list.len > 0) {
            var reg: ?*ir.Variable = null;

            for (list, 0..list.len) |item, i| {
                const last_item_p = i == list.len - 1;

                // If it's the last item, we inherit the popped value passed in
                if (last_item_p) {
                    reg = try cc.compileNode(item, out, popped);
                } else {
                    _ = try cc.compileNode(item, null, true);
                }
            }
            // A popped, if statement is basically a void statement and will return
            // a null operand.
            //
            // For example:
            //   ```ruby
            //   def foo(x); x ? 7 : 8; x; end
            //   ```
            // The if statement is popped and void, and each leg of the if
            // statement has "statement" nodes for children.  That's why
            // it's possible for compiling a statement to return a null value
            return reg;
        } else {
            return try cc.pushLoadNil(out);
        }
    }

    fn compileWhileNode(cc: *Compiler, node: *const c.pm_while_node_t, out: ?*Var, popped: bool) !?*ir.Variable {
        const loop_entry = cc.newLabel();
        try cc.pushLabel(loop_entry);

        if ((node.*.base.flags & c.PM_LOOP_FLAGS_BEGIN_MODIFIER) == c.PM_LOOP_FLAGS_BEGIN_MODIFIER) {
            const ret = if (node.*.statements) |stmt|
                try cc.compileNode(@ptrCast(stmt), null, popped)
                else
                    try cc.pushLoadNil(null);

            _ = try cc.compilePredicate(node.*.predicate, loop_entry, JumpType.jump_if, null, false);
            return ret;
        }

        const loop_end = cc.newLabel();

        // If predicate is false, jump to then label
        const predicate = try cc.compilePredicate(node.*.predicate, loop_end, JumpType.jump_unless, null, false);

        switch (predicate) {
            .always_false => { },
            .always_true, .unknown => {
                if (node.*.statements) |stmt| {
                    _ = try cc.compileNode(@ptrCast(stmt), null, popped);
                }
            }
        }

        try cc.pushJump(loop_entry);
        try cc.pushLabel(loop_end);

        if (!popped) {
            return try cc.pushLoadNil(out);
        } else {
            return out;
        }
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    fn newLabel(self: *Compiler) ir.Label {
        return self.scope.?.newLabel();
    }

    fn newTemp(self: *Compiler) !*ir.Variable {
        return try self.scope.?.newTemp();
    }

    fn pushDefineMethod(self: *Compiler, name: []const u8, scope: *Scope) !*ir.Variable {
        return try self.scope.?.pushDefineMethod(name, scope);
    }

    fn pushCall(self: *Compiler, out: ?*ir.Variable, recv: *ir.Variable, name: []const u8, params: std.ArrayList(*ir.Variable)) !*ir.Variable {
        return try self.scope.?.pushCall(out, recv, name, params);
    }

    fn pushGetself(self: *Compiler) !*ir.Variable {
        return try self.scope.?.pushGetself();
    }

    fn pushJump(self: *Compiler, label: ir.Label) !void {
        try self.scope.?.pushJump(label);
    }

    fn pushJumpIf(self: *Compiler, in: *ir.Variable, label: ir.Label) !void {
        return try self.scope.?.pushJumpIf(in, label);
    }

    fn pushJumpUnless(self: *Compiler, in: *ir.Variable, label: ir.Label) !void {
        return try self.scope.?.pushJumpUnless(in, label);
    }

    fn pushLabel(self: *Compiler, label: ir.Label) !void {
        try self.scope.?.pushLabel(label);
    }

    fn pushLeave(self: *Compiler, in: *ir.Variable) !void {
        return try self.scope.?.pushLeave(in);
    }

    fn pushLoadi(self: *Compiler, out: ?*Var, val: u64) !*ir.Variable {
        return try self.scope.?.pushLoadi(out, val);
    }

    fn pushLoadNil(self: *Compiler, out: ?*Var) !*ir.Variable {
        return try self.scope.?.pushLoadNil(out);
    }

    fn pushMov(self: *Compiler, a: *ir.Variable, b: *ir.Variable) !*ir.Variable {
        return try self.scope.?.pushMov(a, b);
    }

    fn pushSetLocal(self: *Compiler, name: *ir.Variable, val: *ir.Variable) !void {
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
        .parser = @ptrCast(parser.parser),
        .vm = m,
        .allocator = allocator,
        .scope = null,
    };
    return cc;
}

pub fn compileString(allocator: std.mem.Allocator, machine: *vm.VM, code: []const u8) !*Scope {
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

    const scope = try compileString(allocator, machine, "5 + 7");
    defer scope.deinit();

    try std.testing.expectEqual(null, scope.parent);
    const insn = scope.insns.first;
    try std.testing.expect(insn != null);
    try expectInstructionType(ir.Instruction.loadi, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);
}

fn expectInstructionType(expected: ir.InstructionName, actual: ir.InstructionName) !void {
    try std.testing.expectEqual(expected, actual);
}

test "compile local set" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "foo = 5; foo");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.leave,
    }, scope.insns);
}

test "compile local get w/ return" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "foo = 5; return foo");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.leave,
    }, scope.insns);
}

test "pushing instruction adds value" {
    const allocator = std.testing.allocator;

    const scope = try Scope.init(allocator, 0, "empty", null);
    defer scope.deinit();

    _ = try scope.pushLoadi(null, 123);
    try std.testing.expectEqual(1, scope.insns.len());

    const insn = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?));
    try std.testing.expectEqual(123, insn.data.loadi.val);
}

test "compile local get w/ nil return" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "foo = 5; return");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.loadnil,
        ir.Instruction.leave,
    }, scope.insns);
}

fn expectInstructionList(expected: []const ir.InstructionName, actual: ir.InstructionList) !void {
    var insn = actual.first;
    for (expected) |expected_insn| {
        try expectInstructionType(expected_insn, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);
        insn = insn.?.next;
    }
}

test "compile ternary statement" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "5 < 7 ? 123 : 456");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.loadi,
        ir.Instruction.call,
        ir.Instruction.jumpunless,
        ir.Instruction.loadi,
        ir.Instruction.jump,
        ir.Instruction.putlabel,
        ir.Instruction.loadi,
        ir.Instruction.putlabel,
    }, scope.insns);
}

test "compile def method" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo; end");
    defer scope.deinit();

    const insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.define_method, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data.define_method.func;
    const method_insns = method_scope.insns;
    try std.testing.expectEqual(2, method_insns.len());
    try std.testing.expectEqual(0, method_scope.param_size);
    try std.testing.expectEqual(0, method_scope.local_storage);
}

test "compile call no params" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "foo");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getself,
        ir.Instruction.call,
        ir.Instruction.leave,
    }, scope.insns);
}

test "compile def method 2 params" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(a, b); end");
    defer scope.deinit();

    const insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.define_method, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data.define_method.func;
    const method_insns = method_scope.insns;
    try std.testing.expectEqual(4, method_insns.len());
    try std.testing.expectEqual(2, method_scope.param_size);
    try std.testing.expectEqual(2, method_scope.local_storage);
}

test "compile def method 2 params 3 locals" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(a, b); c = 123; d = a; e = d + b; end");
    defer scope.deinit();

    const insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.define_method, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data.define_method.func;
    try std.testing.expectEqual(2, method_scope.param_size);
    try std.testing.expectEqual(5, method_scope.local_storage);
}

test "method returns param" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(a); a; end");
    defer scope.deinit();

    const insn = scope.insns.first;
    try expectInstructionType(ir.Instruction.define_method, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data.define_method.func;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getparam,
        ir.Instruction.leave,
    }, method_scope.insns);

    const inop = @as(*ir.InstructionListNode, @fieldParentPtr("node", method_scope.insns.first.?.next.?)).data.leave.in;
    const inop_type: ir.VariableType = inop.data;
    try std.testing.expectEqual(ir.VariableType.local, inop_type);
    try std.testing.expectEqual(0, inop.data.local.id);
}

test "always true ternary" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "6 ? 7 : 8");
    defer scope.deinit();

    try std.testing.expectEqual(2, scope.insns.len());
    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.leave,
    }, scope.insns);

    try std.testing.expectEqual(7, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.loadi.val);
}

test "always false ternary" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "false ? 7 : 8");
    defer scope.deinit();

    try std.testing.expectEqual(2, scope.insns.len());
    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.leave,
    }, scope.insns);

    try std.testing.expectEqual(8, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.loadi.val);
}

test "always nil ternary" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "nil ? 7 : 8");
    defer scope.deinit();

    try std.testing.expectEqual(2, scope.insns.len());
    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.leave,
    }, scope.insns);

    try std.testing.expectEqual(8, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.loadi.val);
}

test "local ternary" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(x); x ? 7 : 8; end");
    defer scope.deinit();

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.define_method.func;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getparam,
        ir.Instruction.jumpunless,
        ir.Instruction.loadi,
        ir.Instruction.jump,
        ir.Instruction.putlabel,
        ir.Instruction.loadi,
        ir.Instruction.putlabel,
        ir.Instruction.leave,
    }, method_scope.insns);

    // Make sure the jump instruction is testing the first parameter
    const test_reg = @as(*ir.InstructionListNode, @fieldParentPtr("node", method_scope.insns.first.?.next.?)).data.jumpunless.in;
    try std.testing.expectEqual(0, test_reg.data.local.id);
}

test "popped if body" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(x); x ? 7 : 8; x; end");
    defer scope.deinit();

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.define_method.func;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getparam,
        ir.Instruction.jumpunless,
        ir.Instruction.jump,
        ir.Instruction.putlabel,
        ir.Instruction.putlabel,
        ir.Instruction.leave,
    }, method_scope.insns);
}

test "simple function" {
    const mem = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(mem);
    defer machine.deinit(mem);

    const scope = try compileString(mem, machine, "def foo(x); x; end");
    defer scope.deinit();

    const scopes = try scope.childScopes(mem);
    defer scopes.deinit();

    const method_scope = scopes.items[0];

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getparam,
        ir.Instruction.leave,
    }, method_scope.insns);
}

test "while loop" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo(x); while x; puts x; end; end");
    defer scope.deinit();

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.define_method.func;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.getparam,
        ir.Instruction.putlabel,
        ir.Instruction.jumpunless,
        ir.Instruction.getself,
        ir.Instruction.call,
        ir.Instruction.jump,
        ir.Instruction.putlabel,
        ir.Instruction.loadnil,
        ir.Instruction.leave,
    }, method_scope.insns);
}

test "empty while loop" {
    const allocator = std.testing.allocator;

    // Create a new VM
    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "def foo; while true; end; end");
    defer scope.deinit();

    const method_scope: *Scope = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?)).data.define_method.func;

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.putlabel,
        ir.Instruction.jump,
        ir.Instruction.putlabel,
        ir.Instruction.loadnil,
        ir.Instruction.leave,
    }, method_scope.insns);
}

test "+=" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "x = 1; x += 1");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.loadi,
        ir.Instruction.call,
        ir.Instruction.leave,
    }, scope.insns);
}

test "local variable write" {
    const allocator = std.testing.allocator;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try compileString(allocator, machine, "x = 1; a = x");
    defer scope.deinit();

    try expectInstructionList(&[_] ir.InstructionName {
        ir.Instruction.loadi,
        ir.Instruction.mov,
        ir.Instruction.leave,
    }, scope.insns);
}
