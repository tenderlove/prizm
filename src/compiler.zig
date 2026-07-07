const std = @import("std");
const prism = @import("prism.zig");
const Globals = @import("globals.zig").Globals;
const ir = @import("ir.zig");
const Insn = ir.InstructionListNode;
const cfg = @import("cfg.zig");
const BasicBlock = @import("basic_block.zig").BasicBlock;
const Scope = @import("scope.zig").Scope;

const c = @import("prism_c");

pub const Compiler = struct {
    parser: *c.pm_parser_t,
    allocator: std.mem.Allocator,
    globals: *Globals,
    scope_ids: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, m: *Globals, parser: *prism.Prism) !*Compiler {
        const cc = try allocator.create(Compiler);
        cc.* = Compiler{
            .parser = @ptrCast(parser.parser),
            .globals = m,
            .allocator = allocator,
        };
        return cc;
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }

    pub fn compile(cc: *Compiler, node: *prism.pm_scope_node_t) error{ EmptyInstructionSequence, NotImplementedError, OutOfMemory }!*cfg.CFG {
        const scope = try compileScopeNode(cc, null, node);
        return try scope.cfg(cc.allocator);
    }

    fn compileNode(cc: *Compiler, scope: *Scope, node: *const c.pm_node_t) error{ NotImplementedError, OutOfMemory }!*Insn {
        // std.debug.print("--> compiling type {s} {} op: {any}\n", .{c.pm_node_type_to_str(node.*.type), op});
        const opnd = switch (node.*.type) {
            c.PM_BEGIN_NODE => try cc.compileBeginNode(scope, @ptrCast(node)),
            c.PM_BREAK_NODE => try cc.compileBreakNode(scope, @ptrCast(node)),
            c.PM_NEXT_NODE => try cc.compileNextNode(scope, @ptrCast(node)),
            c.PM_DEF_NODE => try cc.compileDefNode(scope, @ptrCast(node)),
            c.PM_CALL_NODE => try cc.compileCallNode(scope, @ptrCast(node)),
            c.PM_ELSE_NODE => try cc.compileElseNode(scope, @ptrCast(node)),
            c.PM_IF_NODE => try cc.compileIfNode(scope, @ptrCast(node)),
            c.PM_INTEGER_NODE => try cc.compileIntegerNode(scope, @ptrCast(node)),
            c.PM_TRUE_NODE => try cc.compileTrueNode(scope, @ptrCast(node)),
            c.PM_FALSE_NODE => try cc.compileFalseNode(scope, @ptrCast(node)),
            c.PM_LOCAL_VARIABLE_READ_NODE => try cc.compileLocalVariableReadNode(scope, @ptrCast(node)),
            c.PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE => try cc.compileLocalVariableOperatorWriteNode(scope, @ptrCast(node)),
            c.PM_LOCAL_VARIABLE_WRITE_NODE => try cc.compileLocalVariableWriteNode(scope, @ptrCast(node)),
            c.PM_RETURN_NODE => try cc.compileReturnNode(scope, @ptrCast(node)),
            c.PM_REQUIRED_PARAMETER_NODE => try cc.compileRequiredParameterNode(scope, @ptrCast(node)),
            c.PM_SCOPE_NODE => return error.NotImplementedError,
            c.PM_STATEMENTS_NODE => try cc.compileStatementsNode(scope, @ptrCast(node)),
            c.PM_STRING_NODE => try cc.compileStringNode(scope, @ptrCast(node)),
            c.PM_WHILE_NODE => try cc.compileWhileNode(scope, @ptrCast(node)),
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            },
        };
        // std.debug.print("<-- compiling type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
        return opnd;
    }

    fn compileBeginNode(cc: *Compiler, scope: *Scope, node: *const c.pm_begin_node_t) !*Insn {
        if (node.*.ensure_clause) |_| {
            return error.NotImplementedError;
        }

        if (node.*.rescue_clause) |_| {
            return error.NotImplementedError;
        }

        if (node.*.statements) |stmt| {
            return try cc.compileNode(scope, @ptrCast(stmt));
        } else {
            return try scope.pushLoadNil();
        }
    }

    fn compileDefNode(cc: *Compiler, scope: *Scope, node: *const c.pm_def_node_t) !*Insn {
        const method_name = try cc.globals.getString(cc.stringFromId(node.*.name));
        const scope_node = try prism.pmNewScopeNode(@ptrCast(node));
        const method_scope = try cc.compileScopeNode(scope, &scope_node);
        const method_cfg = try method_scope.cfg(cc.allocator);
        return try scope.pushDefineMethod(method_name, method_cfg);
    }

    fn compileCallNode(cc: *Compiler, scope: *Scope, node: *const c.pm_call_node_t) !*Insn {
        const method_name = try cc.globals.getString(cc.stringFromId(node.*.name));

        // If there's a receiver, compile it. Otherwise we know it's a
        // "getself". In the case of a "getself" we want to do that right
        // next to the call in order to reduce register interference
        const recv_op = if (node.*.receiver) |recv|
            try cc.compileNode(scope, recv)
        else
            try scope.readVariable(try cc.dedupString("self"), scope.currentBlock());

        var params: std.ArrayList(*Insn) = .empty;

        if (node.*.arguments) |argnode| {
            const arg_size = argnode.*.arguments.size;
            const args = argnode.*.arguments.nodes[0..arg_size];

            for (args) |arg| {
                try params.append(cc.allocator, (try cc.compileNode(scope, arg)));
            }
        }

        // Get a pooled string that's owned by the VM
        const name = try cc.globals.getString(method_name);
        return try scope.pushCall(recv_op, name, params);
    }

    fn compileElseNode(cc: *Compiler, scope: *Scope, node: *const c.pm_else_node_t) !*Insn {
        if (node.*.statements) |stmt| {
            return cc.compileNode(scope, @ptrCast(stmt));
        } else {
            return try scope.pushLoadNil();
        }
    }

    fn compileScopeNode(cc: *Compiler, parent: ?*Scope, node: *const prism.pm_scope_node_t) !*Scope {
        var optionals_list: ?*const c.pm_node_list_t = null;
        var requireds_list: ?*const c.pm_node_list_t = null;
        var keywords_list: ?*const c.pm_node_list_t = null;
        var posts_list: ?*const c.pm_node_list_t = null;

        const parameters_node: ?*const c.pm_parameters_node_t = if (node.*.parameters) |params|
            switch (c.PM_NODE_TYPE(params)) {
                c.PM_PARAMETERS_NODE => @ptrCast(params),
                else => {
                    std.debug.print("unknown parameter type {s}\n", .{c.pm_node_type_to_str(params.*.type)});
                    return error.NotImplementedError;
                },
            }
        else
            null;

        const ast_node = node.*.ast_node;
        const scope_name = switch (c.PM_NODE_TYPE(ast_node)) {
            c.PM_PROGRAM_NODE => "main",
            c.PM_DEF_NODE => blk: {
                const cast: *const c.pm_def_node_t = @ptrCast(ast_node);
                const name = try cc.globals.getString(cc.stringFromId(cast.*.name));
                break :blk name;
            },
            else => {
                std.debug.print("can't get name for {s}\n", .{c.pm_node_type_to_str(ast_node.*.type)});
                return error.NotImplementedError;
            },
        };

        const scope = try Scope.init(cc.allocator, cc.scope_ids, scope_name, parent);
        try scope.sealBlock(scope.currentBlock());
        cc.scope_ids += 1;

        // Every scope gets a "self" as a parameter
        try scope.writeVariable("self", scope.currentBlock(), try scope.pushGetParam(0));

        if (parameters_node) |params| {
            requireds_list = &params.*.requireds;
            optionals_list = &params.*.optionals;
            keywords_list = &params.*.keywords;
            posts_list = &params.*.posts;

            if (requireds_list) |listptr| {
                const list = listptr.*.nodes[0..listptr.*.size];
                for (list, 0..) |param, i| {
                    // We only expect required parameter nodes here.
                    std.debug.assert(c.PM_NODE_TYPE(param) == c.PM_REQUIRED_PARAMETER_NODE);
                    const p: *const c.pm_required_parameter_node_t = @ptrCast(param);

                    const name = try cc.dedupString(cc.stringFromId(p.*.name));
                    const value = try scope.pushGetParam(i + 1); // 0 is self
                    try scope.writeVariable(name, scope.currentBlock(), value);
                }
            }
        }

        const last_op = if (node.*.body) |body|
            (try cc.compileNode(scope, @ptrCast(body)))
        else
            try scope.pushLoadNil();

        _ = try scope.pushLeave(last_op);

        return scope;
    }

    fn compileIfNode(cc: *Compiler, scope: *Scope, node: *const c.pm_if_node_t) !*Insn {
        const predicate = try cc.compilePredicate(scope, node.*.predicate);

        const then_entry = try scope.newBlock();
        const else_entry = try scope.newBlock();
        const if_exit = try scope.newBlock();

        try scope.pushCond(predicate, then_entry, else_entry);

        // Nobody else can jump to these two blocks
        try scope.sealBlock(then_entry);
        try scope.sealBlock(else_entry);

        // Compile the true branch and get a return value
        scope.setCurrentBlock(then_entry);
        const truthy = try cc.compileNode(scope, @ptrCast(node.*.statements));
        const then_terminated = scope.currentBlock().isTerminated();

        // Check if someone put "return" in the if statement. If they did,
        // compileReturnNode's pushLeave already filled the block; otherwise
        // pushJump does so here.
        if (!then_terminated) {
            try scope.pushJump(if_exit);
        }

        // Compile the false branch and get a return value
        scope.setCurrentBlock(else_entry);
        const falsy = if (node.*.subsequent) |sub|
            try cc.compileNode(scope, @ptrCast(sub))
        else
            try scope.pushLoadNil();

        const else_terminated = scope.currentBlock().isTerminated();

        // Check if someone put a return in the else side. Same logic as
        // the then arm — either compileReturnNode filled it or pushJump does.
        if (!else_terminated) {
            try scope.pushJump(if_exit);
        }

        // Both sides got a return
        if (then_terminated and else_terminated) {
            try scope.sealBlock(if_exit);
            scope.setCurrentBlock(if_exit);
            return try scope.pushLoadNil();
        } else {
            // Nobody else can jump to if_exit now.
            try scope.sealBlock(if_exit);
            scope.setCurrentBlock(if_exit);

            if (then_terminated) {
                return falsy;
            } else if (else_terminated) {
                return truthy;
            } else {
                var phi_params: std.ArrayList(*Insn) = .empty;
                try phi_params.appendSlice(cc.allocator, &.{ truthy, falsy });
                return try scope.pushPhi(phi_params);
            }
        }
    }

    fn compilePredicate(cc: *Compiler, scope: *Scope, node: *const c.pm_node_t) !*Insn {
        while (true) {
            switch (node.*.type) {
                c.PM_CALL_NODE, c.PM_LOCAL_VARIABLE_READ_NODE => {
                    const val = try cc.compileNode(scope, node);
                    return try scope.pushTest(val);
                },
                c.PM_INTEGER_NODE, c.PM_TRUE_NODE => {
                    const val = try cc.compileNode(scope, node);
                    return try scope.pushTest(val);
                },
                c.PM_NIL_NODE, c.PM_FALSE_NODE => {
                    const val = try cc.compileNode(scope, node);
                    return try scope.pushTest(val);
                },
                else => {
                    std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                    return error.NotImplementedError;
                },
            }
        }
    }

    fn compileTrueNode(_: *Compiler, scope: *Scope, _: *const c.pm_true_node_t) !*Insn {
        return try scope.pushLoadTrue();
    }

    fn compileFalseNode(_: *Compiler, scope: *Scope, _: *const c.pm_false_node_t) !*Insn {
        return try scope.pushLoadFalse();
    }

    fn compileIntegerNode(_: *Compiler, scope: *Scope, node: *const c.pm_integer_node_t) !*Insn {
        if (node.*.value.values == null) {
            return try scope.pushLoadi(node.*.value.value);
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileStringNode(cc: *Compiler, scope: *Scope, node: *const c.pm_string_node_t) !*Insn {
        const str = try cc.globals.getString(prism.Prism.unwrapString(node));
        return try scope.pushLoadString(str);
    }

    fn compileLocalVariableReadNode(cc: *Compiler, scope: *Scope, node: *const c.pm_local_variable_write_node_t) !*Insn {
        return try scope.readVariable(try cc.dedupString(cc.stringFromId(node.*.name)), scope.currentBlock());
    }

    fn compileLocalVariableOperatorWriteNode(cc: *Compiler, scope: *Scope, node: *const c.pm_local_variable_operator_write_node_t) !*Insn {
        const name = try cc.dedupString(cc.stringFromId(node.*.name));
        const recv = try scope.readVariable(name, scope.currentBlock());

        const op = cc.stringFromId(node.*.binary_operator);
        var params: std.ArrayList(*Insn) = .empty;
        try params.append(cc.allocator, (try cc.compileNode(scope, node.*.value)));

        if (op.len == 1) {
            const result = switch (op[0]) {
                '+', '-' => try scope.pushCall(recv, op, params),
                else => return error.NotImplementedError,
            };
            try scope.writeVariable(name, scope.currentBlock(), result);
            return result;
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileLocalVariableWriteNode(cc: *Compiler, scope: *Scope, node: *const c.pm_local_variable_write_node_t) !*Insn {
        const out = try cc.compileNode(scope, node.*.value);
        try scope.writeVariable(cc.stringFromId(node.*.name), scope.currentBlock(), out);
        return out;
    }

    fn compileReturnNode(cc: *Compiler, scope: *Scope, node: *const c.pm_return_node_t) !*Insn {
        const arguments = node.*.arguments;

        if (arguments) |arg| {
            const arg_size = arg.*.arguments.size;
            const args = arg.*.arguments.nodes[0..arg_size];

            if (arg_size > 1) {
                // need to make a new array
                return error.NotImplementedError;
            } else {
                const inreg = try cc.compileNode(scope, args[0]);
                return try scope.pushLeave(inreg);
            }
        } else {
            const nil = try scope.pushLoadNil();
            return try scope.pushLeave(nil);
        }
    }

    fn compileBreakNode(cc: *Compiler, scope: *Scope, node: *const c.pm_break_node_t) !*Insn {
        const frame = scope.currentLoopFrame() orelse @panic("`break` outside of a loop");

        // Value: `break` == nil, `break EXPR` == EXPR. Multi-arg `break a, b`
        // would be array-wrapped; not yet handled.
        const val = if (node.*.arguments) |args_ptr| val_blk: {
            const arg_size = args_ptr.*.arguments.size;
            if (arg_size > 1) return error.NotImplementedError;
            const arg = args_ptr.*.arguments.nodes[0..arg_size][0];
            break :val_blk try cc.compileNode(scope, arg);
        } else try scope.pushLoadNil();

        // Route the value into break_target's phi via Braun.
        try scope.writeVariable(frame.break_var, scope.currentBlock(), val);
        try scope.pushJump(frame.break_target);

        // Sentinel — callers who happen to use this value are on a dead path.
        return val;
    }

    fn compileNextNode(cc: *Compiler, scope: *Scope, node: *const c.pm_next_node_t) !*Insn {
        const frame = scope.currentLoopFrame() orelse @panic("`next` outside of a loop");

        // Ruby: `next VALUE` in a `while` loop does NOT set the loop's
        // value (that's `break`'s job). Compile for side effects, discard.
        if (node.*.arguments) |args_ptr| {
            const arg_size = args_ptr.*.arguments.size;
            const args = args_ptr.*.arguments.nodes[0..arg_size];
            for (args) |arg| {
                _ = try cc.compileNode(scope, arg);
            }
        }

        // pushJump fills the block, so any sentinel to hand back must be
        // pushed BEFORE the jump.
        const sentinel = try scope.pushLoadNil();
        try scope.pushJump(frame.next_target);
        return sentinel;
    }

    fn compileRequiredParameterNode(cc: *Compiler, scope: *Scope, node: *const c.pm_required_parameter_node) !*Insn {
        _ = cc;
        _ = scope;
        _ = node;
        @panic("FIXME");
    }

    fn compileStatementsNode(cc: *Compiler, scope: *Scope, node: *const c.pm_statements_node_t) !*Insn {
        const body = &node.*.body;
        const list = body.*.nodes[0..body.*.size];

        if (list.len > 0) {
            var reg: ?*Insn = null;

            for (list) |item| {
                reg = try cc.compileNode(scope, item);
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
            return reg.?;
        } else {
            return try scope.pushLoadNil();
        }
    }

    fn compileWhileNode(cc: *Compiler, scope: *Scope, node: *const c.pm_while_node_t) !*Insn {
        // Handle while loops with begin / end:
        // begin
        //   puts x
        //   x -= 1
        // end while x > 0
        if ((node.*.base.flags & c.PM_LOOP_FLAGS_BEGIN_MODIFIER) == c.PM_LOOP_FLAGS_BEGIN_MODIFIER) {
            const body = try scope.newBlock();
            const cond_check = try scope.newBlock();
            const exit = try scope.newBlock();

            // `next` re-evaluates the condition in do-while, so its target
            // is cond_check (NOT the top of body). `break` exits the loop.
            const break_var = try scope.pushLoopFrame(cond_check, exit);
            defer scope.popLoopFrame();

            try scope.pushJump(body);
            scope.setCurrentBlock(body);

            if (node.*.statements) |stmt| {
                _ = try cc.compileNode(scope, @ptrCast(stmt));
            }
            // Body may terminate via `return`/`break`/`next`; skip fallthrough
            // if it did.
            if (!scope.currentBlock().isTerminated()) {
                try scope.pushJump(cond_check);
            }

            scope.setCurrentBlock(cond_check);

            // Seed break_var with nil on the cond-false edge. Dead if no
            // `break` occurs; picked up by Braun's phi otherwise.
            const nil_natural = try scope.pushLoadNil();
            try scope.writeVariable(break_var, cond_check, nil_natural);

            const cmp = try cc.compilePredicate(scope, node.*.predicate);
            try scope.pushCond(cmp, body, exit);
            try scope.sealBlock(body);
            try scope.sealBlock(cond_check);
            try scope.sealBlock(exit);
            scope.setCurrentBlock(exit);

            return try scope.readVariable(break_var, exit);
        } else { // Regular while loops
            const header = try scope.newBlock();
            const body = try scope.newBlock();
            const exit = try scope.newBlock();

            // `next` jumps to header (which re-evaluates cond); `break`
            // jumps to exit.
            const break_var = try scope.pushLoopFrame(header, exit);
            defer scope.popLoopFrame();

            try scope.pushJump(header);

            scope.setCurrentBlock(header);

            // Seed break_var with nil on the cond-false edge from header.
            const nil_natural = try scope.pushLoadNil();
            try scope.writeVariable(break_var, header, nil_natural);

            // If predicate is false, jump to exit
            const predicate = try cc.compilePredicate(scope, node.*.predicate);

            try scope.pushCond(predicate, body, exit);

            // Compile the loop body
            try scope.sealBlock(body);
            scope.setCurrentBlock(body);
            if (node.*.statements) |stmt| {
                _ = try cc.compileNode(scope, @ptrCast(stmt));
            }
            if (!scope.currentBlock().isTerminated()) {
                try scope.pushJump(header);
            }

            try scope.sealBlock(header);
            try scope.sealBlock(exit);
            scope.setCurrentBlock(exit);

            return try scope.readVariable(break_var, exit);
        }
    }

    fn stringFromId(cc: *Compiler, id: c.pm_constant_id_t) []const u8 {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, id);
        return constant.*.start[0..(constant.*.length)];
    }

    fn dedupString(self: *Compiler, str: []const u8) ![]const u8 {
        return try self.globals.getString(str);
    }

    fn writeVariable(self: *Compiler, name: []const u8, block: *BasicBlock, value: *Insn) !void {
        // deduplicate the string
        const dedup = try self.globals.getString(name);
        try self.scope.?.writeVariable(dedup, block, value);
    }
};

fn expectInstructionType(expected: ir.InstructionName, actual: ir.InstructionName) !void {
    try std.testing.expectEqual(expected, actual);
}

pub fn compileString(allocator: std.mem.Allocator, globals: *Globals, code: []const u8) !*Scope {
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

// test "compile math" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "5 + 7");
//     defer scope.deinit();
//
//     try std.testing.expectEqual(null, scope.parent);
//     const insn = scope.insns.first;
//     try std.testing.expect(insn != null);
//     try expectInstructionType(ir.Instruction.getparam, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);
// }

// test "compile local set" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "foo = 5; foo");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.leave,
//     }, scope.insns);
// }
//
// test "compile local get w/ return" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "foo = 5; return foo");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.leave,
//     }, scope.insns);
// }
//
// test "pushing instruction adds value" {
//     const allocator = std.testing.allocator;
//
//     const scope = try Scope.init(allocator, 0, "empty", null);
//     defer scope.deinit();
//
//     _ = try scope.pushLoadi(null, 123);
//     try std.testing.expectEqual(1, scope.insns.len());
//
//     const insn = @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?));
//     try std.testing.expectEqual(123, insn.data.loadi.val);
// }
//
// test "compile local get w/ nil return" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "foo = 5; return");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.loadnil,
//         ir.Instruction.leave,
//     }, scope.insns);
// }

fn expectInstructionList(expected: []const ir.InstructionName, actual: ir.InstructionList) !void {
    var insn = actual.first;
    for (expected) |expected_insn| {
        try expectInstructionType(expected_insn, @as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?)).data);
        insn = insn.?.next;
    }
}

// test "compile ternary statement" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "5 < 7 ? 123 : 456");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.loadi,
//         ir.Instruction.setparam,
//         ir.Instruction.setparam,
//         ir.Instruction.call,
//         ir.Instruction.mov,
//         ir.Instruction.jumpunless,
//         ir.Instruction.loadi,
//         ir.Instruction.jump,
//         ir.Instruction.putlabel,
//         ir.Instruction.loadi,
//         ir.Instruction.putlabel,
//         ir.Instruction.leave,
//     }, scope.insns);
// }
//
// test "compile def method" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//     const method_insns = method_scope.insns;
//     try std.testing.expectEqual(4, method_insns.len());
//     try std.testing.expectEqual(0, method_scope.param_size);
//     try std.testing.expectEqual(0, method_scope.local_storage);
// }
//
// test "compile call no params" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "foo");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.setparam,
//         ir.Instruction.call,
//         ir.Instruction.mov,
//         ir.Instruction.leave,
//     }, scope.insns);
// }
//
// test "compile def method 2 params" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(a, b); end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//     const method_insns = method_scope.insns;
//     try std.testing.expectEqual(8, method_insns.len());
//     try std.testing.expectEqual(2, method_scope.param_size);
//     try std.testing.expectEqual(2, method_scope.local_storage);
// }
//
// test "compile def method 2 params 3 locals" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(a, b); c = 123; d = a; e = d + b; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//
//     const method_scope = scopes.items[0];
//
//     try std.testing.expectEqual(2, method_scope.param_size);
//     try std.testing.expectEqual(5, method_scope.local_storage);
// }
//
// test "method returns param" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(a); a; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.leave,
//     }, method_scope.insns);
// }
//
// test "always true ternary" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "6 ? 7 : 8");
//     defer scope.deinit();
//
//     try std.testing.expectEqual(4, scope.insns.len());
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.leave,
//     }, scope.insns);
//
//     try std.testing.expectEqual(7, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?.next.?.next.?)).data.loadi.val);
// }
//
// test "always false ternary" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "false ? 7 : 8");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.leave,
//     }, scope.insns);
//
//     try std.testing.expectEqual(8, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?.next.?.next.?)).data.loadi.val);
// }
//
// test "always nil ternary" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "nil ? 7 : 8");
//     defer scope.deinit();
//
//     try std.testing.expectEqual(4, scope.insns.len());
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.leave,
//     }, scope.insns);
//
//     try std.testing.expectEqual(8, @as(*ir.InstructionListNode, @fieldParentPtr("node", scope.insns.first.?.next.?.next.?)).data.loadi.val);
// }
//
// test "local ternary" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(x); x ? 7 : 8; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.jumpunless,
//         ir.Instruction.loadi,
//         ir.Instruction.jump,
//         ir.Instruction.putlabel,
//         ir.Instruction.loadi,
//         ir.Instruction.putlabel,
//         ir.Instruction.leave,
//     }, method_scope.insns);
//
//     // Make sure the jump instruction is testing the second parameter (first is self)
//     const test_reg = @as(*ir.InstructionListNode, @fieldParentPtr("node", method_scope.insns.first.?.next.?.next.?.next.?.next.?)).data.jumpunless.in;
//     try std.testing.expectEqual(1, test_reg.data.local.id);
// }
//
// test "popped if body" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(x); x ? 7 : 8; x; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.jumpunless,
//         ir.Instruction.jump,
//         ir.Instruction.putlabel,
//         ir.Instruction.putlabel,
//         ir.Instruction.leave,
//     }, method_scope.insns);
// }
//
// test "simple function" {
//     const mem = std.testing.allocator;
//
//     const globals = try Globals.init(mem);
//     defer globals.deinit(mem);
//
//     const scope = try compileString(mem, globals, "def foo(x); x; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(mem);
//     defer scopes.deinit(mem);
//
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.leave,
//     }, method_scope.insns);
// }
//
// test "while loop" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo(x); while x; puts x; end; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.putlabel,
//         ir.Instruction.jumpunless,
//         ir.Instruction.setparam,
//         ir.Instruction.setparam,
//         ir.Instruction.call,
//         ir.Instruction.mov,
//         ir.Instruction.jump,
//         ir.Instruction.putlabel,
//         ir.Instruction.loadnil,
//         ir.Instruction.leave,
//     }, method_scope.insns);
// }
//
// test "empty while loop" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "def foo; while true; end; end");
//     defer scope.deinit();
//
//     var scopes = try scope.childScopes(allocator);
//     defer scopes.deinit(allocator);
//     const method_scope = scopes.items[0];
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.putlabel,
//         ir.Instruction.jump,
//         ir.Instruction.putlabel,
//         ir.Instruction.loadnil,
//         ir.Instruction.leave,
//     }, method_scope.insns);
// }
//
// test "+=" {
//     const allocator = std.testing.allocator;
//
//     const globals = try Globals.init(allocator);
//     defer globals.deinit(allocator);
//
//     const scope = try compileString(allocator, globals, "x = 1; x += 1");
//     defer scope.deinit();
//
//     try expectInstructionList(&[_]ir.InstructionName{
//         ir.Instruction.getparam,
//         ir.Instruction.mov,
//         ir.Instruction.loadi,
//         ir.Instruction.loadi,
//         ir.Instruction.setparam,
//         ir.Instruction.setparam,
//         ir.Instruction.call,
//         ir.Instruction.mov,
//         ir.Instruction.leave,
//     }, scope.insns);
// }
