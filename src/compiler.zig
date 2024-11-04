const std = @import("std");
const prism = @import("root.zig");

const c = @cImport({
    @cInclude("prism.h");
});

const Register = struct {
    number: u32,
};

const Instruction = struct {
    const Name = enum {
        move,
        loadi,
        add
    };

    insn: Name,
    out: Register,
    in1: Register,
    in2: Register,
};

const InstructionList = std.DoublyLinkedList(Instruction);

const Scope = struct {
    reg_number: u32,
    scope_node: *const prism.pm_scope_node_t,
    insns: InstructionList,
};

const ScopeList = std.SinglyLinkedList(Scope);

pub const Compiler = struct {
    parser: [*c]const c.pm_parser_t,
    scopes: ScopeList,
    allocator: std.mem.Allocator,

    pub fn compile(cc: *Compiler, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!Register {
        return switch (node.*.type) {
            c.PM_CALL_NODE => try cc.compileCallNode(@ptrCast(node)),
            c.PM_INTEGER_NODE => try cc.compileIntegerNode(@ptrCast(node)),
            c.PM_SCOPE_NODE => try cc.compileScopeNode(@constCast(@ptrCast(node))),
            c.PM_STATEMENTS_NODE => try cc.compileStatementsNode(@ptrCast(node)),
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            }
        };
    }

    fn newRegister(cc: *Compiler) !Register {
        const reg = cc.scopes.first.?.data.reg_number;
        cc.scopes.first.?.data.reg_number += 1;
        return Register { .number = reg, };
    }

    fn compileCallNode(cc: *Compiler, node: *const c.pm_call_node_t) !Register {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, node.*.name);

        const method_name = constant.*.start[0..(constant.*.length)];
        std.debug.print("method name {s}\n", .{method_name});
        if (node.*.receiver == null) {
            return error.NotImplementedError;
        }
        else {
            const recv_op = try cc.compile(node.*.receiver);
            return recv_op;
            //_ = recv_op;
        }
    }

    fn compileScopeNode(cc: *Compiler, node: *prism.pm_scope_node_t) !Register {
        if (node.*.parameters != null) {
            return error.NotImplementedError;
        }

        const scope = try cc.allocator.create(ScopeList.Node);
        scope.* = ScopeList.Node {
            .data = .{
                .reg_number = 0,
                .scope_node = node,
                .insns = InstructionList { },
            }
        };
        cc.scopes.prepend(scope);

        return try cc.compile(node.*.body);
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !Register {
        const outreg = cc.newRegister();
        _ = node;
        return outreg;
    }

    fn compileStatementsNode(cc: *Compiler, node: [*c]const c.pm_statements_node_t) !Register {
        const body = &node.*.body;
        if (body.*.size > 0) {
            const list = body.*.nodes[0..body.*.size - 1];
            for (list, 0..body.*.size - 1) |item, i| {
                _ = item;
                _ = i;
                return error.NotImplementedError;
                //cc.compile(
            }
            std.debug.print("len {d}\n", .{body.*.size});
            return try cc.compile(body.*.nodes[body.*.size - 1]);
        } else {
            return error.NotImplementedError;
        }
    }

    pub fn deinit(self: *Compiler, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

// pub const Instructions = struct {
//     fn init(allocator: std.mem.Allocator) !*Instructions {
//         const insns = try allocator.create(Instructions);
//         return insns;
//     }
// };

pub fn init(allocator: std.mem.Allocator, parser: [*c]const c.pm_parser_t) !*Compiler {
    const cc = try allocator.create(Compiler);
    cc.* = Compiler {
        .parser = parser,
        .allocator = allocator,
        .scopes = ScopeList { },
    };
    return cc;
}
