const std = @import("std");
const prism = @import("root.zig");

const c = @cImport({
    @cInclude("prism.h");
});

const Register = struct {
    number: u32,
};

const InstructionName = enum {
    move,
    loadi,
    add
};

const Instruction = union(InstructionName) {
    move: struct {
        out: Register,
        in1: Register,
    },

    loadi: struct {
        out: Register,
        val: u64,
    },

    add: struct {
        out: Register,
        in1: Register,
        in2: Register,
    },

    fn printNode(self: ?*InstructionList.Node) void {
        if (self == null) { return; }

        const node = self.?.data;
        switch(node) {
            .move => |v| {
                std.debug.print("move {d} {d}\n", .{ v.out.number, v.in1.number });
            },
            .loadi => |v| {
                std.debug.print("loadi {d} {d}\n", .{ v.out.number, v.val });
            },
            .add => |v| {
                std.debug.print("add   {d} {d} {d}\n", .{ v.out.number, v.in1.number, v.in2.number });
            },
        }

        printNode(self.?.next);

    }
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
        const reg = compileNode(cc, node);
        return reg;
    }

    pub fn compileNode(cc: *Compiler, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!Register {
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

        if (node.*.receiver == null) {
            return error.NotImplementedError;
        } else {
            const recv_op = try cc.compileNode(node.*.receiver);
            const arg_size = node.*.arguments.*.arguments.size;
            const args = node.*.arguments.*.arguments.nodes[0..arg_size];

            if (std.mem.eql(u8, method_name, "+") and arg_size == 1) {
                const insn = try cc.allocator.create(InstructionList.Node);
                const in2 = try cc.compileNode(args[0]);

                insn.* = InstructionList.Node {
                    .data = Instruction {
                        .add = .{
                            .out = try cc.newRegister(),
                            .in1 = recv_op,
                            .in2 = in2,
                        }
                    }
                };

                cc.scopes.first.?.data.insns.append(insn);

                return insn.data.add.out;
            } else {
                return error.NotImplementedError;
            }
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

        const reg = try cc.compileNode(node.*.body);

        Instruction.printNode(scope.data.insns.first);

        return reg;
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !Register {
        if (node.*.value.values == null) {
            std.debug.print("integer {d}\n", .{ node.*.value.value });
            const insn = try cc.allocator.create(InstructionList.Node);

            insn.* = InstructionList.Node {
                .data = Instruction {
                    .loadi = .{
                        .out = try cc.newRegister(),
                        .val = node.*.value.value,
                    }
                }
            };

            cc.scopes.first.?.data.insns.append(insn);

            return insn.data.loadi.out;
        } else {
            return error.NotImplementedError;
        }
    }

    fn compileStatementsNode(cc: *Compiler, node: [*c]const c.pm_statements_node_t) !Register {
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
