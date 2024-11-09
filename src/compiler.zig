const std = @import("std");
const prism = @import("root.zig");

const c = @cImport({
    @cInclude("prism.h");
});

const Register = struct {
    number: u32,
};

pub const InstructionName = enum {
    move,
    loadi,
    add,
    getself
};

pub const InstructionSequence = struct {
    insns: []u32,
};

const Instruction = union(InstructionName) {
    move: struct {
        out: Register,
        in: Register,
    },

    loadi: struct {
        out: Register,
        val: u32,
    },

    add: struct {
        out: Register,
        in1: Register,
        in2: Register,
    },

    getself: struct {
        out: Register,
    },

    fn encode(self: Instruction) u32 {
        return switch (self) {
            // | out - 13 bit | in1 - 13 bit | insn 6 bit |
            .move => |v| v.out.number << 19 | v.in.number << 6 | @intFromEnum(InstructionName.move),
            .loadi => |v| v.out.number << 19 | v.val << 6 | @intFromEnum(InstructionName.loadi),
            // | out - 8 bit | in1 - 8 bit | in2 - 8 bit | insn 6 bit |
            .add => |v| v.out.number << (6 + 8 + 8) | v.in1.number << (6 + 8) | v.in2.number << 6 | @intFromEnum(InstructionName.add),
            .getself => |v| v.out.number << 6 | @intFromEnum(InstructionName.getself),
        };
    }

    fn printNode(self: ?*InstructionList.Node) void {
        if (self == null) { return; }

        const node = self.?.data;
        switch(node) {
            .move => |v| {
                std.debug.print("move    {d} {d}\n", .{ v.out.number, v.in.number });
            },
            .loadi => |v| {
                std.debug.print("loadi   {d} {d}\n", .{ v.out.number, v.val });
            },
            .add => |v| {
                std.debug.print("add     {d} {d} {d}\n", .{ v.out.number, v.in1.number, v.in2.number });
            },
            .getself => |v| {
                std.debug.print("getself {d}\n", .{ v.out.number });
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

    pub fn encode(cc: *Compiler, insn: ?*InstructionList.Node) ![]u32 {
        var list = std.ArrayList(u32).init(cc.allocator);
        var cursor = insn;
        while (cursor) |val| {
            try list.append(val.data.encode());
            cursor = val.next;
        }
        return list.toOwnedSlice();
    }

    pub fn compile(cc: *Compiler, node: *prism.pm_scope_node_t) error{NotImplementedError, OutOfMemory}!*InstructionSequence {
        return compileScopeNode(cc, node);
    }

    pub fn compileNode(cc: *Compiler, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!Register {
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

    fn newRegister(cc: *Compiler) !Register {
        const reg = cc.scopes.first.?.data.reg_number;
        cc.scopes.first.?.data.reg_number += 1;
        return Register { .number = reg, };
    }

    fn compileRecv(cc: *Compiler, node: ?*const c.pm_node_t) !Register {
        if (node != null) {
            return try cc.compileNode(node);
        } else {
            const outreg = try cc.newRegister();
            try cc.pushInsn(Instruction { .getself = .{ .out = outreg, } });
            return outreg;
        }
    }

    fn compileCallNode(cc: *Compiler, node: *const c.pm_call_node_t) !Register {
        const constant = c.pm_constant_pool_id_to_constant(&cc.parser.*.constant_pool, node.*.name);

        const method_name = constant.*.start[0..(constant.*.length)];

        const recv_op = try cc.compileRecv(node.*.receiver);

        const arg_size = node.*.arguments.*.arguments.size;
        const args = node.*.arguments.*.arguments.nodes[0..arg_size];

        if (std.mem.eql(u8, method_name, "+") and arg_size == 1) {
            const in2 = try cc.compileNode(args[0]);
            const outreg = try cc.newRegister();

            try cc.pushInsn(Instruction {
                .add = .{
                    .out = outreg,
                    .in1 = recv_op,
                    .in2 = in2,
                }
            });

            return outreg;
        } else {
            return error.NotImplementedError;
        }
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
                .insns = InstructionList { },
            }
        };
        cc.scopes.prepend(scope);

        _ = try cc.compileNode(node.*.body);

        Instruction.printNode(scope.data.insns.first);

        const iseq = try cc.allocator.create(InstructionSequence);
        iseq.* = .{
            .insns = try cc.encode(scope.data.insns.first),
        };
        return iseq;
    }

    fn compileIntegerNode(cc: *Compiler, node: *const c.pm_integer_node_t) !Register {
        if (node.*.value.values == null) {
            const outreg = try cc.newRegister();

            try cc.pushInsn(Instruction {
                .loadi = .{
                    .out = outreg,
                    .val = node.*.value.value,
                }
            });

            return outreg;
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

    fn pushInsn(self: *Compiler, insn: Instruction) !void {
        const node = try self.allocator.create(InstructionList.Node);
        node.*.data = insn;
        self.scopes.first.?.data.insns.append(node);
    }
};

pub fn init(allocator: std.mem.Allocator, parser: [*c]const c.pm_parser_t) !*Compiler {
    const cc = try allocator.create(Compiler);
    cc.* = Compiler {
        .parser = parser,
        .allocator = allocator,
        .scopes = ScopeList { },
    };
    return cc;
}
