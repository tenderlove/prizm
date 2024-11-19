const std = @import("std");
const ir = @import("ir.zig");
const cmp = @import("compiler.zig");

const IRPrinter = struct {
    fn printOperand(op: ir.Operand, idx: usize, nitems: usize, out: anytype) void {
        if (idx == 0) {
            out.print("(", .{});
        }

        switch(op) {
            .immediate => |p| out.print("{d}", .{ p.value }),
            .string => |p| out.print("{s}", .{ p.value }),
            .scope => |_| out.print("$", .{ }),
            inline else => |payload| out.print("{s}{d}", .{ op.shortName(), payload.name }),
        }

        if (nitems > 0 and idx != (nitems - 1)) {
            out.print(", ", .{});
        }

        if (idx == (nitems - 1)) {
            out.print(")", .{});
        }

        return;
    }

    fn printInsnParams(insn: ir.Instruction, out: anytype) void {
        insn.eachOperand(printOperand, out);
    }

    fn printInsnName(insn: ir.Instruction, out: anytype) void {
        comptime var maxlen: usize = 0;
        comptime for (@typeInfo(ir.InstructionName).@"enum".fields) |field| {
            const name_len = field.name.len;
            if (name_len > maxlen) {
                maxlen = name_len;
            }
        };
        out.print("{[value]s: <[width]}", .{ .value = @tagName(insn), .width = maxlen + 1, });
    }

    fn countDigits(num: usize) u32 {
        if ((num / 10) == 0) {
            return 1;
        } else {
            return 1 + countDigits(num / 10);
        }
    }

    fn printInsn(insn: ir.Instruction, digits: u32, out: anytype) void {
        if (insn.outVar()) |n| {
            out.print("  {s}", .{ n.shortName()});
            out.print("{[value]d: <[width]}<- ", .{
                .value = n.number(),
                .width = digits + 1,
            });
        } else {
            out.print("   {[value]s: <[width]}   ", .{ .value = "", .width = digits + 1, });
        }

        printInsnName(insn, out);
        printInsnParams(insn, out);
        out.print("\n", .{ });
    }

    pub fn printIR(alloc: std.mem.Allocator, scope: *cmp.Scope, out: anytype) !void {
        var work = std.ArrayList(*cmp.Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);

        while (work.popOrNull()) |work_scope| {
            var node = work_scope.insns.first;
            out.print("t*: temporary variables\n", .{});
            out.print("l*: local variables\n", .{});
            out.print("L*: label\n", .{});
            out.print("=======================\n", .{});

            const digits = countDigits(work_scope.tmpname);

            while (node) |unwrapped| {
                switch(unwrapped.data) {
                    .label => |insn| {
                        out.print("{s}{d}:\n", .{
                            insn.name.shortName(),
                            insn.name.label.name
                        });
                    },
                    .define_method => |insn| {
                        try work.append(insn.func.scope.value);
                        printInsn(unwrapped.data, digits, out);
                    },
                    else => {
                        printInsn(unwrapped.data, digits, out);
                    }
                }

                node = unwrapped.next;
            }
        }
    }
};

pub fn printIR(alloc: std.mem.Allocator, scope: *cmp.Scope, out: anytype) !void {
    try IRPrinter.printIR(alloc, scope, out);
}
