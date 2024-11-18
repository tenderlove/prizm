const std = @import("std");
const ir = @import("ir.zig");

const IRPrinter = struct {
    fn printOperand(op: ir.Operand, idx: usize, nitems: usize, out: anytype) void {
        if (idx == 0) {
            out.print("(", .{});
        }

        switch(op) {
            .immediate => |p| out.print("{d}", .{ p.value }),
            .string => |p| out.print("{s}", .{ p.value }),
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

    pub fn printIR(insns: ir.InstructionList, maxname: usize, out: anytype) void {
        var node = insns.first;
        out.print("t*: temporary variables\n", .{});
        out.print("l*: local variables\n", .{});
        out.print("L*: label\n", .{});
        out.print("=======================\n", .{});

        const digits = countDigits(maxname);

        while (node) |unwrapped| {
            switch(unwrapped.data) {
                .label => |insn| {
                    out.print("{s}{d}:\n", .{
                        insn.name.shortName(),
                        insn.name.label.name
                    });
                },
                else => {
                    if (unwrapped.data.outVar()) |n| {
                        out.print("  {s}", .{ n.shortName()});
                        out.print("{[value]d: <[width]}<- ", .{
                            .value = n.number(),
                            .width = digits + 1,
                        });
                    } else {
                        out.print("   {[value]s: <[width]}   ", .{ .value = "", .width = digits + 1, });
                    }

                    printInsnName(unwrapped.data, out);
                    printInsnParams(unwrapped.data, out);
                    out.print("\n", .{ });
                }
            }

            node = unwrapped.next;
        }
    }
};

pub fn printIR(insns: ir.InstructionList, maxname: usize, out: anytype) void {
    IRPrinter.printIR(insns, maxname, out);
}
