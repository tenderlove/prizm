const std = @import("std");
const ir = @import("ir.zig");

const IRPrinter = struct {
    fn printOperand(op: ir.Operand, idx: usize, nitems: usize) void {
        switch(op) {
            .immediate => |p| std.debug.print("{d}", .{ p.value }),
            .string => |p| std.debug.print("{s}", .{ p.value }),
            inline else => |payload| std.debug.print("{s}{d}", .{ op.shortName(), payload.name }),
        }

        if (nitems > 0 and idx != (nitems - 1)) {
            std.debug.print(", ", .{});
        }

        return;
    }

    fn printInsnParams(insn: ir.Instruction) void {
        std.debug.print("(", .{});
        insn.eachOperand(printOperand);
        std.debug.print(")", .{});
    }

    fn printInsnName(insn: ir.Instruction) void {
        comptime var maxlen: usize = 0;
        comptime for (@typeInfo(ir.InstructionName).@"enum".fields) |field| {
            const name_len = field.name.len;
            if (name_len > maxlen) {
                maxlen = name_len;
            }
        };
        std.debug.print("{[value]s: <[width]}", .{ .value = @tagName(insn), .width = maxlen + 1, });
    }

    fn countDigits(num: usize) u32 {
        if ((num / 10) == 0) {
            return 1;
        } else {
            return 1 + countDigits(num / 10);
        }
    }

    pub fn printIR(insns: ir.InstructionList, maxname: usize) void {
        var node = insns.first;
        const digits = countDigits(maxname);

        while (node) |unwrapped| {
            switch(unwrapped.data) {
                .label => |insn| {
                    std.debug.print("{s}{d}:\n", .{
                        insn.name.shortName(),
                        insn.name.label.name
                    });
                },
                else => {
                    if (unwrapped.data.outVar()) |n| {
                        std.debug.print("  {s}", .{ n.shortName()});
                        std.debug.print("{[value]d: <[width]}<- ", .{
                            .value = n.number(),
                            .width = digits + 1,
                        });
                    } else {
                        std.debug.print("   {[value]s: <[width]}   ", .{ .value = "", .width = digits + 1, });
                    }

                    printInsnName(unwrapped.data);
                    printInsnParams(unwrapped.data);
                    std.debug.print("\n", .{ });
                }
            }

            node = unwrapped.next;
        }
    }
};

pub fn printIR(insns: ir.InstructionList, maxname: usize) void {
    IRPrinter.printIR(insns, maxname);
}
