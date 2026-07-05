const std = @import("std");
const cfg_mod = @import("../cfg.zig");
const CFG = cfg_mod.CFG;
const BasicBlock = @import("../basic_block.zig").BasicBlock;
const ir = @import("../ir.zig");

/// Dumps a CFG in Cranelift-ish `.clif` shape:
///
///     function %name() {
///     block0:
///         v0 = iconst 5
///         v1 = call v0, %+, v0
///         return v1
///     }
///
/// Values are `v<id>` where id is the instruction's push-time id. Terminators
/// use `jump` / `brif` / `return`.
pub fn print(cfg: *CFG, w: *std.Io.Writer) !void {
    const scope = cfg.scope;
    try w.print("function %{s}() {{\n", .{scope.getName()});

    for (cfg.blocks, 0..) |bb, i| {
        if (i > 0) try w.print("\n", .{});
        try printBlock(bb, w);
    }

    try w.print("}}\n", .{});
}

fn printBlock(bb: *BasicBlock, w: *std.Io.Writer) !void {
    try w.print("block{d}:\n", .{bb.name});
    var iter = bb.instructionIter(.{});
    while (iter.next()) |insn| {
        try w.print("    ", .{});
        try printInsn(insn, w);
        try w.print("\n", .{});
    }
}

fn printOpnd(insn: *ir.InstructionListNode, w: *std.Io.Writer) !void {
    try w.print("v{d}", .{insn.id});
}

fn printInsn(insn: *ir.InstructionListNode, w: *std.Io.Writer) !void {
    switch (insn.data) {
        // Terminators — no def, just a verb.
        .jump => |i| try w.print("jump block{d}", .{i.target.name}),
        .cond => |i| {
            try w.print("brif ", .{});
            try printOpnd(i.condition, w);
            try w.print(", block{d}, block{d}", .{ i.truthy.name, i.falsy.name });
        },
        .leave => |i| {
            try w.print("return ", .{});
            try printOpnd(i.in, w);
        },

        // Value-producing ops — `vN = ...`
        .loadi => |i| {
            try printOpnd(insn, w);
            try w.print(" = iconst {d}", .{i.val});
        },
        .loadstr => |i| {
            try printOpnd(insn, w);
            try w.print(" = sconst \"{s}\"", .{i.val});
        },
        .loadnil => {
            try printOpnd(insn, w);
            try w.print(" = nil", .{});
        },
        .getparam => |i| {
            try printOpnd(insn, w);
            try w.print(" = param {d}", .{i.index});
        },
        .call => |i| {
            try printOpnd(insn, w);
            try w.print(" = call ", .{});
            try printOpnd(i.recv, w);
            try w.print(", %{s}", .{i.name});
            for (i.params.items) |p| {
                try w.print(", ", .{});
                try printOpnd(p, w);
            }
        },
        .define_method => |i| {
            try printOpnd(insn, w);
            try w.print(" = def_method %{s}", .{i.name});
        },
        .tst => |i| {
            try printOpnd(insn, w);
            try w.print(" = tst ", .{});
            try printOpnd(i.in, w);
        },
        .phi => |i| {
            try printOpnd(insn, w);
            try w.print(" = phi", .{});
            for (i.params.items, 0..) |p, idx| {
                if (idx == 0) try w.print(" ", .{}) else try w.print(", ", .{});
                try printOpnd(p, w);
            }
        },
    }
}
