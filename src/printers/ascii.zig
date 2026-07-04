const std = @import("std");
const cfg_mod = @import("../cfg.zig");
const CFG = cfg_mod.CFG;
const BasicBlock = @import("../basic_block.zig").BasicBlock;
const ir = @import("../ir.zig");

/// Dumps a CFG in Cranelift-ish `.clif` shape:
///
///     function %name() {
///     block0:
///         jump block1
///
///     block1:
///         ...
///     }
///
/// Per-instruction operand formatting is intentionally minimal right now —
/// just the opcode name — while the IR is mid-transition to the
/// instruction-as-value model. Once payloads stabilize, rebuild the fancier
/// `v0 = iconst 5` / `brif v1, block2, block3` output on top of the new
/// instruction-handle model.
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
        try w.print("    {s}\n", .{@tagName(insn.data)});
    }
}
