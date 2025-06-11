const std = @import("std");
const ir = @import("ir.zig");
const cmp = @import("compiler.zig");
const cfg_z = @import("cfg.zig");
const CFG = cfg_z.CFG;
const BasicBlock = cfg_z.BasicBlock;
const Scope = @import("scope.zig").Scope;
const bm = @import("utils/bitmap.zig");

const IRPrinter = struct {
    fn printIntAsSubscript(num: usize, out: *const std.io.AnyWriter) !void {
        if (num < 10) {
            const val: u8 = @intCast(num);
            const bytes = [3]u8{0xE2, 0x82, 0x80 + val };
            try out.print("{s}", .{ bytes, });
        } else {
            try printIntAsSubscript(num / 10, out);
            const val: u8 = @intCast(@mod(num, 10));
            const bytes = [3]u8{0xE2, 0x82, 0x80 + val};
            try out.print("{s}", .{ bytes, });
        }
    }

    fn printOpnd(op: *const ir.Operand, out: *const std.io.AnyWriter) !void {
        switch (op.*) {
            .immediate => |p| try out.print("{d}", .{p.value}),
            inline .local, .param => |p| {
                try out.print("{s}", .{ p.source_name, });
            },
            .string => |p| try out.print("{s}", .{p.value}),
            .scope => |payload| try out.print("{s}{d}", .{ op.shortName(), payload.value.id }),
            .redef => |r| {
                try printOpnd(r.orig, out);
                try printIntAsSubscript(r.variant, out);
            },
            .prime => |r| {
                try printOpnd(r.orig.redef.orig, out);
                try out.print("′", .{});
                try printIntAsSubscript(r.orig.redef.variant, out);
            },
            else => {
                try out.print("{s}{d}", .{op.shortName(), op.number()});
            },
        }
    }

    fn printInsnParams(insn: ir.Instruction, out: *const std.io.AnyWriter) !void {
        var opiter = insn.opIter();
        var first = true;

        while (opiter.next()) |op| {
            if (first) {
                try out.print("(", .{});
            } else {
                try out.print(", ", .{});
            }
            first = false;
            try printOpnd(op, out);
        }
        if (!first) {
            try out.print(")", .{});
        }
    }

    fn printInsnName(insn: ir.Instruction, maxlen: usize, out: *const std.io.AnyWriter) !void {
        try out.print("{[value]s: <[width]}", .{
            .value = @tagName(insn),
            .width = maxlen + 1,
        });
    }

    fn printInsn(insn: ir.Instruction, digits: usize, insn_name_width: usize, out: *const std.io.AnyWriter) ! void {
        if (insn.outVar()) |n| {
            const width = outVarWidth(n);
            const padding = digits - width;
            try out.print("  ", .{});
            try printOpnd(n, out);
            try out.print("{[x]s: <[width]}", .{ .x = "", .width = padding });
            if (insn.isPMov()) {
                try out.print("<-", .{ });
                try printIntAsSubscript(insn.pmov.group, out);
                try out.print(" ", .{ });
            } else {
                try out.print("<- ", .{ });
            }
        } else {
            try out.print("  ", .{});
            try out.print("{[value]s: <[width]}   ", .{
                .value = "",
                .width = digits,
            });
        }

        try printInsnName(insn, insn_name_width, out);
        try printInsnParams(insn, out);
    }

    pub fn printIR(alloc: std.mem.Allocator, scope: *Scope, out: *const std.io.AnyWriter) !void {
        var work = std.ArrayList(*Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);

        try out.print("t*: temporary variables\n", .{});
        try out.print("l*: local variables\n", .{});
        try out.print("L*: label\n", .{});
        try out.print("p*: parameter\n", .{});
        try out.print("\n", .{});

        while (work.pop()) |work_scope| {
            var node = work_scope.insns.first;
            try out.print("= Scope: {d} ======================\n", .{work_scope.id});

            const digits = widestOutOp(work_scope) + 1;
            var widest_insn: usize = 0;
            var n = work_scope.insns.first;
            while (n) |insn| {
                if (@tagName(insn.data).len > widest_insn) {
                    widest_insn = @tagName(insn.data).len;
                }
                n = insn.next;
            }

            while (node) |unwrapped| {
                switch (unwrapped.data) {
                    .putlabel => |insn| {
                        try out.print("{s}{d}:\n", .{ insn.name.shortName(), insn.name.label.name });
                    },
                    .define_method => |insn| {
                        try work.append(insn.func.scope.value);
                        try printInsn(unwrapped.data, digits, widest_insn + 1, out);
                        try out.print("\n", .{});
                    },
                    else => {
                        try printInsn(unwrapped.data, digits, widest_insn + 1, out);
                        try out.print("\n", .{});
                    },
                }

                node = unwrapped.next;
            }

            try out.print("\n", .{});
        }
    }
};

const CFGPrinter = struct {
    const Context = struct {
        out: *const std.io.AnyWriter,
        work: *std.ArrayList(*Scope),
        scope: *Scope,
        var_width: usize,
        insn_width: usize,
    };

    fn printSet(comptime name: []const u8, scope: *Scope, set: *bm.BitMap, out: *const std.io.AnyWriter) !void {
        const nitems = set.popCount(); // number of variables
        if (nitems > 0) {
            var biti = set.setBitsIterator();
            try out.print(name, .{ });
            try out.print("(", .{});

            var first = true;
            while (biti.next()) |opnd_id| {
                if (!first) {
                    try out.print(", ", .{});
                }
                first = false;

                const op = scope.operands.items[opnd_id];
                try IRPrinter.printOpnd(op, out);
            }
            try out.print(")", .{});
            try out.print("\\l", .{ });
        }
    }

    fn printBlockSet(comptime name: []const u8, set: *bm.BitMap, out: *const std.io.AnyWriter) !void {
        const nitems = set.popCount(); // number of variables
        if (nitems > 0) {
            var biti = set.setBitsIterator();
            try out.print(name, .{ });
            try out.print("(", .{});

            var first = true;
            while (biti.next()) |block_id| {
                if (!first) {
                    try out.print(", ", .{});
                }
                first = false;
                try out.print("BB{d}", .{ block_id });
            }
            try out.print(")", .{});
            try out.print("\\l", .{ });
        }
    }

    fn printBlock(scope: *Scope, blk: *BasicBlock, ctx: *const Context) !void {
        try ctx.out.print("A{d}B{d} [\n", .{ ctx.scope.id, blk.name });
        if (blk.entry) {
            try ctx.out.print("color=green\n", .{});
        }
        try ctx.out.print("label=\"BB{d}\\l", .{ blk.name });

        // Print upward exposed variables
        try printSet("UE: ", scope, blk.upward_exposed_set, ctx.out);

        // Print killed variables
        try printSet("Killed: ", scope, blk.killed_set, ctx.out);

        // Print live out variables
        try printSet("LiveOut: ", scope, blk.liveout_set, ctx.out);

        // Print live in variables
        try printSet("LiveIn: ", scope, blk.livein_set, ctx.out);

        try printBlockSet("DOM: ", blk.dom.?, ctx.out);
        try printBlockSet("DF: ", blk.df.?, ctx.out);
        if (blk.idom) |idom| {
            try ctx.out.print("IDOM: BB{d}\\l", . {idom});
        }

        // Print uninitialized variables
        if (blk.entry) {
            const uninitialized = try blk.uninitializedSet(scope, scope.allocator);
            defer scope.allocator.destroy(uninitialized);
            if (uninitialized.popCount() > 0) {
                try printSet("Uninitialized: ", scope, uninitialized, ctx.out);
            }
        }

        try ctx.out.print("\\l", .{ });

        var iter = blk.instructionIter();
        while (iter.next()) |insn| {
            if (ir.InstructionName.define_method == @as(ir.InstructionName, insn.data)) {
                try ctx.work.append(insn.data.define_method.func.scope.value);
            }
            try IRPrinter.printInsn(insn.data, ctx.var_width, ctx.insn_width, ctx.out);
            try ctx.out.print("\\l", .{});
        }

        try ctx.out.print("\"];\n", .{ });

        if (blk.fall_through_dest) |left| {
            try ctx.out.print("A{d}B{d} -> A{d}B{d} [label=\"fall through\"];\n", .{
                ctx.scope.id,
                blk.name,
                ctx.scope.id,
                left.name
            });
        }

        if (blk.jump_dest) |right| {
            try ctx.out.print("A{d}B{d} -> A{d}B{d} [label=\"jump\"];\n", .{
                ctx.scope.id,
                blk.name,
                ctx.scope.id,
                right.name
            });
        }

    }

    fn printScope(alloc: std.mem.Allocator, work: *std.ArrayList(*Scope), steps: CFG.State, out: *const std.io.AnyWriter) !void {
        while (work.pop()) |work_scope| {
            const cfg = try CFG.build(alloc, work_scope);
            defer cfg.deinit();

            var widest_insn: usize = 0;

            for (cfg.blocks) |bb| {
                if (!bb.reachable) continue;
                var iter = bb.instructionIter();
                while (iter.next()) |insn| {
                    if (@tagName(insn.data).len > widest_insn) {
                        widest_insn = @tagName(insn.data).len;
                    }
                }
            }

            try out.print("subgraph cluster_{d} {{\n", .{ work_scope.id });
            try out.print("label=\"{s}\"\n", .{ work_scope.getName() });
            try out.print("color=lightgrey;\n", .{});
            try cfg.compileUntil(steps);
            const ctx = Context {
                .out = out,
                .work = work,
                .scope = work_scope,
                .var_width = widestOutOp(work_scope),
                .insn_width = widest_insn + 1,
            };

            var iter = try cfg.depthFirstIterator();
            defer iter.deinit();
            while (try iter.next()) |bb| {
                try printBlock(work_scope, bb, &ctx);
            }
            try out.print("}}\n", .{});
        }
    }

    pub fn printAsciiCFG(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, out: *const std.io.AnyWriter) !void {
        try out.print("#####\n", .{ });
        try out.print("# STEP \"{s}\"\n", .{ @tagName(step) });

        var work = std.ArrayList(*Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);
        try printScope(alloc, &work, step, out);
    }

    pub fn printCFG(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, out: *const std.io.AnyWriter) !void {
        try out.print("digraph {{\n", .{});
        try out.print("  rankdir=TD; ordering=out\n", .{});
        try out.print("  fontname=\"Comic Code\";\n", .{});
        try out.print("  node[shape=box fontname=\"Comic Code\"];\n", .{});
        try out.print("  edge[fontname=\"Comic Code\"];\n", .{});
        try out.print("\n\n", .{});

        try out.print("subgraph cluster_S{d} {{\n", .{ @intFromEnum(step) });
        try out.print("label=\"{s}\"\n", .{ @tagName(step) });

        var work = std.ArrayList(*Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);

        try printScope(alloc, &work, step, out);

        try out.print("}}\n", .{});

        try out.print("\n\n", .{});
        try out.print("}}\n", .{});
    }
};

fn countDigits(num: usize) u32 {
    if ((num / 10) == 0) {
        return 1;
    } else {
        return 1 + countDigits(num / 10);
    }
}

fn outVarWidth(opnd: *ir.Operand) usize {
    return switch (opnd.*) {
        inline .local, .param => |v| v.source_name.len,
        .redef => |v| outVarWidth(v.orig) + countDigits(v.variant),
        .prime => |v| outVarWidth(v.orig) + 1,
        .string => |v| v.value.len,
        .scope => |v| countDigits(v.id) + 1,
        .immediate => |v| countDigits(v.value),
        inline else => |v| countDigits(v.name) + 1
    };
}

fn widestOutOp(scope: *Scope) usize {
    const insns = scope.insns;
    var node = insns.first;
    var widest: usize = 0;
    while (node) |insn| {
        if (insn.data.outVar()) |variable| {
            const len = outVarWidth(variable);
            if (len > widest) widest = len;
        }
        node = insn.next;
    }
    return widest;
}

pub fn printIR(alloc: std.mem.Allocator, scope: *Scope, out: std.io.AnyWriter) !void {
    try IRPrinter.printIR(alloc, scope, &out);
}

pub const CFGOptions = struct {
    destruct_ssa: ?u32 = null,
};

pub const CFGFormat = enum {
    dot,
    ascii,
};

pub fn printCFGWithFormat(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, format: CFGFormat, out: std.io.AnyWriter) !void {
    switch (format) {
        .dot => try CFGPrinter.printCFG(alloc, scope, step, &out),
        .ascii => try CFGPrinter.printAsciiCFG(alloc, scope, step, &out),
    }
}
