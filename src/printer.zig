const std = @import("std");
const ir = @import("ir.zig");
const cmp = @import("compiler.zig");
const CFG = @import("cfg.zig");
const bm = @import("utils/bitmap.zig");

const IRPrinter = struct {
    fn printOpnd(op: *const ir.Operand, out: *const std.io.AnyWriter) !void {
        switch (op.*) {
            .immediate => |p| try out.print("{d}", .{p.value}),
            .string => |p| try out.print("{s}", .{p.value}),
            .scope => |payload| try out.print("{s}{d}", .{ op.shortName(), payload.value.name }),
            inline else => |payload| try out.print("{s}{d}", .{ op.shortName(), payload.name }),
        }
    }

    fn printInsnParams(insn: ir.Instruction, out: *const std.io.AnyWriter) !void {
        try out.print("(", .{});
        var opiter = insn.opIter();
        var first = true;

        while (opiter.next()) |op| {
            if (!first) {
                try out.print(", ", .{});
            }
            first = false;
            try printOpnd(op, out);
        }
        try out.print(")", .{});
    }

    fn printInsnName(insn: ir.Instruction, out: *const std.io.AnyWriter) !void {
        comptime var maxlen: usize = 0;
        comptime for (@typeInfo(ir.InstructionName).@"enum".fields) |field| {
            const name_len = field.name.len;
            if (name_len > maxlen) {
                maxlen = name_len;
            }
        };
        try out.print("{[value]s: <[width]}", .{
            .value = @tagName(insn),
            .width = maxlen + 1,
        });
    }

    fn printInsn(insn: ir.Instruction, digits: u32, out: *const std.io.AnyWriter) ! void {
        if (insn.outVar()) |n| {
            try out.print("  {s}", .{n.shortName()});
            try out.print("{[value]d: <[width]}<- ", .{
                .value = n.number(),
                .width = digits + 1,
            });
        } else {
            try out.print("   {[value]s: <[width]}   ", .{
                .value = "",
                .width = digits + 1,
            });
        }

        try printInsnName(insn, out);
        try printInsnParams(insn, out);
    }

    pub fn printIR(alloc: std.mem.Allocator, scope: *cmp.Scope, out: *const std.io.AnyWriter) !void {
        var work = std.ArrayList(*cmp.Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);

        try out.print("t*: temporary variables\n", .{});
        try out.print("l*: local variables\n", .{});
        try out.print("L*: label\n", .{});
        try out.print("p*: parameter\n", .{});
        try out.print("\n", .{});

        while (work.popOrNull()) |work_scope| {
            var node = work_scope.insns.first;
            try out.print("= Scope: {d} ======================\n", .{work_scope.name});

            const digits = countDigits(work_scope.maxId());

            while (node) |unwrapped| {
                switch (unwrapped.data) {
                    .putlabel => |insn| {
                        try out.print("{s}{d}:\n", .{ insn.name.shortName(), insn.name.label.name });
                    },
                    .define_method => |insn| {
                        try work.append(insn.func.scope.value);
                        try printInsn(unwrapped.data, digits, out);
                        try out.print("\n", .{});
                    },
                    else => {
                        try printInsn(unwrapped.data, digits, out);
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
        work: *std.ArrayList(*cmp.Scope),
        scope: *cmp.Scope,
        var_width: u32,
    };

    fn printSet(comptime name: []const u8, scope: *cmp.Scope, set: *bm.BitMap, out: *const std.io.AnyWriter) !void {
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

    fn printBlock(scope: *cmp.Scope, blk: *CFG.BasicBlock, ctx: *const Context) !void {
        try ctx.out.print("A{d}B{d} [\n", .{ ctx.scope.name, blk.block.name });
        if (blk.block.entry) {
            try ctx.out.print("color=green\n", .{});
        }
        try ctx.out.print("label=\"BB{d}\\l", .{ blk.block.name });

        // Print upward exposed variables
        try printSet("UE: ", scope, blk.block.upward_exposed_set, ctx.out);

        // Print killed variables
        try printSet("Killed: ", scope, blk.block.killed_set, ctx.out);

        // Print live out variables
        try printSet("LiveOut: ", scope, blk.block.liveout_set, ctx.out);

        // Print uninitialized variables
        if (blk.block.entry) {
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
            try IRPrinter.printInsn(insn.data, ctx.var_width, ctx.out);
            try ctx.out.print("\\l", .{});
        }

        try ctx.out.print("\"];\n", .{ });

        if (blk.block.out) |left| {
            try ctx.out.print("A{d}B{d} -> A{d}B{d} [label=\"out1\"];\n", .{
                ctx.scope.name,
                blk.block.name,
                ctx.scope.name,
                left.block.name
            });
        }

        if (blk.block.out2) |right| {
            try ctx.out.print("A{d}B{d} -> A{d}B{d} [label=\"out1\"];\n", .{
                ctx.scope.name,
                blk.block.name,
                ctx.scope.name,
                right.block.name
            });
        }

    }

    pub fn printCFG(alloc: std.mem.Allocator, scope: *cmp.Scope, out: *const std.io.AnyWriter) !void {
        var work = std.ArrayList(*cmp.Scope).init(alloc);
        defer work.deinit();

        var ctx = Context {
            .out = out,
            .work = &work,
            .scope = scope,
            .var_width = countDigits(scope.maxId()),
        };

        try work.append(scope);

        try out.print("digraph {{\n", .{});
        try out.print("  rankdir=TD; ordering=out\n", .{});
        try out.print("  node[shape=box fontname=\"Comic Code\"];\n", .{});
        try out.print("  edge[fontname=\"Comic Code\"];\n", .{});
        try out.print("\n\n", .{});
        while (work.popOrNull()) |work_scope| {
            ctx.scope = work_scope;
            ctx.var_width = countDigits(work_scope.maxId());
            try out.print("subgraph cluster_{d} {{\n", .{ work_scope.name });
            try out.print("color=lightgrey;\n", .{});
            const cfg = try CFG.buildCFG(alloc, work_scope);
            var iter = try cfg.depthFirstIterator();
            defer iter.deinit();
            while (try iter.next()) |bb| {
                try printBlock(work_scope, bb, &ctx);
            }
            try out.print("}}\n", .{});
        }
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


pub fn printIR(alloc: std.mem.Allocator, scope: *cmp.Scope, out: std.io.AnyWriter) !void {
    try IRPrinter.printIR(alloc, scope, &out);
}

pub fn printCFG(alloc: std.mem.Allocator, scope: *cmp.Scope, out: std.io.AnyWriter) !void {
    try CFGPrinter.printCFG(alloc, scope, &out);
}
