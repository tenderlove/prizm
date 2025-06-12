const std = @import("std");
const ir = @import("ir.zig");
const cmp = @import("compiler.zig");
const cfg_z = @import("cfg.zig");
const CFG = cfg_z.CFG;
const BasicBlock = cfg_z.BasicBlock;
const Scope = @import("scope.zig").Scope;
const bm = @import("utils/bitmap.zig");

const DotOutputStrategy = struct {
    writer: std.io.AnyWriter,

    pub fn init(writer: std.io.AnyWriter) DotOutputStrategy {
        return DotOutputStrategy{ .writer = writer };
    }

    pub fn printClusterHeader(self: *const DotOutputStrategy, work_scope: *Scope) !void {
        try self.print("subgraph cluster_{d} {{\n", .{work_scope.id});
        try self.print("label=\"{s}\"\n", .{work_scope.getName()});
        try self.print("color=lightgrey;\n", .{});
    }

    pub fn printBlockHeader(self: *const @This(), scope: *Scope, blk: *BasicBlock) !void {
        try self.print("A{d}B{d} [\n", .{ scope.id, blk.name });
        if (blk.entry) {
            try self.print("color=green\n", .{});
        }
        try self.print("label=\"BB{d}\\l", .{blk.name});
    }

    pub fn printSet(self: *const DotOutputStrategy, string: []const u8) !void {
        try self.print("{s}\\l", .{string});
    }

    pub fn printBlockSeparator(_: *const @This()) !void {}

    pub fn printClusterFooter(self: *const DotOutputStrategy, _: *Scope) !void {
        try self.print("}}\n", .{});
    }

    pub fn printListItem(self: *const DotOutputStrategy, comptime format: []const u8, args: anytype) !void {
        try self.print(format, args);
        try self.print("\\l", .{});
    }

    pub fn printBlockFooter(self: *const @This()) !void {
        try self.print("\"];\n", .{});
    }

    pub fn printFallThrough(self: *const @This(), scope: *Scope, blk: *BasicBlock, dest: *BasicBlock) !void {
        try self.print("A{d}B{d} -> A{d}B{d} [label=\"fall through\"];\n", .{ scope.id, blk.name, scope.id, dest.name });
    }

    pub fn printJump(self: *const @This(), scope: *Scope, blk: *BasicBlock, dest: *BasicBlock) !void {
        try self.print("A{d}B{d} -> A{d}B{d} [label=\"jump\"];\n", .{ scope.id, blk.name, scope.id, dest.name });
    }

    pub fn printLineBreak(self: *const @This()) !void {
        try self.print("\\l", .{});
    }

    pub fn printInsnHeader(_: *const @This()) !void {}

    pub fn printInsnFooter(_: *const @This()) !void {}

    pub fn print(self: *const DotOutputStrategy, comptime format: []const u8, args: anytype) !void {
        try self.writer.print(format, args);
    }
};

const AsciiOutputStrategy = struct {
    writer: std.io.AnyWriter,

    pub fn init(writer: std.io.AnyWriter) AsciiOutputStrategy {
        return AsciiOutputStrategy{ .writer = writer };
    }

    pub fn printBlockHeader(self: *const AsciiOutputStrategy, _: *Scope, blk: *BasicBlock) !void {
        try self.print("### BB{d}:\n", .{blk.name});
    }

    pub fn printClusterHeader(self: *const AsciiOutputStrategy, work_scope: *Scope) !void {
        try self.print("## SCOPE ({d}): {s}\n", .{ work_scope.id, work_scope.getName() });
    }

    pub fn printClusterFooter(_: *const AsciiOutputStrategy, _: *Scope) !void {}

    pub fn printSet(self: *const @This(), string: []const u8) !void {
        try self.print("* {s}\n", .{string});
    }

    pub fn printListItem(self: *const @This(), comptime format: []const u8, args: anytype) !void {
        try self.print("* ", .{});
        try self.print(format, args);
        try self.print("\n", .{});
    }

    pub fn printFallThrough(self: *const @This(), _: *Scope, _: *BasicBlock, dest: *BasicBlock) !void {
        try self.print("* Falls Through To: BB{d}\n", .{dest.name});
    }

    pub fn printJump(self: *const @This(), _: *Scope, _: *BasicBlock, dest: *BasicBlock) !void {
        try self.print("* Jumps To: BB{d}\n", .{dest.name});
    }

    pub fn printBlockFooter(self: *const @This()) !void {
        try self.print("\n", .{});
    }

    pub fn printBlockSeparator(self: *const @This()) !void {
        try self.print("\n", .{});
    }

    pub fn printLineBreak(self: *const @This()) !void {
        try self.print("\n", .{});
    }

    pub fn printInsnHeader(self: *const @This()) !void {
        try self.print("```\n", .{});
    }

    pub fn printInsnFooter(self: *const @This()) !void {
        try self.print("```\n", .{});
    }

    pub fn print(self: *const AsciiOutputStrategy, comptime format: []const u8, args: anytype) !void {
        try self.writer.print(format, args);
    }
};

const IRPrinter = struct {
    fn formatIntAsSubscript(alloc: std.mem.Allocator, num: usize) ![]u8 {
        var result = std.ArrayList(u8).init(alloc);
        defer result.deinit();

        if (num < 10) {
            const val: u8 = @intCast(num);
            const bytes = [3]u8{ 0xE2, 0x82, 0x80 + val };
            try result.appendSlice(&bytes);
        } else {
            const prefix = try formatIntAsSubscript(alloc, num / 10);
            defer alloc.free(prefix);
            try result.appendSlice(prefix);
            const val: u8 = @intCast(@mod(num, 10));
            const bytes = [3]u8{ 0xE2, 0x82, 0x80 + val };
            try result.appendSlice(&bytes);
        }

        return result.toOwnedSlice();
    }

    fn formatOpnd(alloc: std.mem.Allocator, op: *const ir.Operand) ![]u8 {
        var result = std.ArrayList(u8).init(alloc);
        defer result.deinit();

        switch (op.*) {
            .immediate => |p| try result.writer().print("{d}", .{p.value}),
            inline .local, .param => |p| {
                try result.appendSlice(p.source_name);
            },
            .string => |p| try result.appendSlice(p.value),
            .scope => |payload| try result.writer().print("{s}{d}", .{ op.shortName(), payload.value.id }),
            .redef => |r| {
                const orig_str = try formatOpnd(alloc, r.orig);
                defer alloc.free(orig_str);
                try result.appendSlice(orig_str);
                const subscript = try formatIntAsSubscript(alloc, r.variant);
                defer alloc.free(subscript);
                try result.appendSlice(subscript);
            },
            .prime => |r| {
                const orig_str = try formatOpnd(alloc, r.orig.redef.orig);
                defer alloc.free(orig_str);
                try result.appendSlice(orig_str);
                try result.appendSlice("′");
                const subscript = try formatIntAsSubscript(alloc, r.orig.redef.variant);
                defer alloc.free(subscript);
                try result.appendSlice(subscript);
            },
            else => {
                try result.writer().print("{s}{d}", .{ op.shortName(), op.number() });
            },
        }

        return result.toOwnedSlice();
    }

    fn printIntAsSubscript(num: usize, out: anytype) !void {
        if (num < 10) {
            const val: u8 = @intCast(num);
            const bytes = [3]u8{ 0xE2, 0x82, 0x80 + val };
            try out.print("{s}", .{
                bytes,
            });
        } else {
            try printIntAsSubscript(num / 10, out);
            const val: u8 = @intCast(@mod(num, 10));
            const bytes = [3]u8{ 0xE2, 0x82, 0x80 + val };
            try out.print("{s}", .{
                bytes,
            });
        }
    }

    fn printOpnd(op: *const ir.Operand, out: anytype) !void {
        switch (op.*) {
            .immediate => |p| try out.print("{d}", .{p.value}),
            inline .local, .param => |p| {
                try out.print("{s}", .{
                    p.source_name,
                });
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
                try out.print("{s}{d}", .{ op.shortName(), op.number() });
            },
        }
    }

    fn printInsnParams(insn: ir.Instruction, out: anytype) !void {
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

    fn printInsnName(insn: ir.Instruction, maxlen: usize, out: anytype) !void {
        try out.print("{[value]s: <[width]}", .{
            .value = @tagName(insn),
            .width = maxlen + 1,
        });
    }

    fn printInsn(insn: ir.Instruction, digits: usize, insn_name_width: usize, out: anytype) !void {
        if (insn.outVar()) |n| {
            const width = outVarWidth(n);
            const padding = digits - width;
            try out.print("  ", .{});
            try printOpnd(n, out);
            try out.print("{[x]s: <[width]}", .{ .x = "", .width = padding });
            if (insn.isPMov()) {
                try out.print("<-", .{});
                try printIntAsSubscript(insn.pmov.group, out);
                try out.print(" ", .{});
            } else {
                try out.print("<- ", .{});
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

    pub fn printIR(alloc: std.mem.Allocator, scope: *Scope, out: anytype) !void {
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
    fn Context(comptime OutputStrategy: type) type {
        return struct {
            out: *const OutputStrategy,
            work: *std.ArrayList(*Scope),
            scope: *Scope,
            var_width: usize,
            insn_width: usize,
            allocator: std.mem.Allocator,
        };
    }

    fn printSet(comptime name: []const u8, scope: *Scope, set: *bm.BitMap, ctx: anytype) !void {
        const nitems = set.popCount(); // number of variables
        if (nitems > 0) {
            var buffer = std.ArrayList(u8).init(ctx.allocator);
            defer buffer.deinit();

            // Build the complete string in buffer
            try buffer.appendSlice(name);
            try buffer.append('(');

            var biti = set.setBitsIterator();
            var first = true;
            while (biti.next()) |opnd_id| {
                if (!first) {
                    try buffer.appendSlice(", ");
                }
                first = false;

                const op = scope.operands.items[opnd_id];
                const opnd_str = try IRPrinter.formatOpnd(ctx.allocator, op);
                defer ctx.allocator.free(opnd_str);
                try buffer.appendSlice(opnd_str);
            }
            try buffer.append(')');
            try ctx.out.printSet(buffer.items);
        }
    }

    fn printBlockSet(comptime name: []const u8, set: *bm.BitMap, ctx: anytype) !void {
        const nitems = set.popCount(); // number of variables
        if (nitems > 0) {
            var buffer = std.ArrayList(u8).init(ctx.allocator);
            defer buffer.deinit();

            // Build the complete string in buffer
            try buffer.appendSlice(name);
            try buffer.append('(');

            var biti = set.setBitsIterator();
            var first = true;
            while (biti.next()) |block_id| {
                if (!first) {
                    try buffer.appendSlice(", ");
                }
                first = false;
                try buffer.writer().print("BB{d}", .{block_id});
            }
            try buffer.append(')');

            // Print the complete string in one go
            try ctx.out.printSet(buffer.items);
        }
    }

    fn printBlock(scope: *Scope, blk: *BasicBlock, ctx: anytype) !void {
        try ctx.out.printBlockHeader(scope, blk);

        // Print upward exposed variables
        try printSet("UE: ", scope, blk.upward_exposed_set, ctx);

        // Print killed variables
        try printSet("Killed: ", scope, blk.killed_set, ctx);

        // Print live out variables
        try printSet("LiveOut: ", scope, blk.liveout_set, ctx);

        // Print live in variables
        try printSet("LiveIn: ", scope, blk.livein_set, ctx);

        if (blk.dom) |dom| {
            try printBlockSet("DOM: ", dom, ctx);
        }
        if (blk.df) |df| {
            try printBlockSet("DF: ", df, ctx);
        }
        if (blk.idom) |idom| {
            try ctx.out.printListItem("IDOM: BB{d}", .{idom});
        }

        // Print uninitialized variables
        if (blk.entry) {
            const uninitialized = try blk.uninitializedSet(scope, scope.allocator);
            defer scope.allocator.destroy(uninitialized);
            if (uninitialized.popCount() > 0) {
                try printSet("Uninitialized: ", scope, uninitialized, ctx);
            }
        }

        try ctx.out.printLineBreak();
        try ctx.out.printInsnHeader();

        var iter = blk.instructionIter();
        while (iter.next()) |insn| {
            if (ir.InstructionName.define_method == @as(ir.InstructionName, insn.data)) {
                try ctx.work.append(insn.data.define_method.func.scope.value);
            }
            try IRPrinter.printInsn(insn.data, ctx.var_width, ctx.insn_width, ctx.out);
            try ctx.out.printLineBreak();
        }

        try ctx.out.printInsnFooter();

        try ctx.out.printBlockFooter();

        if (blk.fall_through_dest) |left| {
            try ctx.out.printFallThrough(ctx.scope, blk, left);
        }

        if (blk.jump_dest) |right| {
            try ctx.out.printJump(ctx.scope, blk, right);
        }
    }

    fn printScope(alloc: std.mem.Allocator, work: *std.ArrayList(*Scope), steps: CFG.State, out: anytype) !void {
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

            try out.printClusterHeader(work_scope);
            try cfg.compileUntil(steps);
            const ctx = Context(@TypeOf(out.*)){
                .out = out,
                .work = work,
                .scope = work_scope,
                .var_width = widestOutOp(work_scope),
                .insn_width = widest_insn + 1,
                .allocator = alloc,
            };

            var iter = try cfg.depthFirstIterator();
            defer iter.deinit();
            while (try iter.next()) |bb| {
                try printBlock(work_scope, bb, &ctx);
                try ctx.out.printBlockSeparator();
            }
            try out.printClusterFooter(work_scope);
        }
    }

    pub fn printAsciiCFG(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, out: anytype) !void {
        try out.print("#####\n", .{});
        try out.print("# STEP \"{s}\"\n", .{@tagName(step)});

        var work = std.ArrayList(*Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);
        try printScope(alloc, &work, step, out);
    }

    pub fn printCFG(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, out: anytype) !void {
        try out.print("digraph {{\n", .{});
        try out.print("  rankdir=TD; ordering=out\n", .{});
        try out.print("  fontname=\"Comic Code\";\n", .{});
        try out.print("  node[shape=box fontname=\"Comic Code\"];\n", .{});
        try out.print("  edge[fontname=\"Comic Code\"];\n", .{});
        try out.print("\n\n", .{});

        try out.print("subgraph cluster_S{d} {{\n", .{@intFromEnum(step)});
        try out.print("label=\"{s}\"\n", .{@tagName(step)});

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
        inline else => |v| countDigits(v.name) + 1,
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
    const strategy = DotOutputStrategy.init(out);
    try IRPrinter.printIR(alloc, scope, &strategy);
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
        .dot => {
            const strategy = DotOutputStrategy.init(out);
            try CFGPrinter.printCFG(alloc, scope, step, &strategy);
        },
        .ascii => {
            const strategy = AsciiOutputStrategy.init(out);
            try CFGPrinter.printAsciiCFG(alloc, scope, step, &strategy);
        },
    }
}
