const std = @import("std");
const Io = std.Io;
const ir = @import("ir.zig");
const cmp = @import("compiler.zig");
const cfg_z = @import("cfg.zig");
const CFG = cfg_z.CFG;
const BasicBlock = cfg_z.BasicBlock;
const Scope = @import("scope.zig").Scope;
const BitMap = std.DynamicBitSetUnmanaged;
const InterferenceGraph = @import("interference_graph.zig").InterferenceGraph;
const assert = @import("std").debug.assert;

pub const Output = struct {
    writer: std.fs.File.Writer,

    fn print(self: *Output, comptime format: []const u8, args: anytype) !void {
        const wr = &self.writer.interface;
        try wr.print(format, args);
    }
};

const DotOutputStrategy = struct {
    writer: *Output,

    pub fn init(writer: *Output) DotOutputStrategy {
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

    pub fn printSet(self: *const DotOutputStrategy, name: []const u8, string: []const u8) !void {
        try self.print("{s}: {s}\\l", .{ name, string });
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
    writer: *Output,
    table_started: bool = false,

    pub fn init(writer: *Output) AsciiOutputStrategy {
        return AsciiOutputStrategy{ .writer = writer, .table_started = false };
    }

    pub fn printBlockHeader(self: *const AsciiOutputStrategy, _: *Scope, blk: *BasicBlock) !void {
        try self.print("### BB{d}:\n\n", .{blk.name});
        try self.print("| Property | Value |\n", .{});
        try self.print("|----------|-------|\n", .{});
    }

    pub fn printClusterHeader(self: *const AsciiOutputStrategy, work_scope: *Scope) !void {
        try self.print("## SCOPE ({d}): {s}\n", .{ work_scope.id, work_scope.getName() });
    }

    pub fn printClusterFooter(_: *const AsciiOutputStrategy, _: *Scope) !void {}

    pub fn printSet(self: *const @This(), name: []const u8, string: []const u8) !void {
        // Remove the colon and space from the name for table display
        try self.print("| {s} | {s} |\n", .{ name, string });
    }

    pub fn printListItem(self: *const @This(), comptime format: []const u8, args: anytype) !void {
        // For the table format, we'll handle common cases explicitly
        if (std.mem.eql(u8, format, "IDOM: BB{d}")) {
            try self.print("| IDOM | BB{d} |\n", args);
        } else {
            // Fallback for other formats - just put the whole thing in the value column
            try self.print("| Info | ", .{});
            try self.print(format, args);
            try self.print(" |\n", .{});
        }
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
        try self.print("#### Instructions:\n```\n", .{});
    }

    pub fn printInsnFooter(self: *const @This()) !void {
        try self.print("```\n", .{});
    }

    pub fn print(self: *const AsciiOutputStrategy, comptime format: []const u8, args: anytype) !void {
        try self.writer.print(format, args);
    }
};

pub const IRPrinter = struct {
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

    fn formatOpnd(alloc: std.mem.Allocator, op: *const ir.Variable) ![]u8 {
        var result = std.ArrayList(u8).init(alloc);
        defer result.deinit();

        switch (op.data) {
            .local => |p| try result.appendSlice(p.source_name),
            .redef => |r| {
                const orig_str = try formatOpnd(alloc, r.orig);
                defer alloc.free(orig_str);
                try result.appendSlice(orig_str);
                const subscript = try formatIntAsSubscript(alloc, r.variant);
                defer alloc.free(subscript);
                try result.appendSlice(subscript);
            },
            .prime => |r| {
                const orig_str = try formatOpnd(alloc, r.orig.data.redef.orig);
                defer alloc.free(orig_str);
                try result.appendSlice(orig_str);
                try result.appendSlice("′");
                const subscript = try formatIntAsSubscript(alloc, r.orig.data.redef.variant);
                defer alloc.free(subscript);
                try result.appendSlice(subscript);
            },
            .temp => |t| {
                const temp_str = try std.fmt.allocPrint(alloc, "t{d}", .{t.id});
                defer alloc.free(temp_str);
                try result.appendSlice(temp_str);
            },
            .live_range => |t| {
                const temp_str = try std.fmt.allocPrint(alloc, "LR{d}", .{t.id});
                defer alloc.free(temp_str);
                try result.appendSlice(temp_str);
            },
            .physical_register => |t| {
                const temp_str = try std.fmt.allocPrint(alloc, "R{d}", .{t.id});
                defer alloc.free(temp_str);
                try result.appendSlice(temp_str);
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

    pub fn printOpnd(op: *const ir.Variable, out: anytype) !void {
        switch (op.data) {
            .local => |p| try out.print("{s}", .{p.source_name}),
            .redef => |r| {
                try printOpnd(r.orig, out);
                try printIntAsSubscript(r.variant, out);
            },
            .prime => |r| {
                try printOpnd(r.orig.data.redef.orig, out);
                try out.print("′", .{});
                try printIntAsSubscript(r.orig.data.redef.variant, out);
            },
            .temp => |t| {
                try out.print("t{d}", .{t.id});
            },
            .live_range => |t| {
                try out.print("LR{d}", .{t.id});
            },
            .physical_register => |t| {
                try out.print("R{d}", .{t.id});
            },
        }
    }

    fn printInsnParams(insn: ir.Instruction, out: anytype) !void {
        switch (insn) {
            .define_method => |i| {
                try out.print("({s})", .{i.name});
            },
            .getparam => |i| try out.print("({d})", .{i.index}),
            .setparam => |i| {
                try out.print("({d}, ", .{i.index});
                try printOpnd(i.in, out);
                try out.print(")", .{});
            },
            .putlabel => |i| try out.print("L{d}", .{i.name.id}),
            .loadi => |i| try out.print("({d})", .{i.val}),
            .jump => |i| try out.print("(L{d})", .{i.label.id}),
            .jumpif => |i| {
                try out.print("(", .{});
                try printOpnd(i.in, out);
                try out.print(", L{d})", .{i.label.id});
            },
            .jumpunless => |i| {
                try out.print("(", .{});
                try printOpnd(i.in, out);
                try out.print(", L{d})", .{i.label.id});
            },
            else => {
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
            },
        }
    }

    fn printInsnName(insn: ir.Instruction, maxlen: usize, out: anytype) !void {
        try out.print("{[value]s: <[width]}", .{
            .value = @tagName(insn),
            .width = maxlen + 1,
        });
    }

    fn printInsn(insn: ir.Instruction, digits: usize, insn_name_width: usize, out: anytype) !void {
        switch (insn) {
            .putlabel => |i| try out.print("L{d}:", .{i.name.id}),
            else => {
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
            },
        }
    }

    pub fn printIR(alloc: std.mem.Allocator, scope: *Scope, out: anytype) !void {
        var work = std.ArrayList(*Scope).init(alloc);
        defer work.deinit();
        try work.append(scope);

        try out.print("t*: temporary variables\n", .{});
        try out.print("v*: named variables\n", .{});
        try out.print("L*: label\n", .{});
        try out.print("\n", .{});

        while (work.pop()) |work_scope| {
            var node = work_scope.insns.first;
            try out.print("= Scope: {d} ======================\n", .{work_scope.id});

            const digits = widestOutOp(work_scope) + 1;
            var widest_insn: usize = 0;
            var n = work_scope.insns.first;
            while (n) |insn| {
                const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);

                if (@tagName(insn_node.data).len > widest_insn) {
                    widest_insn = @tagName(insn_node.data).len;
                }
                n = insn.next;
            }

            while (node) |unwrapped| {
                const unwrapped_node: *ir.InstructionListNode = @fieldParentPtr("node", unwrapped);

                switch (unwrapped_node.data) {
                    .putlabel => |insn| {
                        try out.print("L{d}:\n", .{insn.name.id});
                    },
                    .define_method => |insn| {
                        try work.append(insn.func);
                        try printInsn(unwrapped_node.data, digits, widest_insn + 1, out);
                        try out.print("\n", .{});
                    },
                    else => {
                        try printInsn(unwrapped_node.data, digits, widest_insn + 1, out);
                        try out.print("\n", .{});
                    },
                }

                node = unwrapped.next;
            }

            try out.print("\n", .{});
        }
    }
};

const InterferenceGraphPrinter = struct {
    pub fn printDOT(allocator: std.mem.Allocator, cfg: *CFG, graph: *const InterferenceGraph, writer: std.io.AnyWriter) !void {
        const node_count = graph.size();

        try writer.print("graph InterferenceGraph {{\n", .{});
        try writer.print("  rankdir=TB;\n", .{});
        try writer.print("  node [shape=circle];\n", .{});
        try writer.print("\n", .{});

        // Create lookup table for live range variables (already sorted by ID)
        var lr_lookup = try std.ArrayList(*const ir.Variable).initCapacity(allocator, node_count);
        defer lr_lookup.deinit();

        // Populate lookup table - live ranges are already sorted by ID
        for (cfg.scope.variables.items) |var_ptr| {
            if (var_ptr.data == .live_range) {
                try lr_lookup.append(var_ptr);
            }
        }

        // Print all nodes with their variable contents
        for (0..node_count) |i| {
            assert(i < lr_lookup.items.len);
            const lr = lr_lookup.items[i];

            // Build a label showing the variables in this live range
            var label = std.ArrayList(u8).init(allocator);
            defer label.deinit();

            try label.appendSlice("LR");
            try label.writer().print("{d}\\n", .{i});

            // Add variables in this live range
            var var_iter = lr.data.live_range.variables.iterator(.{});
            var first = true;
            while (var_iter.next()) |var_id| {
                if (!first) {
                    try label.appendSlice(", ");
                }
                first = false;

                const var_ptr = cfg.scope.getVariableById(var_id);
                const var_name = try IRPrinter.formatOpnd(allocator, var_ptr);
                defer allocator.free(var_name);
                try label.appendSlice(var_name);
            }

            try writer.print("  LR{d} [label=\"{s}", .{ i, label.items });

            switch (lr.data.live_range.constraint) {
                .specific_register => |sr| {
                    try writer.print(" wants: {d}", .{sr});
                },
                else => {},
            }
            try writer.print("\"];\n", .{});
        }
        try writer.print("\n", .{});

        // Print edges (interference relationships)
        for (0..node_count) |i| {
            for (i + 1..node_count) |j| {
                if (graph.interferes(i, j)) {
                    try writer.print("  LR{d} -- LR{d};\n", .{ i, j });
                }
            }
        }

        try writer.print("}}\n", .{});
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

    fn printSet(comptime name: []const u8, scope: *Scope, set: *BitMap, ctx: anytype) !void {
        const nitems = set.count(); // number of variables
        if (nitems > 0) {
            var buffer = std.ArrayList(u8).init(ctx.allocator);
            defer buffer.deinit();

            // Build the complete string in buffer
            try buffer.append('(');

            var biti = set.iterator(.{});
            var first = true;
            while (biti.next()) |opnd_id| {
                if (!first) {
                    try buffer.appendSlice(", ");
                }
                first = false;

                const op = scope.getVariableById(opnd_id);
                const opnd_str = try IRPrinter.formatOpnd(ctx.allocator, op);
                defer ctx.allocator.free(opnd_str);
                try buffer.appendSlice(opnd_str);
            }
            try buffer.append(')');
            try ctx.out.printSet(name, buffer.items);
        }
    }

    fn printBlockSet(comptime name: []const u8, set: *BitMap, ctx: anytype) !void {
        const nitems = set.count(); // number of variables
        if (nitems > 0) {
            var buffer = std.ArrayList(u8).init(ctx.allocator);
            defer buffer.deinit();

            // Build the complete string in buffer
            try buffer.append('(');

            var biti = set.iterator(.{});
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
            try ctx.out.printSet(name, buffer.items);
        }
    }

    fn printBlock(scope: *Scope, blk: *BasicBlock, ctx: anytype) !void {
        try ctx.out.printBlockHeader(scope, blk);

        // Print upward exposed variables
        try printSet("UE      ", scope, &blk.upward_exposed_set, ctx);

        // Print killed variables
        try printSet("Killed  ", scope, &blk.killed_set, ctx);

        // Print live out variables
        try printSet("LiveOut ", scope, &blk.liveout_set, ctx);

        // Print live in variables
        try printSet("LiveIn  ", scope, &blk.livein_set, ctx);

        try printBlockSet("DOM     ", &blk.dom, ctx);
        try printBlockSet("DF      ", &blk.df, ctx);

        if (blk.idom) |idom| {
            try ctx.out.printListItem("IDOM: BB{d}", .{idom});
        }

        // Print uninitialized variables
        if (blk.entry) {
            var uninitialized = try blk.uninitializedSet(scope.allocator);
            defer uninitialized.deinit(scope.allocator);
            if (uninitialized.count() > 0) {
                try printSet("Uninitialized: ", scope, &uninitialized, ctx);
            }
        }

        try ctx.out.printLineBreak();
        try ctx.out.printInsnHeader();

        var iter = blk.instructionIter(.{});
        while (iter.next()) |insn| {
            if (ir.InstructionName.define_method == @as(ir.InstructionName, insn.data)) {
                try ctx.work.append(insn.data.define_method.func);
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
                var iter = bb.instructionIter(.{});
                while (iter.next()) |insn| {
                    if (@tagName(insn.data).len > widest_insn) {
                        widest_insn = @tagName(insn.data).len;
                    }
                }
            }

            try out.printClusterHeader(work_scope);
            try cfg.compileUntil(steps);
            // std.debug.print("opnd count {d}\n", .{ work_scope.opndCount() });
            try cfg.analyze();
            // try cfg.placePhis();
            // try cfg.rename();
            // std.debug.print("opnd count {d}\n", .{ work_scope.opndCount() });
            //std.debug.print("live count {d}\n", .{ cfg.liveOpndCount() });
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

            // Add mermaid diagram for ASCII output only
            if (@TypeOf(out.*) == AsciiOutputStrategy) {
                try printMermaidDiagram(cfg, out);
            }
        }
    }

    fn printMermaidDiagram(cfg: *CFG, out: anytype) !void {
        try out.print("\n## Control Flow Graph\n\n", .{});
        try out.print("```mermaid\n", .{});
        try out.print("flowchart TD\n", .{});

        // Print all reachable blocks
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            // Create node with block name
            try out.print("    BB{d}[\"BB{d}\"]\n", .{ bb.name, bb.name });
        }

        // Print connections between blocks
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            // Fall through connection
            if (bb.fall_through_dest) |dest| {
                try out.print("    BB{d} -->|\"fall through\"| BB{d}\n", .{ bb.name, dest.name });
            }

            // Jump connection
            if (bb.jump_dest) |dest| {
                try out.print("    BB{d} -->|\"jump\"| BB{d}\n", .{ bb.name, dest.name });
            }
        }

        try out.print("```\n", .{});
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

fn outVarWidth(opnd: *ir.Variable) usize {
    return switch (opnd.data) {
        .local => |l| l.source_name.len,
        .redef => |r| outVarWidth(r.orig) + countDigits(r.variant),
        .prime => |p| outVarWidth(p.orig) + 1,
        .temp => |t| countDigits(t.id) + 1,
        .live_range => |t| countDigits(t.id) + 2,
        .physical_register => |t| countDigits(t.id) + 1,
    };
}

fn widestOutOp(scope: *Scope) usize {
    const insns = scope.insns;
    var node = insns.first;
    var widest: usize = 0;
    while (node) |insn| {
        const insnn: *ir.InstructionListNode = @fieldParentPtr("node", insn);
        if (insnn.data.outVar()) |variable| {
            const len = outVarWidth(variable);
            if (len > widest) widest = len;
        }
        node = insn.next;
    }
    return widest;
}

pub fn printIR(alloc: std.mem.Allocator, scope: *Scope, out: *Output) !void {
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

pub fn printCFGWithFormat(alloc: std.mem.Allocator, scope: *Scope, step: CFG.State, format: CFGFormat, out: *Output) !void {
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

pub fn printInterferenceGraphDOT(allocator: std.mem.Allocator, cfg: *CFG, graph: *const InterferenceGraph, out: *Output) !void {
    try InterferenceGraphPrinter.printDOT(allocator, cfg, graph, out);
}
