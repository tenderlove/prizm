const std = @import("std");
const cfg_mod = @import("../cfg.zig");
const CFG = cfg_mod.CFG;
const BasicBlock = @import("../basic_block.zig").BasicBlock;
const ir = @import("../ir.zig");

// ion.json schema, version 1. Mirrors mozilla-spidermonkey/iongraph's
// src/iongraph.ts — when that schema bumps, diff against this file.
// Upstream: https://github.com/mozilla-spidermonkey/iongraph/blob/main/src/iongraph.ts
//
// Field notes not in the TS:
//   * `ptr` is a stable identity across passes — same block/instruction
//     across passes must carry the same ptr. `id` is per-pass display.
//   * `memInputs` shape is untyped upstream ("TODO"); empty slice is safe.
//   * `type` is renamed to `@"type"` since it's a Zig keyword. Zig's JSON
//     serializer emits it without the `@""` sigil.
const IonJSON = struct {
    version: u32 = 1,
    functions: []const Func,
};

const Func = struct {
    name: []const u8,
    passes: []const Pass,
};

const Pass = struct {
    name: []const u8,
    mir: struct { blocks: []const MIRBlock },
    lir: struct { blocks: []const LIRBlock } = .{ .blocks = &.{} },
};

const MIRBlock = struct {
    ptr: u64,
    id: u32,
    loopDepth: u32,
    attributes: []const []const u8,
    predecessors: []const u32,
    successors: []const u32,
    instructions: []const MIRInstruction,
};

const MIRInstruction = struct {
    ptr: u64,
    id: u32,
    opcode: []const u8,
    attributes: []const []const u8,
    inputs: []const u32,
    uses: []const u32,
    memInputs: []const struct {} = &.{},
    @"type": []const u8,
};

const LIRBlock = struct {
    ptr: u64,
    id: u32,
    instructions: []const LIRInstruction,
};

const LIRInstruction = struct {
    ptr: u64,
    id: u32,
    mirPtr: ?u64,
    opcode: []const u8,
    defs: []const u32,
};

pub const IonGraph = struct {
    const BlockSet = std.AutoHashMapUnmanaged(*BasicBlock, void);
    const DepthMap = std.AutoHashMapUnmanaged(*BasicBlock, u32);

    const LoopInfo = struct {
        // Blocks that jump back to a loop header (their successor is the header).
        backedges: BlockSet,
        // Blocks that are the target of at least one back-edge.
        headers: BlockSet,
        // Loop nesting depth for every block (0 outside any loop).
        depth: DepthMap,
    };

    // DFS from entry, marking blocks whose successor is currently on the
    // DFS stack (== a back-edge in classical graph-theory sense). Also
    // records each back-edge's target as a loop header.
    fn analyzeLoops(cfg: *CFG, a: std.mem.Allocator) !LoopInfo {
        var info: LoopInfo = .{ .backedges = .empty, .headers = .empty, .depth = .empty };
        var visited: BlockSet = .empty;
        defer visited.deinit(a);
        var on_stack: BlockSet = .empty;
        defer on_stack.deinit(a);
        try dfsBackedges(cfg.head, a, &visited, &on_stack, &info);

        // Compute per-block loop depth: for each back-edge B→H, every block
        // reachable backward from B through preds (stopping at H) is in H's
        // loop body. Increment depth for each such block (including H).
        // Nested loops layer naturally: an inner loop's body ⊆ its outer.
        var it = info.backedges.keyIterator();
        while (it.next()) |be| {
            const b = be.*;
            // Successor of a back-edge block is its header. Ours always has
            // exactly one successor, but be defensive.
            const succs = b.successors();
            const header = succs[0] orelse succs[1].?;

            var body: BlockSet = .empty;
            defer body.deinit(a);
            var stack: std.ArrayList(*BasicBlock) = .empty;
            defer stack.deinit(a);

            try body.put(a, header, {});
            try body.put(a, b, {});
            try stack.append(a, b);

            while (stack.pop()) |cur| {
                for (cur.predecessors.items) |p| {
                    if (body.contains(p)) continue;
                    try body.put(a, p, {});
                    try stack.append(a, p);
                }
            }

            var body_it = body.keyIterator();
            while (body_it.next()) |member| {
                const entry = try info.depth.getOrPut(a, member.*);
                if (!entry.found_existing) entry.value_ptr.* = 0;
                entry.value_ptr.* += 1;
            }
        }

        return info;
    }

    fn dfsBackedges(
        block: *BasicBlock,
        a: std.mem.Allocator,
        visited: *BlockSet,
        on_stack: *BlockSet,
        info: *LoopInfo,
    ) !void {
        try visited.put(a, block, {});
        try on_stack.put(a, block, {});
        for (block.successors()) |maybe| {
            const succ = maybe orelse continue;
            if (on_stack.contains(succ)) {
                try info.backedges.put(a, block, {});
                try info.headers.put(a, succ, {});
            } else if (!visited.contains(succ)) {
                try dfsBackedges(succ, a, visited, on_stack, info);
            }
        }
        _ = on_stack.remove(block);
    }

    fn buildMIRBlocks(cfg: *CFG, a: std.mem.Allocator) ![]MIRBlock {
        var info = try analyzeLoops(cfg, a);
        defer {
            info.backedges.deinit(a);
            info.headers.deinit(a);
            info.depth.deinit(a);
        }

        // IDs are assigned at push time on InstructionListNode.id. Find the
        // max id in this function so we can size the inputs/uses arrays.
        var max_id: usize = 0;
        for (cfg.blocks) |bb| {
            var it = bb.insns.first;
            while (it) |raw| {
                const node: *ir.InstructionListNode = @fieldParentPtr("node", raw);
                if (node.id > max_id) max_id = node.id;
                it = raw.next;
            }
        }
        const n_slots = max_id + 1;

        // Pass 1: per-instruction inputs. Walk operand fields per variant.
        const inputs = try a.alloc(std.ArrayList(u32), n_slots);
        for (inputs) |*x| x.* = .empty;
        const uses = try a.alloc(std.ArrayList(u32), n_slots);
        for (uses) |*x| x.* = .empty;

        for (cfg.blocks) |bb| {
            var it = bb.insns.first;
            while (it) |raw| {
                const node: *ir.InstructionListNode = @fieldParentPtr("node", raw);
                try appendOperandIds(node.data, &inputs[node.id], a);
                it = raw.next;
            }
        }

        // Pass 2: invert inputs → uses.
        for (inputs, 0..) |ins, my_id_usize| {
            for (ins.items) |input_id| {
                try uses[input_id].append(a, @intCast(my_id_usize));
            }
        }

        const out = try a.alloc(MIRBlock, cfg.blocks.len);
        for (cfg.blocks, 0..) |block, i| {
            out[i] = try buildMIRBlock(block, .{
                .is_backedge = info.backedges.contains(block),
                .is_header = info.headers.contains(block),
                .loop_depth = info.depth.get(block) orelse 0,
            }, inputs, uses, a);
        }
        return out;
    }

    fn appendOperandIds(
        insn: ir.Instruction,
        out: *std.ArrayList(u32),
        a: std.mem.Allocator,
    ) !void {
        switch (insn) {
            .call => |cc| {
                try out.append(a, @intCast(cc.recv.id));
                for (cc.params.items) |p| try out.append(a, @intCast(p.id));
            },
            .cond => |co| try out.append(a, @intCast(co.condition.id)),
            .leave => |l| try out.append(a, @intCast(l.in.id)),
            .tst => |t| try out.append(a, @intCast(t.in.id)),
            .phi => |p| for (p.params.items) |x| try out.append(a, @intCast(x.id)),
            .define_method, .getparam, .jump, .loadi, .loadstr, .loadnil => {},
        }
    }

    const BlockKind = struct {
        is_backedge: bool,
        is_header: bool,
        loop_depth: u32,
    };

    fn buildMIRBlock(
        block: *BasicBlock,
        kind: BlockKind,
        inputs: []std.ArrayList(u32),
        uses: []std.ArrayList(u32),
        a: std.mem.Allocator,
    ) !MIRBlock {
        // predecessors — map each pred's stable name to u32.
        const preds = try a.alloc(u32, block.predecessors.items.len);
        for (block.predecessors.items, 0..) |p, i| preds[i] = @intCast(p.name);

        // successors — taken/jump target first, not-taken second. iongraph
        // relies on positional order for edge labeling.
        var succ_buf: [2]u32 = undefined;
        var n_succ: usize = 0;
        for (block.successors()) |maybe| {
            const s = maybe orelse continue;
            succ_buf[n_succ] = @intCast(s.name);
            n_succ += 1;
        }
        const succs = try a.dupe(u32, succ_buf[0..n_succ]);

        // attributes — mark entry, loop headers, and back-edge blocks.
        // "loopheader" drives iongraph's per-loop tracking; "backedge"
        // stops its layering recursion at the loop's latch.
        var attr_list: std.ArrayList([]const u8) = .empty;
        if (block.entry) try attr_list.append(a, "root");
        if (kind.is_header) try attr_list.append(a, "loopheader");
        if (kind.is_backedge) try attr_list.append(a, "backedge");
        const attrs = try attr_list.toOwnedSlice(a);

        return .{
            .ptr = @intFromPtr(block),
            .id = @intCast(block.name),
            .loopDepth = kind.loop_depth,
            .attributes = attrs,
            .predecessors = preds,
            .successors = succs,
            .instructions = try buildMIRInsns(block, inputs, uses, a),
        };
    }

    fn buildMIRInsns(
        block: *BasicBlock,
        inputs: []std.ArrayList(u32),
        uses: []std.ArrayList(u32),
        a: std.mem.Allocator,
    ) ![]MIRInstruction {
        var list: std.ArrayList(MIRInstruction) = .empty;
        var iter: ?*std.DoublyLinkedList.Node = block.insns.first;
        while (iter) |raw| {
            const node: *ir.InstructionListNode = @fieldParentPtr("node", raw);
            try list.append(a, .{
                .ptr = @intFromPtr(node),
                .id = @intCast(node.id),
                .opcode = try formatOpcode(node.data, a),
                .attributes = &.{},
                .inputs = inputs[node.id].items,
                .uses = uses[node.id].items,
                .@"type" = "None",
            });
            iter = raw.next;
        }
        return list.toOwnedSlice(a);
    }

    /// Display mnemonic for an instruction — used both as the leading verb
    /// on its own row AND when other instructions reference it as an input
    /// (via `<mnemonic>#<id>`, which iongraph recognizes as a clickable
    /// use marker per Graph.ts's split on `/([A-Za-z0-9_]+)#(\d+)/`).
    fn mnemonic(insn: ir.Instruction) []const u8 {
        return switch (insn) {
            .loadi => "iconst",
            .loadstr => "sconst",
            .loadnil => "nil",
            .getparam => "param",
            .call => "call",
            .define_method => "def_method",
            .jump => "jump",
            .cond => "brif",
            .leave => "return",
            .tst => "tst",
            .phi => "phi",
        };
    }

    /// Renders `<mnemonic>#<id>` for an SSA operand reference. Matches the
    /// convention Ion's own dumps use (e.g. `Add#12` refers to the Add
    /// instruction with id 12).
    fn writeRef(w: *std.Io.Writer, insn: *ir.InstructionListNode) !void {
        try w.print("{s}#{d}", .{ mnemonic(insn.data), insn.id });
    }

    /// Renders the opcode + operand references into a display string.
    fn formatOpcode(insn: ir.Instruction, a: std.mem.Allocator) ![]const u8 {
        var buf: std.Io.Writer.Allocating = .init(a);
        const w = &buf.writer;
        try w.print("{s}", .{mnemonic(insn)});
        switch (insn) {
            .loadi => |i| try w.print(" {d}", .{i.val}),
            .loadstr => |i| try w.print(" \"{s}\"", .{i.val}),
            .loadnil => {},
            .getparam => |i| try w.print(" {d}", .{i.index}),

            .call => |i| {
                try w.print(" ", .{});
                try writeRef(w, i.recv);
                try w.print(", %{s}", .{i.name});
                for (i.params.items) |p| {
                    try w.print(", ", .{});
                    try writeRef(w, p);
                }
            },
            .phi => |i| {
                for (i.params.items, 0..) |p, idx| {
                    if (idx == 0) try w.print(" ", .{}) else try w.print(", ", .{});
                    try writeRef(w, p);
                }
            },

            .define_method => |i| try w.print(" %{s}", .{i.name}),
            .jump => |i| try w.print(" block{d}", .{i.target.name}),
            .cond => |i| {
                try w.print(" ", .{});
                try writeRef(w, i.condition);
                try w.print(", block{d}, block{d}", .{ i.truthy.name, i.falsy.name });
            },
            .leave => |i| {
                try w.print(" ", .{});
                try writeRef(w, i.in);
            },
            .tst => |i| {
                try w.print(" ", .{});
                try writeRef(w, i.in);
            },
        }
        return buf.written();
    }

    pub fn print(cfg: *CFG, mem: std.mem.Allocator, w: *std.Io.Writer) !void {
        var arena = std.heap.ArenaAllocator.init(mem);
        defer arena.deinit();

        const a = arena.allocator();

        const blocks = try buildMIRBlocks(cfg, a);
        const doc = IonJSON{
            .functions = try a.dupe(Func, &.{.{
                .name = cfg.scope.name,
                .passes = try a.dupe(Pass, &.{.{
                    .name = "CFG built",
                    .mir = .{ .blocks = blocks },
                }}),
            }}),
        };

        try std.json.Stringify.value(doc, .{ .whitespace = .indent_2 }, w);
        try w.writeByte('\n');
    }
};
