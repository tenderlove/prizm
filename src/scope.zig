const ir = @import("ir.zig");
const Insn = ir.InstructionListNode;
const std = @import("std");
const cfg = @import("cfg.zig");
const CFG = cfg.CFG;
const BasicBlock = @import("basic_block.zig").BasicBlock;
const assert = @import("std").debug.assert;

pub const Scope = struct {
    /// Per-local defs, keyed by source name (interned upstream) then block.
    /// Outer StringHashMap handles content-based string hashing; inner
    /// AutoHashMap uses block pointer identity.
    const DefMap = std.StringHashMapUnmanaged(std.AutoHashMapUnmanaged(*BasicBlock, *Insn));

    /// Compile-time loop context. `break` jumps to `break_target` and
    /// `next` jumps to `next_target`. `break_var` is a fake-local that
    /// carries the loop's exit value through Braun's phi construction: each
    /// break site does `writeVariable(break_var, current, value)` before
    /// jumping, and the loop's natural cond-false predecessor seeds it with
    /// nil. compileWhileNode reads it back at the exit block.
    pub const LoopFrame = struct {
        next_target: *BasicBlock,
        break_target: *BasicBlock,
        break_var: []const u8,
    };

    insn_id: usize = 0,
    block_name: usize = 0,
    loop_var_seq: u32 = 0,
    id: u32,
    name: []const u8,
    parent: ?*Scope,
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    children: std.ArrayList(*CFG) = .empty,
    entry_block: *BasicBlock,
    current_block: *BasicBlock,
    blocks: std.ArrayList(*BasicBlock),
    currentDef: DefMap,
    incomplete_phis: std.AutoHashMapUnmanaged(*BasicBlock, std.StringHashMapUnmanaged(*Insn)) = .empty,
    loop_stack: std.ArrayList(LoopFrame) = .empty,

    pub fn getName(self: *Scope) []const u8 {
        return self.name;
    }

    pub fn writeVariable(self: *Scope, name: []const u8, block: *BasicBlock, value: *Insn) !void {
        const gop = try self.currentDef.getOrPut(self.allocator, name);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.put(self.allocator, block, value);
    }

    fn tryRemoveTrivialPhi(_: *Scope, phi: *Insn) !*Insn {
        var same: ?*Insn = null;
        for (phi.data.phi.params.items) |raw_op| {
            // Resolve through prior aliases so a phi whose operand-of-record
            // is now forwarded compares equal to whatever it forwards to.
            const op = raw_op.resolve();
            if (op == same or op == phi) continue; // dup or self-ref
            if (same != null) return phi;          // merges >=2 distinct values
            same = op;
        }

        // Trivial: `same` is the sole real operand. Forward the phi via
        // aliasing — no use-list walk needed. If `same` is null the phi has
        // no non-self operands (undef case); leave it alone.
        const target = same orelse return phi;
        if (target != phi) phi.alias = target;
        return target;
    }

    fn addPhiOperands(self: *Scope, name: []const u8, block: *BasicBlock, phi: *Insn) !*Insn {
        for (block.predecessors.items) |pred| {
            const operand = try self.readVariable(name, pred);
            try phi.data.phi.params.append(self.allocator, operand);
        }
        return try self.tryRemoveTrivialPhi(phi);
    }

    fn readVariableRecursive(self: *Scope, name: []const u8, block: *BasicBlock) error{OutOfMemory}!*Insn {
        var val: *Insn = undefined;

        if (!block.sealed) {
            // Incomplete CFG
            val = try self.newPhiAtStart(block);
            const outer = try self.incomplete_phis.getOrPut(self.allocator, block);
            if (!outer.found_existing) outer.value_ptr.* = .empty;
            try outer.value_ptr.put(self.allocator, name, val);
        } else if (block.predecessors.items.len == 1) {
            // Common case of 1 predecessor
            val = try self.readVariable(name, block.predecessors.items[0]);
        } else {
            // Break potential cycles with operandless phi
            val = try self.newPhiAtStart(block);
            try self.writeVariable(name, block, val);
            val = try self.addPhiOperands(name, block, val);
        }

        try self.writeVariable(name, block, val);
        return val;
    }

    pub fn readVariable(self: *Scope, name: []const u8, block: *BasicBlock) error{OutOfMemory}!*Insn {
        if (self.currentDef.getPtr(name)) |inner| {
            if (inner.get(block)) |v|
                return v
            else
                return try self.readVariableRecursive(name, block);
        } else {
            @panic("FIXME");
        }
    }

    pub fn insnCount(self: *Scope) usize {
        var count: usize = 0;
        for (self.blocks.items) |block| {
            count += block.insnCount();
        }
        return count;
    }

    fn makeInsn(self: *Scope, insn: ir.Instruction) !*Insn {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.* = .{ .node = .{}, .id = self.insn_id, .data = insn };
        self.insn_id += 1;
        return node;
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        return self.current_block.pushVoidInsn(try self.makeInsn(insn));
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !*Insn {
        return self.current_block.pushInsn(try self.makeInsn(insn));
    }

    /// Fixed-point trivial-phi elimination. Braun's algorithm cascades via
    /// use-lists; we don't have those, so we re-scan every phi until no new
    /// alias is created. Each phi can be aliased at most once, so this
    /// terminates in O(phis * passes) with passes bounded by phi count.
    fn simplifyPhis(self: *Scope) !void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.blocks.items) |block| {
                var it = block.instructionIter(.{});
                while (it.next()) |insn| {
                    if (insn.data != .phi) continue;
                    if (insn.alias != null) continue;
                    _ = try self.tryRemoveTrivialPhi(insn);
                    if (insn.alias != null) changed = true;
                }
            }
        }
    }

    /// One-shot: rewrite every operand pointer through resolve(), so the
    /// mid-end / interpreter / printer see a clean pointer graph with no
    /// forwarding to chase. Aliased phis are left in the insn list as dead
    /// code (nothing references them anymore) for a future sweep.
    fn resolveAllOperands(self: *Scope) void {
        for (self.blocks.items) |block| {
            var it = block.instructionIter(.{});
            while (it.next()) |insn| {
                switch (insn.data) {
                    .call => |*x| {
                        x.recv = x.recv.resolve();
                        for (x.params.items) |*p| p.* = p.*.resolve();
                    },
                    .cond => |*x| x.condition = x.condition.resolve(),
                    .leave => |*x| x.in = x.in.resolve(),
                    .phi => |*x| {
                        for (x.params.items) |*p| p.* = p.*.resolve();
                    },
                    .tst => |*x| x.in = x.in.resolve(),
                    // No instruction-typed operands.
                    .define_method, .getparam, .jump, .loadi, .loadstr,
                    .loadnil, .loadtrue, .loadfalse => {},
                }
            }
        }
    }

    /// Finalize this scope into a CFG. Transfers arena / blocks / children
    /// into the new CFG, then calls the regular `deinit` on the now-empty
    /// scope. No special teardown logic here — `deinit` handles everything.
    pub fn cfg(self: *Scope, mem: std.mem.Allocator) !*CFG {
        try self.simplifyPhis();
        self.resolveAllOperands();

        // Well-formedness invariant at the frontend/CFG boundary: every block
        // handed off must be sealed (no more predecessors coming) and filled
        // (no more instructions coming). Fill is automatic on terminator
        // push (pushJump/pushCond/pushLeave); this catches "forgot to seal"
        // and "block never got a terminator" bugs at the earliest point.
        for (self.blocks.items) |block| {
            assert(block.sealed);
            assert(block.filled);
        }

        // Move the arena out. Replace with a fresh empty one so the
        // subsequent `deinit()` finds nothing to free.
        const arena_out = self.arena;
        self.arena = std.heap.ArenaAllocator.init(self.allocator);

        // toOwnedSlice on blocks/children leaves them as empty ArrayLists,
        // so deinit's loops walk zero elements.
        const cfg_ptr = try CFG.init(
            mem,
            self.name,
            arena_out,
            self.entry_block,
            try self.blocks.toOwnedSlice(mem),
            try self.children.toOwnedSlice(self.allocator),
        );

        self.deinit();
        return cfg_ptr;
    }

    pub fn pushDefineMethod(self: *Scope, name: []const u8, func: *CFG) !*Insn {
        try self.children.append(self.allocator, func);
        return try self.pushInsn(.{ .define_method = .{
            .name = name,
            .func = func,
        } });
    }

    pub fn pushCall(self: *Scope, recv: *Insn, name: []const u8, params: std.ArrayList(*Insn)) !*Insn {
        return try self.pushInsn(.{ .call = .{
            .recv = recv,
            .name = name,
            .params = params,
        } });
    }

    pub fn pushPhi(self: *Scope, params: std.ArrayList(*Insn)) !*Insn {
        return try self.pushInsn(.{ .phi = .{
            .params = params,
        } });
    }

    pub fn pushGetParam(self: *Scope, index: usize) !*Insn {
        return try self.pushInsn(.{ .getparam = .{ .index = index } });
    }

    pub fn pushJump(self: *Scope, target: *BasicBlock) !void {
        try self.pushVoidInsn(.{ .jump = .{ .target = target } });
        try target.addPredecessor(self.allocator, self.current_block);
        self.current_block.filled = true;
    }

    pub fn pushCond(self: *Scope, cond: *Insn, truthy: *BasicBlock, falsy: *BasicBlock) !void {
        try self.pushVoidInsn(.{ .cond = .{ .condition = cond, .truthy = truthy, .falsy = falsy } });
        try truthy.addPredecessor(self.allocator, self.current_block);
        try falsy.addPredecessor(self.allocator, self.current_block);
        self.current_block.filled = true;
    }

    pub fn pushTest(self: *Scope, in: *Insn) !*Insn {
        return try self.pushInsn(.{ .tst = .{ .in = in } });
    }

    pub fn pushLeave(self: *Scope, in: *Insn) !*Insn {
        const insn = try self.pushInsn(.{ .leave = .{ .in = in } });
        self.current_block.filled = true;
        return insn;
    }

    pub fn pushLoadi(self: *Scope, val: u64) !*Insn {
        return try self.pushInsn(.{ .loadi = .{ .val = val } });
    }

    pub fn pushLoadString(self: *Scope, val: []const u8) !*Insn {
        return try self.pushInsn(.{ .loadstr = .{
            .val = val,
        } });
    }

    pub fn pushLoadNil(self: *Scope) !*Insn {
        return try self.pushInsn(.{ .loadnil = .{} });
    }

    pub fn pushLoadTrue(self: *Scope) !*Insn {
        return try self.pushInsn(.{ .loadtrue = .{} });
    }

    pub fn pushLoadFalse(self: *Scope) !*Insn {
        return try self.pushInsn(.{ .loadfalse = .{} });
    }

    pub fn newBlock(self: *Scope) !*BasicBlock {
        defer self.block_name += 1;
        const bb = try BasicBlock.initBlock(self.allocator, self.block_name, false);
        try self.blocks.append(self.allocator, bb);
        return bb;
    }

    pub fn setCurrentBlock(self: *Scope, block: *BasicBlock) void {
        self.current_block = block;
    }

    pub fn currentBlock(self: *Scope) *BasicBlock {
        return self.current_block;
    }

    /// Enter a loop context. Returns the frame's `break_var` name so the
    /// caller can seed it into the natural cond-false predecessor.
    pub fn pushLoopFrame(self: *Scope, next_target: *BasicBlock, break_target: *BasicBlock) ![]const u8 {
        const name = try std.fmt.allocPrint(
            self.arena.allocator(),
            "__break_{d}",
            .{self.loop_var_seq},
        );
        self.loop_var_seq += 1;
        try self.loop_stack.append(self.allocator, .{
            .next_target = next_target,
            .break_target = break_target,
            .break_var = name,
        });
        return name;
    }

    pub fn popLoopFrame(self: *Scope) void {
        _ = self.loop_stack.pop();
    }

    pub fn currentLoopFrame(self: *Scope) ?LoopFrame {
        if (self.loop_stack.items.len == 0) return null;
        return self.loop_stack.items[self.loop_stack.items.len - 1];
    }

    pub fn sealBlock(self: *Scope, block: *BasicBlock) !void {
        if (self.incomplete_phis.getPtr(block)) |inner| {
            var it = inner.iterator();
            while (it.next()) |entry| {
                _ = try self.addPhiOperands(entry.key_ptr.*, block, entry.value_ptr.*);
            }
            inner.deinit(self.allocator);
            _ = self.incomplete_phis.remove(block);
        }
        block.sealed = true;
    }

    fn newPhiAtStart(self: *Scope, block: *BasicBlock) !*Insn {
        const node = try self.makeInsn(.{ .phi = .{ .params = .empty } });
        block.insns.prepend(&node.node);
        return node;
    }

    pub fn init(alloc: std.mem.Allocator, id: u32, name: []const u8, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        const entry_block = try BasicBlock.initBlock(alloc, 0, true);

        scope.* = Scope{
            .id = id,
            .name = name,
            .parent = parent,
            .currentDef = .empty,
            .blocks = .empty,
            .allocator = alloc,
            .entry_block = entry_block,
            .current_block = entry_block,
            .arena = std.heap.ArenaAllocator.init(alloc),
        };

        // Invariant: `self.blocks` owns every block including the entry.
        // `Scope.deinit` frees each block from this list.
        try scope.blocks.append(alloc, entry_block);

        scope.block_name += 1;

        return scope;
    }

    pub fn deinit(self: *Scope) void {
        var cd_it = self.currentDef.iterator();
        while (cd_it.next()) |entry| entry.value_ptr.deinit(self.allocator);
        self.currentDef.deinit(self.allocator);

        // Braun's invariant: every block must be sealed by end of compilation,
        // which means sealBlock has drained every entry from incomplete_phis.
        // A non-empty map here is a bug in the surrounding compiler flow.
        std.debug.assert(self.incomplete_phis.count() == 0);
        self.incomplete_phis.deinit(self.allocator);

        // Every pushLoopFrame must have a matching popLoopFrame by the time
        // the scope tears down.
        std.debug.assert(self.loop_stack.items.len == 0);
        self.loop_stack.deinit(self.allocator);

        // Blocks and children are typically owned by a CFG at this point
        // (both moved out via toOwnedSlice in `cfg()`), leaving empty
        // ArrayLists here. If cfg() never ran (e.g., a pre-CFG unit test),
        // the scope still owns them and these loops free them.
        for (self.blocks.items) |block| {
            block.deinit(self.allocator);
        }
        self.blocks.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.arena.deinit();
        self.allocator.destroy(self);
    }
};

test "instructions are numbered at push time" {
    const alloc = std.testing.allocator;

    const scope = try Scope.init(alloc, 0, "test", null);
    defer scope.deinit();

    const a = try scope.pushLoadi(123);
    const b = try scope.pushLoadi(456);
    const c = try scope.pushLoadi(789);

    // Each push should assign the next sequential id.
    try std.testing.expectEqual(@as(usize, 0), a.id);
    try std.testing.expectEqual(@as(usize, 1), b.id);
    try std.testing.expectEqual(@as(usize, 2), c.id);
}

