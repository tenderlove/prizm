const ir = @import("ir.zig");
const std = @import("std");
const cfg = @import("cfg.zig");
const Var = ir.Variable;
const BasicBlock = cfg.BasicBlock;
const BitMap = std.DynamicBitSetUnmanaged;
const assert = @import("std").debug.assert;

pub const Scope = struct {
    tmp_id: u32 = 0,
    local_id: u32 = 0,
    param_id: u32 = 0,
    label_id: usize = 0,
    live_range_id: usize = 0,
    redef_id: usize = 0,
    physical_register_id: usize = 0,
    param_size: usize = 0,
    local_storage: usize = 0,
    primes: usize = 0,
    id: u32,
    name: []const u8,
    insns: ir.InstructionList,
    parent: ?*Scope,
    locals: std.StringHashMapUnmanaged(*Var),
    variables: std.ArrayList(*Var),
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,

    pub fn getName(self: *Scope) []const u8 {
        return self.name;
    }

    pub fn getLocalName(self: *Scope, name: []const u8) !*Var {
        const info = self.locals.get(name);
        if (info) |v| {
            return v;
        } else {
            const lname = try self.newLocal(name);
            try self.locals.put(self.allocator, name, lname);
            return lname;
        }
    }

    fn addVar(self: *Scope, var_: *Var) !*Var {
        try self.variables.append(var_);
        return var_;
    }

    pub fn nextVarId(self: *Scope) usize {
        return self.variables.items.len;
    }

    pub fn varCount(self: Scope) usize {
        return self.variables.items.len;
    }

    pub fn liveRangeCount(self: Scope) usize {
        return self.live_range_id;
    }

    pub fn insnCount(self: *Scope) usize {
        return self.insns.len();
    }

    pub fn getVariableById(self: Scope, id: usize) *Var {
        return self.variables.items[id];
    }

    pub fn getLiveRangeById(self: Scope, id: usize) *Var {
        return self.variables.items[id];
    }

    fn newLocal(self: *Scope, source_name: []const u8) !*Var {
        defer self.local_id += 1;
        return try self.addVar(try Var.initLocal(self.arena.allocator(), self.nextVarId(), self.local_id, source_name));
    }

    pub fn newDefinition(self: *Scope, opnd: *Var, bb: *BasicBlock, variant: usize) !*Var {
        defer self.redef_id += 1;
        const new = try Var.initRedef(self.arena.allocator(), self.nextVarId(), self.redef_id, variant, opnd, bb);
        return try self.addVar(new);
    }

    pub fn newPrime(self: *Scope, op: *Var) !*Var {
        defer self.primes += 1;
        return try self.addVar(try Var.initPrime(self.arena.allocator(), self.nextVarId(), self.primes, op));
    }

    pub fn newLiveRange(self: *Scope, varcount: usize) !*Var {
        defer self.live_range_id += 1;
        return try self.addVar(try Var.initLiveRange(self.arena.allocator(), self.nextVarId(), self.live_range_id, varcount));
    }

    pub fn newTemp(self: *Scope) !*Var {
        defer self.tmp_id += 1;
        return try self.addVar(try Var.initTemp(self.arena.allocator(), self.nextVarId(), self.tmp_id));
    }

    pub fn newPhysicalRegister(self: *Scope, register: usize) !*Var {
        defer self.physical_register_id += 1;
        return try self.addVar(try Var.initPhysicalRegister(self.arena.allocator(), self.nextVarId(), self.physical_register_id, register));
    }

    pub fn newLabel(self: *Scope) ir.Label {
        defer self.label_id += 1;
        return .{ .id = self.label_id };
    }

    fn makeInsn(self: *Scope, insn: ir.Instruction) !*ir.InstructionListNode {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.*.data = insn;
        return node;
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        self.insns.append(&(try self.makeInsn(insn)).node);
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !*Var {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.*.data = insn;
        self.insns.append(&node.node);

        return switch (insn) {
            .putlabel => unreachable,
            .jump => unreachable,
            .jumpif => unreachable,
            .jumpunless => unreachable,
            .setlocal => unreachable,
            inline else => |payload| payload.out,
        };
    }

    pub fn pushDefineMethod(self: *Scope, name: []const u8, scope: *Scope) !*Var {
        const outreg = try self.newTemp();
        return try self.pushInsn(.{ .define_method = .{
            .out = outreg,
            .name = name,
            .func = scope,
        } });
    }

    pub fn pushCall(self: *Scope, out: ?*Var, recv: *Var, name: []const u8, params: std.ArrayList(*Var)) !*Var {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .call = .{
            .out = outreg,
            .recv = recv,
            .name = name,
            .params = params,
        } });
    }

    pub fn pushGetself(self: *Scope) !*Var {
        const outreg = try self.newTemp();
        return try self.pushInsn(.{ .getself = .{ .out = outreg } });
    }

    pub fn pushGetParam(self: *Scope, out: *Var, index: usize) !*Var {
        return try self.pushInsn(.{ .getparam = .{ .out = out, .index = index } });
    }

    pub fn pushSetParam(self: *Scope, in: *Var, index: usize) !*Var {
        return try self.pushInsn(.{ .setparam = .{
            .out = try self.newTemp(),
            .in = in,
            .index = index,
        } });
    }

    pub fn pushJump(self: *Scope, label: ir.Label) !void {
        try self.pushVoidInsn(.{ .jump = .{ .label = label } });
    }

    pub fn pushJumpIf(self: *Scope, in: *Var, label: ir.Label) !void {
        try self.pushVoidInsn(.{ .jumpif = .{ .in = in, .label = label } });
    }

    pub fn pushJumpUnless(self: *Scope, in: *Var, label: ir.Label) !void {
        try self.pushVoidInsn(.{ .jumpunless = .{ .in = in, .label = label } });
    }

    pub fn pushLabel(self: *Scope, name: ir.Label) !void {
        try self.pushVoidInsn(.{ .putlabel = .{ .name = name } });
    }

    pub fn pushLeave(self: *Scope, in: *Var) !*Var {
        return try self.pushInsn(.{ .leave = .{ .in = in, .out = try self.newTemp() } });
    }

    pub fn pushLoadi(self: *Scope, out: ?*Var, val: u64) !*Var {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .loadi = .{
            .out = outreg,
            .val = val,
        } });
    }

    pub fn pushLoadNil(self: *Scope, out: ?*Var) !*Var {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .loadnil = .{ .out = outreg } });
    }

    pub fn pushMov(self: *Scope, out: *Var, in: *Var) !*Var {
        try self.pushVoidInsn(.{ .mov = .{ .out = out, .in = in } });
        return out;
    }

    pub fn makeMov(self: *Scope, out: *Var, in: *Var) !*ir.InstructionListNode {
        return try self.makeInsn(.{ .mov = .{ .out = out, .in = in } });
    }

    pub fn insertPhi(self: *Scope, node: *ir.InstructionListNode, op: *Var) !*ir.InstructionListNode {
        const new_node = try self.arena.allocator().create(ir.InstructionListNode);
        const params = std.ArrayList(*Var).init(self.arena.allocator());
        new_node.*.data = .{ .phi = .{ .out = op, .params = params } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn insertParallelCopy(self: *Scope, node: *ir.InstructionListNode, dest: *Var, src: *Var, block: *BasicBlock, group: usize) !*ir.InstructionListNode {
        const new_node = try self.arena.allocator().create(ir.InstructionListNode);
        new_node.*.data = .{ .pmov = .{ .out = dest, .in = src, .block = block, .group = group } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn pushSetLocal(self: *Scope, name: *Var, val: *Var) !void {
        return try self.pushVoidInsn(.{ .setlocal = .{ .name = name, .val = val } });
    }

    pub fn init(alloc: std.mem.Allocator, id: u32, name: []const u8, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        scope.* = Scope{
            .insns = ir.InstructionList{},
            .id = id,
            .name = name,
            .parent = parent,
            .locals = std.StringHashMapUnmanaged(*Var){},
            .variables = std.ArrayList(*Var).init(alloc),
            .allocator = alloc,
            .arena = std.heap.ArenaAllocator.init(alloc),
        };

        return scope;
    }

    pub fn childScopes(self: *Scope, alloc: std.mem.Allocator) !std.ArrayList(*Scope) {
        var children = std.ArrayList(*Scope).init(alloc);

        var it = self.insns.first;
        while (it) |insn| {
            const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
            switch (insn_node.data) {
                .define_method => |method| {
                    // The func field is a scope operand containing the child scope
                    const child_scope = method.func;
                    try children.append(child_scope);
                },
                else => {},
            }
            it = insn.next;
        }

        return children;
    }

    pub const ChildScopeIterator = struct {
        current: ?*ir.InstructionList.Node,

        pub fn next(self: *ChildScopeIterator) ?*Scope {
            while (self.current) |insn| {
                const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
                self.current = insn.next; // Advance to next instruction

                switch (insn_node.data) {
                    .define_method => |method| {
                        return method.func;
                    },
                    else => continue,
                }
            }
            return null;
        }
    };

    pub fn childScopeIterator(self: *Scope) ChildScopeIterator {
        return ChildScopeIterator{
            .current = self.insns.first,
        };
    }

    pub fn numberAllInstructions(self: *Scope) void {
        var counter: usize = 0;
        var it = self.insns.first;
        while (it) |insn| {
            const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
            insn_node.number = counter;
            counter += 1;
            it = insn.next;
        }
    }

    pub fn sweepUnusedInstructions(self: *Scope, alloc: std.mem.Allocator, live_blocks: []const *cfg.BasicBlock) !void {
        const last_insn: *ir.InstructionListNode = @fieldParentPtr("node", self.insns.last.?);

        // First, collect all instruction numbers that are alive
        var alive_numbers = try BitMap.initEmpty(alloc, last_insn.number + 1);
        defer alive_numbers.deinit(alloc);

        // Walk through all live basic blocks and mark their instructions as alive
        for (live_blocks) |block| {
            var iter = block.instructionIter(.{});
            while (iter.next()) |insn| {
                alive_numbers.set(insn.number);
            }
        }

        var counter: usize = 0;

        // Now walk through the scope's instruction list and remove dead instructions
        var it = self.insns.first;
        while (it) |insn| {
            const next_insn = insn.next; // Save next before potentially removing current
            const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);

            // If this instruction number is not in the alive set, remove it
            if (!alive_numbers.isSet(insn_node.number)) {
                self.insns.remove(insn);
                insn_node.data.deinit();
            } else {
                // While we're here, renumber the instructions
                insn_node.number = counter;
                counter += 1;
            }

            it = next_insn;
        }
    }

    pub fn deinit(self: *Scope) void {
        var it = self.insns.first;
        while (it) |insn| {
            it = insn.next;
            @as(*ir.InstructionListNode, @fieldParentPtr("node", insn)).data.deinit();
        }
        self.locals.deinit(self.allocator);
        self.variables.deinit();
        self.arena.deinit();
        self.allocator.destroy(self);
    }
};

test "number and sweep unused instructions" {
    const alloc = std.testing.allocator;

    // Create a scope with some instructions
    const scope = try Scope.init(alloc, 0, "test", null);
    defer scope.deinit();

    // Add some instructions to the scope
    _ = try scope.pushLoadi(null, 123);
    _ = try scope.pushLoadi(null, 456);
    _ = try scope.pushGetself();

    // Number all instructions
    scope.numberAllInstructions();

    // Verify instructions were numbered correctly
    try std.testing.expectEqual(3, scope.insnCount());
    var it = scope.insns.first;
    var expected_number: usize = 0;
    while (it) |insn| {
        const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
        try std.testing.expectEqual(expected_number, insn_node.number);
        expected_number += 1;
        it = insn.next;
    }

    // For testing, we'll verify the function doesn't crash with empty live blocks
    const live_blocks = [_]*cfg.BasicBlock{};

    // Call sweep with no live blocks - this should remove all instructions
    try scope.sweepUnusedInstructions(alloc, &live_blocks);

    // Count remaining instructions (should be 0 since no blocks were live)
    try std.testing.expectEqual(0, scope.insnCount());
}

test "child scope iterator" {
    const allocator = std.testing.allocator;

    // Create a scope with multiple child methods
    const scope = try Scope.init(allocator, 0, "parent", null);
    defer scope.deinit();

    // Add some child method definitions
    _ = try scope.pushDefineMethod("method1", try Scope.init(allocator, 1, "method1", scope));
    _ = try scope.pushDefineMethod("method2", try Scope.init(allocator, 2, "method2", scope));
    _ = try scope.pushDefineMethod("method3", try Scope.init(allocator, 3, "method3", scope));

    // Test the iterator
    var count: usize = 0;
    var iter = scope.childScopeIterator();
    while (iter.next()) |child_scope| {
        count += 1;
        // Verify it's actually a child scope
        try std.testing.expect(child_scope.parent == scope);
    }

    try std.testing.expectEqual(3, count);

    // Compare with existing childScopes method
    const children = try scope.childScopes(allocator);
    defer children.deinit();
    try std.testing.expectEqual(3, children.items.len);
}
