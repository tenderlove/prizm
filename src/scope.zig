const ir = @import("ir.zig");
const std = @import("std");
const cfg = @import("cfg.zig");
const Op = ir.Operand;
const BasicBlock = cfg.BasicBlock;
const BitMap = std.DynamicBitSetUnmanaged;

pub const Scope = struct {
    tmp_id: u32 = 0,
    local_id: u32 = 0,
    param_id: u32 = 0,
    label_id: u32 = 0,
    param_size: usize = 0,
    local_storage: usize = 0,
    primes: usize = 0,
    id: u32,
    name: []const u8,
    insns: ir.InstructionList,
    parent: ?*Scope,
    locals: std.StringHashMapUnmanaged(*ir.Operand),
    operands: std.ArrayList(*ir.Operand),
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,

    pub fn getName(self: *Scope) []const u8 {
        return self.name;
    }

    pub fn maxId(self: *Scope) u32 {
        const list = [_]u32{ self.tmp_id, self.local_id, self.param_id, self.label_id };
        var max: u32 = 0;
        for (list) |item| {
            if (item > max) {
                max = item;
            }
        }
        return max;
    }

    pub fn getLocalName(self: *Scope, name: []const u8) !*ir.Operand {
        const info = self.locals.get(name);
        if (info) |v| {
            return v;
        } else {
            const lname = try self.newLocal(name);
            try self.locals.put(self.allocator, name, lname);
            return lname;
        }
    }

    fn addOpnd(self: *Scope, opnd: *ir.Operand) !*ir.Operand {
        try self.operands.append(opnd);
        return opnd;
    }

    pub fn nextOpndId(self: *Scope) usize {
        return self.operands.items.len;
    }

    pub fn opndCount(self: Scope) usize {
        return self.operands.items.len;
    }

    pub fn insnCount(self: *Scope) usize {
        return self.insns.len();
    }

    pub fn getOperandById(self: Scope, id: usize) *ir.Operand {
        return self.operands.items[id];
    }

    fn newLocal(self: *Scope, source_name: []const u8) !*ir.Operand {
        const name = self.local_id;
        self.local_id += 1;
        return try self.addOpnd(try ir.Operand.initLocal(self.arena.allocator(), self.nextOpndId(), name, source_name));
    }

    fn newScope(self: *Scope, scope: *Scope) !*ir.Operand {
        return try self.addOpnd(try ir.Operand.initScope(self.arena.allocator(), scope));
    }

    fn newString(self: *Scope, name: []const u8) !*ir.Operand {
        return try self.addOpnd(try ir.Operand.initString(self.arena.allocator(), name));
    }

    pub fn newDefinition(self: *Scope, opnd: *ir.Operand, bb: *BasicBlock, variant: usize) !*ir.Operand {
        const new = try ir.Operand.initRedef(self.arena.allocator(), self.nextOpndId(), variant, opnd, bb);
        return try self.addOpnd(new);
    }

    pub fn newPrime(self: *Scope, op: *Op) !*Op {
        const new = try Op.initPrime(self.arena.allocator(), self.nextOpndId(), self.primes, op);
        self.primes += 1;
        return try self.addOpnd(new);
    }

    pub fn newTemp(self: *Scope) !*ir.Operand {
        const name = self.tmp_id;
        self.tmp_id += 1;
        return try self.addOpnd(try ir.Operand.initTemp(self.arena.allocator(), self.nextOpndId(), name));
    }

    fn newImmediate(self: *Scope, value: u64) !*ir.Operand {
        return try self.addOpnd(try ir.Operand.initImmediate(self.arena.allocator(), value));
    }

    pub fn newLabel(self: *Scope) !*ir.Operand {
        const name = self.label_id;
        self.label_id += 1;
        return try self.addOpnd(try ir.Operand.initLabel(self.arena.allocator(), name));
    }

    fn makeInsn(self: *Scope, insn: ir.Instruction) !*ir.InstructionListNode {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.*.data = insn;
        return node;
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        self.insns.append(&(try self.makeInsn(insn)).node);
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !*ir.Operand {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.*.data = insn;
        self.insns.append(&node.node);

        return switch (insn) {
            .putlabel => unreachable,
            .jump => unreachable,
            .jumpif => unreachable,
            .jumpunless => unreachable,
            .setlocal => unreachable,
            .leave => unreachable,
            inline else => |payload| payload.out,
        };
    }

    pub fn pushDefineMethod(self: *Scope, name: []const u8, scope: *Scope) !*ir.Operand {
        const outreg = try self.newTemp();
        return try self.pushInsn(.{ .define_method = .{
            .out = outreg,
            .name = try self.newString(name),
            .func = try self.newScope(scope),
        } });
    }

    pub fn pushCall(self: *Scope, out: ?*ir.Operand, recv: *ir.Operand, name: []const u8, params: std.ArrayList(*ir.Operand)) !*ir.Operand {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .call = .{
            .out = outreg,
            .recv = recv,
            .name = try self.newString(name),
            .params = params,
        } });
    }

    pub fn pushGetself(self: *Scope) !*ir.Operand {
        const outreg = try self.newTemp();
        return try self.pushInsn(.{ .getself = .{ .out = outreg } });
    }

    pub fn pushGetParam(self: *Scope, out: *ir.Operand, index: usize) !*ir.Operand {
        return try self.pushInsn(.{ .getparam = .{ .out = out, .index = index } });
    }

    pub fn pushJump(self: *Scope, label: *ir.Operand) !void {
        try self.pushVoidInsn(.{ .jump = .{ .label = label } });
    }

    pub fn pushJumpIf(self: *Scope, in: *ir.Operand, label: *ir.Operand) !void {
        try self.pushVoidInsn(.{ .jumpif = .{ .in = in, .label = label } });
    }

    pub fn pushJumpUnless(self: *Scope, in: *ir.Operand, label: *ir.Operand) !void {
        try self.pushVoidInsn(.{ .jumpunless = .{ .in = in, .label = label } });
    }

    pub fn pushLabel(self: *Scope, name: *ir.Operand) !void {
        try self.pushVoidInsn(.{ .putlabel = .{ .name = name } });
    }

    pub fn pushLeave(self: *Scope, in: *ir.Operand) !void {
        try self.pushVoidInsn(.{ .leave = .{ .in = in } });
    }

    pub fn pushLoadi(self: *Scope, out: ?*Op, val: u64) !*ir.Operand {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .loadi = .{
            .out = outreg,
            .val = try self.newImmediate(val),
        } });
    }

    pub fn pushLoadNil(self: *Scope, out: ?*Op) !*ir.Operand {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .loadnil = .{ .out = outreg } });
    }

    pub fn pushMov(self: *Scope, out: *ir.Operand, in: *ir.Operand) !*ir.Operand {
        try self.pushVoidInsn(.{ .mov = .{ .out = out, .in = in } });
        return out;
    }

    pub fn makeMov(self: *Scope, out: *ir.Operand, in: *ir.Operand) !*ir.InstructionListNode {
        return try self.makeInsn(.{ .mov = .{ .out = out, .in = in } });
    }

    pub fn insertPhi(self: *Scope, node: *ir.InstructionListNode, op: *ir.Operand) !*ir.InstructionListNode {
        const new_node = try self.arena.allocator().create(ir.InstructionListNode);
        const params = std.ArrayList(*ir.Operand).init(self.arena.allocator());
        new_node.*.data = .{ .phi = .{ .out = op, .params = params } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn insertParallelCopy(self: *Scope, node: *ir.InstructionListNode, dest: *Op, src: *Op, block: *BasicBlock, group: usize) !*ir.InstructionListNode {
        const new_node = try self.arena.allocator().create(ir.InstructionListNode);
        new_node.*.data = .{ .pmov = .{ .out = dest, .in = src, .block = block, .group = group } };
        self.insns.insertAfter(&node.node, &new_node.node);
        return new_node;
    }

    pub fn pushSetLocal(self: *Scope, name: *ir.Operand, val: *ir.Operand) !void {
        return try self.pushVoidInsn(.{ .setlocal = .{ .name = name, .val = val } });
    }

    pub fn init(alloc: std.mem.Allocator, id: u32, name: []const u8, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        scope.* = Scope{
            .insns = ir.InstructionList{},
            .id = id,
            .name = name,
            .parent = parent,
            .locals = std.StringHashMapUnmanaged(*ir.Operand){},
            .operands = std.ArrayList(*ir.Operand).init(alloc),
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
                    const child_scope = method.func.data.scope.value;
                    try children.append(child_scope);
                },
                else => {},
            }
            it = insn.next;
        }

        return children;
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
            var iter = block.instructionIter();
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
        self.operands.deinit();
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
