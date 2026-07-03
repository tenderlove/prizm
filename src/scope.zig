const ir = @import("ir.zig");
const std = @import("std");
const cfg = @import("cfg.zig");
const CFG = cfg.CFG;
const Var = ir.Variable;
const BasicBlock = @import("basic_block.zig").BasicBlock;
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
    block_name: usize = 0,
    id: u32,
    name: []const u8,
    parent: ?*Scope,
    locals: std.StringHashMapUnmanaged(*Var),
    variables: std.ArrayList(*Var),
    children: std.ArrayList(*Scope),
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    entry_block: *BasicBlock,
    current_block: *BasicBlock,
    blocks: std.ArrayList(*BasicBlock),

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
        try self.variables.append(self.allocator, var_);
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
        var count: usize = 0;
        for (self.blocks.items) |block| {
            count += block.insnCount();
        }
        return count;
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
        node.* = .{ .node = .{}, .data = insn };
        return node;
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        return self.current_block.pushVoidInsn(self.arena.allocator(), insn);
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !*Var {
        return self.current_block.pushInsn(self.arena.allocator(), insn);
    }

    pub fn cfg(self: *Scope, mem: std.mem.Allocator) !*CFG {
        return try CFG.init(mem, self, self.entry_block, try self.blocks.toOwnedSlice(mem));
    }

    pub fn pushDefineMethod(self: *Scope, name: []const u8, scope: *Scope) !*Var {
        const outreg = try self.newTemp();
        try self.children.append(self.allocator, scope);
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

    pub fn pushLoadString(self: *Scope, out: ?*Var, val: []const u8) !*Var {
        const outreg = if (out) |o| o else try self.newTemp();
        return try self.pushInsn(.{ .loadstr = .{
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

    pub fn pushSetLocal(self: *Scope, name: *Var, val: *Var) !void {
        return try self.pushVoidInsn(.{ .setlocal = .{ .name = name, .val = val } });
    }

    pub fn init(alloc: std.mem.Allocator, id: u32, name: []const u8, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        const entry_block = try BasicBlock.initBlock(alloc, 0, scope, true);

        scope.* = Scope{
            .id = id,
            .name = name,
            .parent = parent,
            .locals = std.StringHashMapUnmanaged(*Var){},
            .variables = .empty,
            .children = .empty,
            .blocks = .empty,
            .allocator = alloc,
            .entry_block = entry_block,
            .current_block = entry_block,
            .arena = std.heap.ArenaAllocator.init(alloc),
        };

        scope.block_name += 1;

        return scope;
    }

    pub fn childScopes(self: *Scope) std.ArrayList(*Scope) {
        return self.children;
    }

    pub fn numberAllInstructions(self: *Scope) void {
        var counter: usize = 0;
        for (self.blocks.items) |block| {
            var it = block.insns.first;
            while (it) |insn| {
                const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
                insn_node.number = counter;
                counter += 1;
                it = insn.next;
            }
        }
    }

    pub fn deinit(self: *Scope) void {
        self.locals.deinit(self.allocator);
        self.variables.deinit(self.allocator);
        self.children.deinit(self.allocator);
        self.arena.deinit();
        self.allocator.destroy(self);
    }
};

test "number instructions" {
    const alloc = std.testing.allocator;

    // Create a scope with some instructions
    const scope = try Scope.init(alloc, 0, "test", null);
    defer scope.deinit();

    // Add some instructions to the scope
    _ = try scope.pushLoadi(null, 123);
    _ = try scope.pushLoadi(null, 456);
    _ = try scope.pushLoadi(null, 789);

    // Number all instructions
    scope.numberAllInstructions();

    // Verify instructions were numbered correctly
    try std.testing.expectEqual(3, scope.insnCount());
    var it = scope.current_block.insns.first;
    var expected_number: usize = 0;
    while (it) |insn| {
        const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);
        try std.testing.expectEqual(expected_number, insn_node.number);
        expected_number += 1;
        it = insn.next;
    }
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
    for (scope.children.items) |child_scope| {
        count += 1;
        // Verify it's actually a child scope
        try std.testing.expect(child_scope.parent == scope);
    }

    try std.testing.expectEqual(3, count);
}
