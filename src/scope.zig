const ir = @import("ir.zig");
const Insn = ir.InstructionListNode;
const std = @import("std");
const cfg = @import("cfg.zig");
const CFG = cfg.CFG;
const Var = ir.Variable;
const BasicBlock = @import("basic_block.zig").BasicBlock;
const BitMap = std.DynamicBitSetUnmanaged;
const assert = @import("std").debug.assert;

pub const Scope = struct {
    const Key = struct { local: usize, block: *BasicBlock };

    tmp_id: u32 = 0,
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
    currentDef: std.AutoHashMapUnmanaged(Key, *Insn),

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

    pub fn writeVariable(self: *Scope, name: []const u8, block: *BasicBlock, value: *Insn) !void {
        const lname = try self.getLocalName(name);
        try self.currentDef.put(self.allocator, .{ .local = lname.id, .block = block }, value);
    }

    pub fn readVariable(self: *Scope, name: []const u8, block: *BasicBlock) !*Insn {
        const lname = try self.getLocalName(name);
        const insn = self.currentDef.get(.{ .local = lname.id, .block = block });
        if (insn) |v| {
            return v;
        } else {
            @panic("FIXME: implement ReadVariableRecursive");
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

    fn newLocal(self: *Scope, source_name: []const u8) !*Var {
        return try self.addVar(try Var.initLocal(self.arena.allocator(), self.nextVarId(), source_name));
    }

    fn makeInsn(self: *Scope, insn: ir.Instruction) !*Insn {
        const node = try self.arena.allocator().create(ir.InstructionListNode);
        node.* = .{ .node = .{}, .data = insn };
        return node;
    }

    fn pushVoidInsn(self: *Scope, insn: ir.Instruction) !void {
        return self.current_block.pushVoidInsn(self.arena.allocator(), insn);
    }

    fn pushInsn(self: *Scope, insn: ir.Instruction) !*Insn {
        return self.current_block.pushInsn(self.arena.allocator(), insn);
    }

    pub fn cfg(self: *Scope, mem: std.mem.Allocator) !*CFG {
        return try CFG.init(mem, self, self.entry_block, try self.blocks.toOwnedSlice(mem));
    }

    pub fn pushDefineMethod(self: *Scope, name: []const u8, scope: *Scope) !*Insn {
        try self.children.append(self.allocator, scope);
        return try self.pushInsn(.{ .define_method = .{
            .name = name,
            .func = scope,
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
    }

    pub fn pushCond(self: *Scope, cond: *Insn, truthy: *BasicBlock, falsy: *BasicBlock) !void {
        try self.pushVoidInsn(.{ .cond = .{ .condition = cond, .truthy = truthy, .falsy = falsy } });
        try truthy.addPredecessor(self.allocator, self.current_block);
        try falsy.addPredecessor(self.allocator, self.current_block);
    }

    pub fn pushTest(self: *Scope, in: *Insn) !*Insn {
        return try self.pushInsn(.{ .tst = .{ .in = in } });
    }

    pub fn pushLeave(self: *Scope, in: *Insn) !*Insn {
        return try self.pushInsn(.{ .leave = .{ .in = in } });
    }

    pub fn pushLoadi(self: *Scope, val: u64) !*Insn {
        return try self.pushInsn(.{ .loadi = .{ .val = val } });
    }

    pub fn pushLoadString(self: *Scope, val: []const u8) !*Insn {
        return try self.pushInsn(.{ .loadstr = .{ .val = val, } });
    }

    pub fn pushLoadNil(self: *Scope) !*Insn {
        return try self.pushInsn(.{ .loadnil = .{} });
    }

    pub fn newBlock(self: *Scope) !*BasicBlock {
        defer self.block_name += 1;
        const bb = try BasicBlock.initBlock(self.allocator, self.block_name, self, false);
        try self.blocks.append(self.allocator, bb);
        return bb;
    }

    pub fn setCurrentBlock(self: *Scope, block: *BasicBlock) void {
        self.current_block = block;
    }

    pub fn init(alloc: std.mem.Allocator, id: u32, name: []const u8, parent: ?*Scope) !*Scope {
        const scope = try alloc.create(Scope);

        const entry_block = try BasicBlock.initBlock(alloc, 0, scope, true);

        scope.* = Scope{
            .id = id,
            .name = name,
            .parent = parent,
            .locals = std.StringHashMapUnmanaged(*Var){},
            .currentDef = .empty,
            .variables = .empty,
            .children = .empty,
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
        for (self.children.items) |scope| {
            scope.deinit();
        }
        self.children.deinit(self.allocator);
        for (self.blocks.items) |block| {
            block.deinit(self.allocator);
        }
        self.blocks.deinit(self.allocator);
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
