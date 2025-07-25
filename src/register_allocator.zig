const std = @import("std");
const CFG = @import("cfg.zig").CFG;
const cmp = @import("compiler.zig");
const BasicBlock = @import("cfg.zig").BasicBlock;
const ir = @import("ir.zig");
const Var = ir.Variable;
const BitMap = std.DynamicBitSetUnmanaged;
const assert = @import("std").debug.assert;
const UnionFind = @import("union_find.zig").UnionFind;
const IRPrinter = @import("printer.zig").IRPrinter;
const InterferenceGraph = @import("interference_graph.zig").InterferenceGraph;
const VM = @import("vm.zig");
const bitmatrix = @import("utils/bitmatrix.zig");
const BitMatrix = bitmatrix.BitMatrix;

// Physical register representation
pub const PhysicalRegister = enum(u8) {
    R0 = 0,
    R1 = 1,
    R2 = 2,
    R3 = 3,
    R4 = 4,
    R5 = 5,
    R6 = 6,
    R7 = 7,
    // Add more registers as needed
};
const MAX_PARAM_REGISTERS = 4; // R0-R3 for parameters
const RETURN_REGISTER = 0; // R0

const K = @typeInfo(PhysicalRegister).@"enum".fields.len;

// Lookup table from Live Range to Physical Register
// Index is the LiveRange's local ID
pub const RegisterMapping = std.ArrayList(*const Var);

// Map of variable id (global) to LiveRange variable
pub const LiveRangeMap = std.AutoHashMap(usize, *Var);

// List of live ranges
pub const LiveRangeList = std.ArrayList(*Var);

const ChaitinAllocator = struct {
    graph: *InterferenceGraph,
    lr_list: std.ArrayList(*Var),
    physical_registers: std.ArrayList(*Var),

    pub fn init(graph: *InterferenceGraph, lr_list: LiveRangeList, physical_registers: std.ArrayList(*Var)) ChaitinAllocator {
        return ChaitinAllocator{ .graph = graph, .lr_list = lr_list, .physical_registers = physical_registers };
    }

    pub fn deinit(self: *ChaitinAllocator, allocator: std.mem.Allocator) void {
        _ = self;
        _ = allocator;
    }

    pub fn allocate(self: *ChaitinAllocator, allocator: std.mem.Allocator) !RegisterMapping {
        const stack = try self.simplify(allocator);
        defer stack.deinit();
        return self.select(allocator, stack);
    }

    fn simplify(self: *ChaitinAllocator, allocator: std.mem.Allocator) !LiveRangeList {
        const work_list = self.lr_list;
        var graph = try self.graph.clone(allocator);
        defer graph.deinit(allocator);

        // Create a stack for each constraint type based on enum size
        const constraint_count = @typeInfo(ir.RegisterConstraint).@"union".fields.len;
        var constraint_stacks: [constraint_count]LiveRangeList = undefined;
        for (0..constraint_count) |i| {
            constraint_stacks[i] = LiveRangeList.init(allocator);
        }
        defer {
            for (0..constraint_count) |i| {
                constraint_stacks[i].deinit();
            }
        }

        var selected = try BitMap.initFull(allocator, work_list.items.len);
        defer selected.deinit(allocator);

        var trial: usize = 0;

        while (selected.count() != 0) {
            var iter = selected.iterator(.{});
            var trivial = LiveRangeList.init(allocator);
            defer trivial.deinit();
            while (iter.next()) |idx| {
                const lr = work_list.items[idx];
                if (graph.degree(lr.getLocalId()) < 4) {
                    selected.unset(idx);
                    try trivial.append(lr);
                }
            }

            // Group trivial nodes by their register constraints
            for (trivial.items) |lr| {
                const constraint = lr.data.live_range.constraint;
                const constraint_index = @intFromEnum(constraint);
                try constraint_stacks[constraint_index].append(lr);
                graph.removeNode(lr.getLocalId());
            }

            if (trivial.items.len == 0 and selected.count() != 0) {
                // TODO: figure out how to calculate spill cost
                return error.CannotAllocate;
            }
            trial += 1;
        }

        // Concatenate stacks in forward order (easiest to hardest)
        // This puts hardest constraints on top of stack for priority processing
        var final_stack = LiveRangeList.init(allocator);
        for (0..constraint_count) |i| {
            try final_stack.appendSlice(constraint_stacks[i].items);
        }

        return final_stack;
    }

    fn select(self: *ChaitinAllocator, allocator: std.mem.Allocator, worklist: LiveRangeList) !RegisterMapping {
        var register_mapping = RegisterMapping.init(allocator);

        // Initialize with undefined - index will be live range local ID
        try register_mapping.resize(self.graph.size());

        // Track which live ranges have been assigned
        var assigned = try BitMap.initEmpty(allocator, worklist.items.len);
        defer assigned.deinit(allocator);

        // Iterate through worklist in reverse order (stack behavior)
        var i = worklist.items.len;
        while (i > 0) {
            i -= 1;
            const lr = worklist.items[i];
            const lr_id = lr.getLocalId();

            // Check which colors are forbidden by already-colored neighbors
            var forbidden = std.StaticBitSet(K).initEmpty();
            var neighbor_iter = self.graph.neighborIterator(lr_id);
            while (neighbor_iter.next()) |neighbor_id| {
                if (assigned.isSet(neighbor_id)) {
                    const reg_var = register_mapping.items[neighbor_id];
                    // Extract the physical register from the variable
                    const reg = reg_var.data.physical_register.register;
                    forbidden.set(reg);
                }
            }

            const color: PhysicalRegister = switch (lr.data.live_range.constraint) {
                .general_purpose => blk: {
                    // Pick first available color
                    for (0..K) |reg_num| {
                        if (!forbidden.isSet(reg_num)) {
                            break :blk @enumFromInt(reg_num);
                        }
                    } else return error.OutOfRegisters;
                },
                .register_class => unreachable, // TODO: make use of this
                .specific_register => |sr| blk: {
                    if (forbidden.isSet(sr)) {
                        return error.RegisterConstraintConflict;
                    } else {
                        break :blk @enumFromInt(sr);
                    }
                },
            };

            // Get the static Variable for this physical register
            const reg_var = self.getPhysicalRegisterVar(color);
            register_mapping.items[lr_id] = reg_var;
            assigned.set(lr_id);
        }

        return register_mapping;
    }

    fn getPhysicalRegisterVar(self: *ChaitinAllocator, reg: PhysicalRegister) *const Var {
        return self.physical_registers.items[@intFromEnum(reg)];
    }
};

pub const RegisterAllocator = struct {
    allocator: std.mem.Allocator,
    cfg: *CFG,
    union_find: UnionFind,
    range_map: LiveRangeMap,
    interference_graph: *InterferenceGraph,
    register_mapping: RegisterMapping,

    /// Performs register allocation and returns a RegisterAllocator with all data structures populated
    pub fn allocateRegisters(allocator: std.mem.Allocator, cfg: *CFG) !*RegisterAllocator {
        // Ensure CFG is in the right state (after SSA renaming, before phi isolation)
        assert(cfg.state == .renamed);

        // Reanalyze so we get the right liveout sets
        try cfg.analyze();

        // Step 1: Each SSA name starts in its own set
        const var_count = cfg.scope.varCount();
        var union_find = try UnionFind.init(allocator, var_count);
        errdefer union_find.deinit();

        // Step 2: Visit each phi function and union parameters with result
        processPhiFunctions(cfg, &union_find);

        // Step 3: Build live ranges from union-find groups
        var range_map = LiveRangeMap.init(allocator);
        errdefer range_map.deinit();
        try buildLiveRangesFromUnionFind(cfg, &union_find, &range_map);

        // Rewrite the code with Live Ranges
        try rewriteWithLiveRanges(cfg, &union_find, &range_map);

        // We have to re-analyze the CFG after updating the code with LR
        try cfg.analyze();

        // Build an interference graph
        const interference_graph = try InterferenceGraph.init(allocator, cfg.scope.liveRangeCount());
        errdefer interference_graph.deinit(allocator);
        try buildInterferenceGraph(allocator, cfg, interference_graph);

        // Print interference graph
        //const stdout = std.io.getStdErr().writer().any();
        //try @import("printer.zig").printInterferenceGraphDOT(allocator, cfg, interference_graph, stdout);

        // Build a list of live ranges from the LiveRangeMap
        var lr_list = std.ArrayList(*Var).init(allocator);
        defer lr_list.deinit();

        var value_iter = range_map.valueIterator();
        while (value_iter.next()) |value_ptr| {
            try lr_list.append(value_ptr.*);
        }

        // Build a list of Variables that wrap Physical Registers
        var physical_registers = std.ArrayList(*Var).init(allocator);
        defer physical_registers.deinit();
        for (0..K) |i| {
            try physical_registers.append(try cfg.scope.newPhysicalRegister(i));
        }

        // Create register mapping
        var alloc = ChaitinAllocator.init(interference_graph, lr_list, physical_registers);
        defer alloc.deinit(allocator);

        var register_mapping = try alloc.allocate(allocator);
        errdefer register_mapping.deinit();

        // Remove phi nodes before rewriting with physical registers
        try cfg.removePhi();

        // Assert that all variables are still live ranges after phi removal
        assertAllVariablesAreLiveRanges(cfg);

        rewriteWithPhysicalRegisters(cfg, register_mapping);

        // Create the allocator instance with all data structures populated
        const self = try allocator.create(RegisterAllocator);

        self.* = RegisterAllocator{
            .allocator = allocator,
            .cfg = cfg,
            .union_find = union_find,
            .range_map = range_map,
            .interference_graph = interference_graph,
            .register_mapping = register_mapping,
        };

        return self;
    }

    pub fn deinit(self: *RegisterAllocator) void {
        self.union_find.deinit();
        self.range_map.deinit();
        self.interference_graph.deinit(self.allocator);
        self.register_mapping.deinit();
        self.allocator.destroy(self);
    }

    fn buildInterferenceGraph(allocator: std.mem.Allocator, cfg: *CFG, graph: *InterferenceGraph) !void {
        for (cfg.blocks) |bb| {
            assert(bb.reachable);

            // The LiveOut set should be a set of Live Ranges
            var live_now = try bb.liveout_set.clone(allocator);
            defer live_now.deinit(allocator);

            var iter = bb.instructionIter(.{ .direction = .reverse });
            while (iter.next()) |insn| {
                // TODO: we need to skip adding edges if the operation is a mov
                if (insn.data.outVar()) |n| {
                    assert(n.data == .live_range);

                    live_now.unset(n.getGlobalId());
                    var live_iter = live_now.iterator(.{});
                    while (live_iter.next()) |lr_id| {
                        const from_id = cfg.scope.getVariableById(lr_id).getLocalId();
                        graph.add(from_id, n.getLocalId());
                    }
                }

                var opiter = insn.data.opIter();
                while (opiter.next()) |op| {
                    assert(op.data == .live_range);

                    live_now.set(op.getGlobalId());
                }
            }
        }
    }

    // Step 2: Process phi functions to union related variables
    fn processPhiFunctions(cfg: *CFG, union_find: *UnionFind) void {
        for (cfg.blocks) |bb| {
            assert(bb.reachable);

            var iter = bb.instructionIter(.{});
            while (iter.next()) |insn_node| {
                switch (insn_node.data) {
                    .putlabel => {}, // Skip putlabel instructions
                    .phi => |phi| {
                        const phi_out = phi.out.id;

                        // Union the phi output with each of its parameters
                        for (phi.params.items) |input_var| {
                            // This is the core of the algorithm: union phi parameters with result
                            union_find.unite(phi_out, input_var.id);
                        }
                    },
                    else => {
                        // Quit after we've passed phi's
                        break;
                    },
                }
            }
        }
    }

    // Step 3: Build live ranges from union-find groups
    fn buildLiveRangesFromUnionFind(cfg: *CFG, union_find: *UnionFind, range_map: *std.AutoHashMap(usize, *Var)) !void {
        const varcount = cfg.scope.varCount();
        const scope = cfg.scope;

        // Process each variable and assign it to a live range
        for (cfg.blocks) |block| {
            assert(block.reachable);
            var iter = block.killed_set.iterator(.{});
            while (iter.next()) |var_id| {
                const root = union_find.find(var_id);
                if (range_map.get(root)) |range| {
                    // Add this variable to existing live range
                    range.addVariable(var_id);
                } else {
                    // Create new live range for this group
                    var new_range = try scope.newLiveRange(varcount);
                    new_range.addVariable(var_id);
                    try range_map.put(root, new_range);
                }
            }
        }
    }

    fn rewriteWithLiveRanges(cfg: *CFG, union_find: *UnionFind, range_map: *std.AutoHashMap(usize, *Var)) !void {
        // Iterate through all instructions in the scope and replace variables with live ranges
        var node = cfg.scope.insns.first;

        while (node) |insn_node| {
            const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", insn_node);
            const insn = &insn_list_node.data;

            // Replace output variable if it exists
            if (insn.getOut()) |out_var| {
                const root = union_find.find(out_var.id);
                if (range_map.get(root)) |live_range_var| {
                    insn.setOut(live_range_var);
                    live_range_var.setDefinition(insn);
                    switch (insn.*) {
                        .getparam => |param| {
                            if (param.index < MAX_PARAM_REGISTERS) {
                                live_range_var.setSpecificRegister(param.index);
                            } else {
                                // Transform getparam into load_stack_param
                                const stack_index = param.index - MAX_PARAM_REGISTERS;
                                insn.* = .{ .load_stack_param = .{
                                    .out = live_range_var,
                                    .offset = stack_index,
                                } };
                            }
                        },
                        .setparam => |param| {
                            if (param.index < MAX_PARAM_REGISTERS) {
                                live_range_var.setSpecificRegister(param.index);
                            }
                        },
                        .call, .leave => {
                            live_range_var.setSpecificRegister(RETURN_REGISTER);
                        },
                        else => {},
                    }
                } else {
                    unreachable;
                }
            }

            // Replace input operands
            var op_iter = insn.opIter();
            while (op_iter.next()) |operand| {
                const root = union_find.find(operand.id);
                if (range_map.get(root)) |live_range_var| {
                    insn.replaceOpnd(operand, live_range_var);
                    try live_range_var.addUse(cfg.arena.allocator(), insn);
                } else {
                    unreachable;
                }
            }

            node = insn_node.next;
        }
    }

    fn rewriteWithPhysicalRegisters(cfg: *CFG, register_mapping: RegisterMapping) void {
        // Collect all replacements first to avoid iterator corruption
        const Replacement = struct {
            insn: *ir.Instruction,
            old_var: *const Var,
            new_var: *const Var,
            is_output: bool,
        };

        var replacements = std.ArrayList(Replacement).init(std.heap.page_allocator);
        defer replacements.deinit();

        // First pass: collect all replacements
        var node = cfg.scope.insns.first;
        while (node) |insn_node| {
            const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", insn_node);
            const insn = &insn_list_node.data;

            // Collect output variable replacement
            if (insn.getOut()) |out_var| {
                assert(out_var.data == .live_range);
                const lr_id = out_var.getLocalId();
                const physical_reg_var = register_mapping.items[lr_id];
                replacements.append(.{
                    .insn = insn,
                    .old_var = out_var,
                    .new_var = physical_reg_var,
                    .is_output = true,
                }) catch unreachable;
            }

            // Collect input operand replacements
            var op_iter = insn.opIter();
            while (op_iter.next()) |operand| {
                assert(operand.data == .live_range);
                const lr_id = operand.getLocalId();
                const physical_reg_var = register_mapping.items[lr_id];
                replacements.append(.{
                    .insn = insn,
                    .old_var = operand,
                    .new_var = physical_reg_var,
                    .is_output = false,
                }) catch unreachable;
            }

            node = insn_node.next;
        }

        // Second pass: apply all replacements
        for (replacements.items) |replacement| {
            if (replacement.is_output) {
                replacement.insn.setOut(@constCast(replacement.new_var));
            } else {
                replacement.insn.replaceOpnd(@constCast(replacement.old_var), @constCast(replacement.new_var));
            }
        }
    }

    fn assertAllVariablesAreLiveRanges(cfg: *CFG) void {
        // Iterate through all instructions and assert that all variables are live ranges
        var node = cfg.scope.insns.first;

        while (node) |insn_node| {
            const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", insn_node);
            const insn = &insn_list_node.data;

            // Check output variable if it exists
            if (insn.getOut()) |out_var| {
                if (out_var.data != .live_range) {
                    std.debug.print("Non-live-range output variable: {} in instruction: {}\n", .{ out_var.data, insn.* });
                    assert(out_var.data == .live_range);
                }
            }

            // Check input operands
            var op_iter = insn.opIter();
            while (op_iter.next()) |operand| {
                if (operand.data != .live_range) {
                    std.debug.print("Non-live-range operand: {} in instruction: {}\n", .{ operand.data, insn.* });
                    assert(operand.data == .live_range);
                }
            }

            node = insn_node.next;
        }
    }
};

test "live ranges" {
    const allocator = std.testing.allocator;

    const machine = try VM.init(allocator);
    defer machine.deinit(allocator);

    const code =
        \\ x = 123
        \\ y = 456
        \\ if foo
        \\   y += 3
        \\ else
        \\   y += 2
        \\ end
        \\ x + y
    ;

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();
    try cfg.compileUntil(.renamed);
    const ra = try RegisterAllocator.allocateRegisters(allocator, cfg);
    defer ra.deinit();

    // const stdout = std.io.getStdErr().writer().any();
    // try @import("printer.zig").printInterferenceGraphDOT(allocator, cfg, ra.interference_graph, stdout);

    // std.debug.print("edge count {d}\n", .{ ra.interference_graph.edgeCount() });
}

test "params use specific registers" {
    const allocator = std.testing.allocator;

    const machine = try VM.init(allocator);
    defer machine.deinit(allocator);

    const code =
        \\ def a b, c
        \\   b + c
        \\ end
    ;

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();
    try cfg.compileUntil(.renamed);
    const ra = try RegisterAllocator.allocateRegisters(allocator, cfg);
    defer ra.deinit();

    var iter = scope.childScopeIterator();
    const method_scope = iter.next().?;

    // Build CFG for the method scope to get register allocation
    const method_cfg = try CFG.build(allocator, method_scope);
    defer method_cfg.deinit();
    try method_cfg.compileUntil(.renamed);
    const method_ra = try RegisterAllocator.allocateRegisters(allocator, method_cfg);
    defer method_ra.deinit();

    // Find getparam instructions and verify their register assignments
    var insn_node = method_scope.insns.first;
    var param_count: usize = 0;
    var leave_count: usize = 0;
    var call_count: usize = 0;

    while (insn_node) |node| {
        const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", node);

        switch (insn_list_node.data) {
            .getparam => |param| {
                // After register allocation, getparam variables should be physical registers
                const out_var = param.out;

                // Verify it's a physical register with the correct register index
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(param.index, out_var.data.physical_register.register);
                param_count += 1;
            },
            .leave => |param| {
                const out_var = param.out;
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(0, out_var.data.physical_register.register);
                leave_count += 1;
            },
            .call => |param| {
                const out_var = param.out;
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(0, out_var.data.physical_register.register);
                call_count += 1;
            },
            else => {},
        }

        insn_node = node.next;
    }

    // Verify we found the expected number of parameters
    try std.testing.expectEqual(3, param_count);
    try std.testing.expectEqual(1, leave_count);
    try std.testing.expectEqual(1, call_count);
}

test "N params use N specific registers" {
    const allocator = std.testing.allocator;

    const machine = try VM.init(allocator);
    defer machine.deinit(allocator);

    const code =
        \\ def a b, c, d, e
        \\   b + c + d + e
        \\ end
    ;

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();
    try cfg.compileUntil(.renamed);
    const ra = try RegisterAllocator.allocateRegisters(allocator, cfg);
    defer ra.deinit();

    var iter = scope.childScopeIterator();
    const method_scope = iter.next().?;

    // Build CFG for the method scope to get register allocation
    const method_cfg = try CFG.build(allocator, method_scope);
    defer method_cfg.deinit();
    try method_cfg.compileUntil(.renamed);
    const method_ra = try RegisterAllocator.allocateRegisters(allocator, method_cfg);
    defer method_ra.deinit();

    // Find getparam instructions and verify their register assignments
    var insn_node = method_scope.insns.first;
    var param_count: usize = 0;
    var leave_count: usize = 0;
    var call_count: usize = 0;

    while (insn_node) |node| {
        const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", node);

        switch (insn_list_node.data) {
            .getparam => |param| {
                // After register allocation, getparam variables should be physical registers
                const out_var = param.out;

                // Verify it's a physical register with the correct register index
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(param.index, out_var.data.physical_register.register);
                param_count += 1;
            },
            .leave => |param| {
                const out_var = param.out;
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(0, out_var.data.physical_register.register);
                leave_count += 1;
            },
            .call => |param| {
                const out_var = param.out;
                try std.testing.expectEqual(.physical_register, @as(ir.VariableType, out_var.data));
                try std.testing.expectEqual(0, out_var.data.physical_register.register);
                call_count += 1;
            },
            else => {},
        }

        insn_node = node.next;
    }

    // Verify we found the expected number of parameters
    try std.testing.expectEqual(4, param_count);
    try std.testing.expectEqual(1, leave_count);
    try std.testing.expectEqual(3, call_count);
}

test "setparam and call can share R0 register" {
    const allocator = std.testing.allocator;

    const machine = try VM.init(allocator);
    defer machine.deinit(allocator);

    // Code that generates setparam(0) followed by call
    const code =
        \\def test(a)
        \\  a + 1
        \\end
        \\
        \\result = test(42)
    ;

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();
    try cfg.compileUntil(.renamed);

    // Build register allocator - this should succeed without constraint conflicts
    const ra = try RegisterAllocator.allocateRegisters(allocator, cfg);
    defer ra.deinit();

    // Find the setparam(0) and call instructions
    var setparam_found = false;
    var call_found = false;

    var node = cfg.scope.insns.first;
    while (node) |insn_node| {
        const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", insn_node);
        const insn = &insn_list_node.data;

        switch (insn.*) {
            .setparam => |param| {
                if (param.index == 0) {
                    setparam_found = true;
                    // setparam(0) should be assigned to R0
                    try std.testing.expect(param.out.data == .physical_register);
                    try std.testing.expectEqual(0, param.out.data.physical_register.register);
                }
            },
            .call => |call| {
                call_found = true;
                // call return value should be assigned to R0
                try std.testing.expect(call.out.data == .physical_register);
                try std.testing.expectEqual(0, call.out.data.physical_register.register);
            },
            else => {},
        }

        node = insn_node.next;
    }

    // Both setparam(0) and call should have been found
    try std.testing.expect(setparam_found);
    try std.testing.expect(call_found);
}
