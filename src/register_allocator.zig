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

    const K = @typeInfo(PhysicalRegister).@"enum".fields.len;

    pub fn init(graph: *InterferenceGraph, lr_list: LiveRangeList) ChaitinAllocator {
        return ChaitinAllocator{
            .graph = graph,
            .lr_list = lr_list,
        };
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

        var stack = LiveRangeList.init(allocator);

        var selected = try BitMap.initFull(allocator, work_list.items.len);
        defer selected.deinit(allocator);

        var trial: usize = 0;

        while (selected.count() != 0) {
            std.debug.print("trial: {d}\n", .{trial});
            var iter = selected.iterator(.{});
            var trivial = LiveRangeList.init(allocator);
            defer trivial.deinit();
            while (iter.next()) |idx| {
                const lr = work_list.items[idx];
                if (graph.degree(lr.getLocalId()) < 4) {
                    std.debug.print("hello d: {d}\n", .{graph.degree(lr.getLocalId())});
                    selected.unset(idx);
                    try trivial.append(lr);
                }
            }

            for (trivial.items) |lr| {
                try stack.append(lr);
                graph.removeNode(lr.getLocalId());
            }

            if (trivial.items.len == 0 and selected.count() != 0) {
                // TODO: figure out how to calculate spill cost
                return error.CannotAllocate;
            }
            trial += 1;
        }

        return stack;
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

            // Pick first available color
            const color: PhysicalRegister = for (0..K) |reg_num| {
                if (!forbidden.isSet(reg_num)) {
                    break @enumFromInt(reg_num);
                }
            } else return error.OutOfRegisters;

            // Get the static Variable for this physical register
            const reg_var = getPhysicalRegisterVar(color);
            register_mapping.items[lr_id] = reg_var;
            assigned.set(lr_id);
        }

        return register_mapping;
    }

    // Static array of physical register variables
    const physical_registers: [K]Var = blk: {
        var regs: [K]Var = undefined;
        for (0..K) |i| {
            regs[i] = Var{
                .id = std.math.maxInt(usize), // We should never look these up by ID
                .data = .{ .physical_register = .{ .id = i, .register = i } },
            };
        }
        break :blk regs;
    };

    fn getPhysicalRegisterVar(reg: PhysicalRegister) *const Var {
        return &physical_registers[@intFromEnum(reg)];
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
        rewriteWithLiveRanges(cfg, &union_find, &range_map);

        // We have to re-analyze the CFG after updating the code with LR
        try cfg.analyze();

        // Build an interference graph
        const interference_graph = try InterferenceGraph.init(allocator, cfg.scope.liveRangeCount());
        errdefer interference_graph.deinit(allocator);
        try buildInterferenceGraph(allocator, cfg, interference_graph);

        // Build a list of live ranges from the LiveRangeMap
        var lr_list = std.ArrayList(*Var).init(allocator);
        defer lr_list.deinit();

        var value_iter = range_map.valueIterator();
        while (value_iter.next()) |value_ptr| {
            try lr_list.append(value_ptr.*);
        }

        // Create register mapping
        var alloc = ChaitinAllocator.init(interference_graph, lr_list);
        errdefer alloc.deinit(allocator);

        var register_mapping = try alloc.allocate(allocator);
        errdefer register_mapping.deinit();

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

                    live_now.unset(n.getLocalId());
                    var live_iter = live_now.iterator(.{});
                    while (live_iter.next()) |lr_id| {
                        const from_id = cfg.scope.getVariableById(lr_id).getLocalId();
                        graph.add(from_id, n.getLocalId());
                    }
                }

                var opiter = insn.data.opIter();
                while (opiter.next()) |op| {
                    assert(op.data == .live_range);

                    live_now.set(op.getLocalId());
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

    fn rewriteWithLiveRanges(cfg: *CFG, union_find: *UnionFind, range_map: *std.AutoHashMap(usize, *Var)) void {
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
                }
            }

            // Replace input operands
            var op_iter = insn.opIter();
            while (op_iter.next()) |operand| {
                const root = union_find.find(operand.id);
                if (range_map.get(root)) |live_range_var| {
                    insn.replaceOpnd(operand, live_range_var);
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
        \\ puts x + y
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
