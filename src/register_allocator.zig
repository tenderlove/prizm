const std = @import("std");
const CFG = @import("cfg.zig").CFG;
const BasicBlock = @import("cfg.zig").BasicBlock;
const ir = @import("ir.zig");
const Var = ir.Variable;
const BitMap = std.DynamicBitSetUnmanaged;
const assert = @import("std").debug.assert;
const UnionFind = @import("union_find.zig").UnionFind;
const IRPrinter = @import("printer.zig").IRPrinter;
const InterferenceGraph = @import("interference_graph.zig").InterferenceGraph;

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

pub const RegisterMapping = std.AutoHashMap(usize, PhysicalRegister);

pub const RegisterAllocator = struct {
    allocator: std.mem.Allocator,
    cfg: *CFG,
    union_find: UnionFind,
    range_map: std.AutoHashMap(usize, *Var),
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
        var range_map = std.AutoHashMap(usize, *Var).init(allocator);
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

        // Create register mapping
        const register_mapping = RegisterMapping.init(allocator);

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
                if (insn.data.outVar()) |n| {
                    live_now.unset(n.id);
                    var live_iter = live_now.iterator(.{});
                    while (live_iter.next()) |lr_id| {
                        graph.add(lr_id, n.id);
                    }
                }

                var opiter = insn.data.opIter();
                while (opiter.next()) |op| {
                    live_now.set(op.id);
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
