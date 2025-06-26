const std = @import("std");
const CFG = @import("cfg.zig").CFG;
const BasicBlock = @import("cfg.zig").BasicBlock;
const ir = @import("ir.zig");
const Var = ir.Variable;
const BitMap = std.DynamicBitSetUnmanaged;
const assert = @import("std").debug.assert;
const UnionFind = @import("union_find.zig").UnionFind;
const IRPrinter = @import("printer.zig").IRPrinter;

pub const RegisterAllocator = struct {
    allocator: std.mem.Allocator,
    cfg: *CFG,
    union_find: UnionFind,
    
    pub fn init(allocator: std.mem.Allocator, cfg: *CFG) !RegisterAllocator {
        const var_count = cfg.scope.varCount();
        return RegisterAllocator{
            .allocator = allocator,
            .cfg = cfg,
            .union_find = try UnionFind.init(allocator, var_count),
        };
    }
    
    pub fn deinit(self: *RegisterAllocator) void {
        self.union_find.deinit();
    }
    
    // Register allocation using union-find algorithm:
    // 1. Each SSA name starts in its own set
    // 2. Visit each phi function and union parameters with result  
    // 3. Each remaining unique set becomes a LiveRange
    pub fn allocate(self: *RegisterAllocator) !void {
        // Ensure CFG is in the right state (after SSA renaming, before phi isolation)
        if (self.cfg.state != .renamed) {
            std.debug.print("Register allocation requires CFG to be in 'renamed' state\n", .{});
            return;
        }

        // Reanalyze so we get the right liveout sets
        try self.cfg.analyze();
        
        std.debug.print("Starting register allocation with {d} variables...\n", .{self.cfg.opndCount()});
        
        // Step 1: Each SSA name starts in its own set (already done in UnionFind.init)
        
        // Step 2: Visit each phi function and union parameters with result
        self.processPhiFunctions();
        
        // Step 3: Build live ranges from union-find groups
        // Map from union-find representative to live range index
        var range_map = std.AutoHashMap(usize, *Var).init(self.allocator);
        defer range_map.deinit();
        try self.buildLiveRangesFromUnionFind(&range_map);

        // Rewrite the code with Live Ranges
        self.rewriteWithLiveRanges(range_map);
    }
    
    // Step 2: Process phi functions to union related variables
    fn processPhiFunctions(self: *RegisterAllocator) void {
        for (self.cfg.blocks) |bb| {
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
                            self.union_find.unite(phi_out, input_var.id);
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
    fn buildLiveRangesFromUnionFind(self: *RegisterAllocator, range_map: anytype) !void {
        const varcount = self.cfg.scope.varCount();
        const scope = self.cfg.scope;

        // Process each variable and assign it to a live range
        for (self.cfg.blocks) |block| {
            assert(block.reachable);
            var iter = block.killed_set.iterator(.{});
            while (iter.next()) |var_id| {
                const root = self.union_find.find(var_id);

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

    fn rewriteWithLiveRanges(self: *RegisterAllocator, range_map: anytype) void {
        // Iterate through all instructions in the scope and replace variables with live ranges
        var node = self.cfg.scope.insns.first;

        while (node) |insn_node| {
            const insn_list_node: *ir.InstructionListNode = @fieldParentPtr("node", insn_node);
            const insn = &insn_list_node.data;

            // Replace output variable if it exists
            if (insn.getOut()) |out_var| {
                const root = self.union_find.find(out_var.id);
                if (range_map.get(root)) |live_range_var| {
                    insn.setOut(live_range_var);
                }
            }

            // Replace input operands
            var op_iter = insn.opIter();
            while (op_iter.next()) |operand| {
                const root = self.union_find.find(operand.id);
                if (range_map.get(root)) |live_range_var| {
                    insn.replaceOpnd(operand, live_range_var);
                }
            }
            
            node = insn_node.next;
        }
    }
};
