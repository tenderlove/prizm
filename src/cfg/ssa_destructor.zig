const std = @import("std");
const cfg_zig = @import("../cfg.zig");
const ir = @import("../ir.zig");
const CFG = cfg_zig.CFG;
const BasicBlock = cfg_zig.BasicBlock;
const Op = ir.Operand;

pub const SSADestructor = struct {
    mem: std.mem.Allocator,
    group: usize = 0,

    const ParallelCopy = struct {
        source_block: *BasicBlock,
        dest_block: *BasicBlock,
        prime: *Op,
        original: *Op,

        fn cmpDest(_: void, a: ParallelCopy, b: ParallelCopy) bool {
            return a.dest_block.name < b.dest_block.name;
        }
    };

    // Isolate Phi functions by placing parallel copies immediately after the
    // Phi functions, and also in predecessor blocks.  Figure 9.20, part C
    // (part A and B are already done)
    fn isolatePhi(self: *SSADestructor, cfg: *CFG, primed_insns: *std.ArrayList(*ir.Instruction)) !void {
        var copy_groups = std.ArrayList(ParallelCopy).init(self.mem);
        defer copy_groups.deinit();

        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            var predecessor_copies = std.ArrayList(ParallelCopy).init(self.mem);
            defer predecessor_copies.deinit();

            var isolation_copies = std.ArrayList(ParallelCopy).init(self.mem);
            defer isolation_copies.deinit();

            var iter: ?*ir.InstructionList.Node = bb.start;

            while (iter) |insn| {
                switch(insn.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => |p| {
                        try primed_insns.append(&insn.data);
                        for (0..p.params.items.len) |param_i| {
                            const param = p.params.items[param_i];
                            const prime_in = try cfg.scope.newPrime(param);
                            p.params.items[param_i] = prime_in;

                            try predecessor_copies.append(.{
                                .source_block = bb,
                                .dest_block = cfg.blocks[param.redef.defblock.name],
                                .prime = prime_in,
                                .original = param
                            });
                        }

                        const current_out = insn.data.getOut().?;
                        const prime_out = try cfg.scope.newPrime(current_out);

                        try isolation_copies.append(.{
                            .source_block = bb,
                            .dest_block = bb,
                            .prime = prime_out,
                            .original = current_out,
                        });

                        insn.data.setOut(prime_out);
                    },
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == bb.finish) break;

                iter = insn.next;
            }

            if (isolation_copies.items.len > 0) {
                // The insn we're on should be the one right after the last Phi.
                std.debug.assert(iter != null);
                std.debug.assert(!iter.?.data.isPhi());
                std.debug.assert(iter.?.prev.?.data.isPhi());

                var insn = iter.?.prev.?; // Should be the last Phi in the block

                for (isolation_copies.items) |copy| {
                    insn = try cfg.scope.insertParallelCopy(insn, copy.original, copy.prime, self.group);
                    try primed_insns.append(&insn.data);
                }
                self.group += 1;
            }

            if (predecessor_copies.items.len > 0) {
                std.debug.print("copy len {d}\n", .{ predecessor_copies.items.len });
                std.mem.sort(ParallelCopy, predecessor_copies.items, {}, ParallelCopy.cmpDest);
                var current_block = predecessor_copies.items[0].dest_block.name;

                for (predecessor_copies.items) |copy| {
                    if (copy.dest_block.name != current_block) {
                        current_block = copy.dest_block.name;
                        self.group += 1;
                    }
                    const insn = try copy.dest_block.appendParallelCopy(cfg.scope, copy.prime, copy.original, self.group);
                    try primed_insns.append(&insn.data);
                    std.debug.print("dest {d} group {d}\n", .{ copy.dest_block.name, self.group });
                }
                self.group += 1;
            }
        }
    }

    // Now that Phi are isolated, we need to rename prime's to compiler
    // generated temps and remove subscript variables.  Figure 9.20, part d
    fn renameAllVariables(self: *SSADestructor, cfg: *CFG, primed_insns: *std.ArrayList(*ir.Instruction)) !void {
        // We need to map primes to new temp variables, so lets allocate
        // an array here then allocate new temps as we need them.
        const prime_map: []*Op = try self.mem.alloc(*Op, cfg.scope.primes);
        defer self.mem.free(prime_map);

        for (0..cfg.scope.primes) |i| {
            prime_map[i] = try cfg.scope.newTemp();
        }

        removePrimesAndAliases(primed_insns.items, prime_map);
        removePrimesAndAliases(cfg.insns_with_alias.?, prime_map);
    }

    // Eliminate phi functions. Figure 9.20 part e
    fn eliminatePhi(self: *SSADestructor, cfg: *CFG) !void {
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            var phi_copies = std.ArrayList(ParallelCopy).init(self.mem);
            defer phi_copies.deinit();

            var iter: ?*ir.InstructionList.Node = bb.start;

            while (iter) |insn| {
                switch(insn.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => |p| {
                        const dest = p.out;
                        for (p.params.items) |src| {
                            const def_block = src.getDefinitionBlock();
                            try phi_copies.append(.{
                                .source_block = bb,
                                .dest_block = def_block,
                                .prime = dest,
                                .original = src
                            });
                        }
                        cfg.scope.insns.remove(insn);
                    },
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == bb.finish) break;
                iter = insn.next;
            }

            if (phi_copies.items.len > 0) {
                std.mem.sort(ParallelCopy, phi_copies.items, {}, ParallelCopy.cmpDest);
                var current_block = phi_copies.items[0].dest_block.name;

                for (phi_copies.items) |copy| {
                    if (copy.dest_block.name != current_block) {
                        current_block = copy.dest_block.name;
                        self.group += 1;
                    }
                    _ = try copy.dest_block.appendParallelCopy(cfg.scope, copy.prime, copy.original, self.group);
                }
                self.group += 1;
            }
        }
    }

    pub fn destruct(self: *SSADestructor, cfg: *CFG) !void {
        // A list of instructions we need to replace prime variables
        var primed_insns = std.ArrayList(*ir.Instruction).init(self.mem);
        defer primed_insns.deinit();

        // Figure 9.20, part C
        try self.isolatePhi(cfg, &primed_insns);
        // Figure 9.20, part D
        try self.renameAllVariables(cfg, &primed_insns);
        // Figure 9.20, part E
        try self.eliminatePhi(cfg);
    }

    fn removePrimesAndAliases(insns: []*ir.Instruction, prime_map: []*Op) void {
        for (insns) |insn| {
            if (insn.getOut()) |out| {
                if (out.isPrime()) {
                    const tmp = prime_map[out.prime.prime_id];
                    tmp.setDefinitionBlock(out.getDefinitionBlock());
                    insn.setOut(tmp);
                }

                if (out.isRedef()) {
                    insn.setOut(out.redef.orig);
                }
            }

            var itr = insn.opIter();
            while (itr.next()) |op| {
                if (op.isPrime()) {
                    insn.replaceOpnd(op, prime_map[op.prime.prime_id]);
                }

                if (op.isRedef()) {
                    insn.replaceOpnd(op, op.redef.orig);
                }
            }
        }
    }

};
