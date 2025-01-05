const std = @import("std");
const cfg_zig = @import("../cfg.zig");
const ir = @import("../ir.zig");
const CFG = cfg_zig.CFG;
const BasicBlock = cfg_zig.BasicBlock;
const Op = ir.Operand;
const vm = @import("../vm.zig");
const cmp = @import("../compiler.zig");
const bitmatrix = @import("../utils/bitmatrix.zig");
const BitMatrix = bitmatrix.BitMatrix;
const bitmap = @import("../utils/bitmap.zig");
const BitMap = bitmap.BitMap;

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
    fn isolatePhi(self: *SSADestructor, cfg: *CFG, primed_insns: *std.ArrayList(*ir.Instruction), copy_groups: *std.ArrayList(*ir.InstructionList.Node)) !void {
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

            // Insert parallel copies immediately after the Phi nodes so
            // that we can isolate them from the rest of the instructions
            if (isolation_copies.items.len > 0) {
                // The insn we're on should be the one right after the last Phi.
                std.debug.assert(iter != null);
                std.debug.assert(!iter.?.data.isPhi());
                std.debug.assert(iter.?.prev.?.data.isPhi());

                var insn = iter.?.prev.?; // Should be the last Phi in the block

                for (isolation_copies.items) |copy| {
                    insn = try bb.insertParallelCopy(cfg.scope, insn, copy.original, copy.prime, self.group);
                    try copy_groups.append(insn);
                    try primed_insns.append(&insn.data);
                }
                self.group += 1;
            }

            if (predecessor_copies.items.len > 0) {
                std.mem.sort(ParallelCopy, predecessor_copies.items, {}, ParallelCopy.cmpDest);
                var current_block = predecessor_copies.items[0].dest_block.name;

                for (predecessor_copies.items) |copy| {
                    if (copy.dest_block.name != current_block) {
                        current_block = copy.dest_block.name;
                        self.group += 1;
                    }
                    const insn = try copy.dest_block.appendParallelCopy(cfg.scope, copy.prime, copy.original, self.group);
                    try primed_insns.append(&insn.data);
                    try copy_groups.append(insn);
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

    fn insertPhiCopies(self: *SSADestructor, cfg: *CFG, primed_insns: *std.ArrayList(*ir.Instruction), copy_groups: *std.ArrayList(*ir.InstructionList.Node)) !void {
        var phi_copies = std.ArrayList(ParallelCopy).init(self.mem);
        defer phi_copies.deinit();

        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

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
                    },
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == bb.finish) break;
                iter = insn.next;
            }
        }

        if (phi_copies.items.len > 0) {
            std.mem.sort(ParallelCopy, phi_copies.items, {}, ParallelCopy.cmpDest);
            var current_block = phi_copies.items[0].dest_block.name;

            for (phi_copies.items) |copy| {
                if (copy.dest_block.name != current_block) {
                    current_block = copy.dest_block.name;
                    self.group += 1;
                }
                const pcopy = try copy.dest_block.appendParallelCopy(cfg.scope, copy.prime, copy.original, self.group);
                try primed_insns.append(&pcopy.data);
                try copy_groups.append(pcopy);
            }
            self.group += 1;
        }
    }

    // Eliminate phi functions. Figure 9.20 part e
    fn eliminatePhi(_: *SSADestructor, cfg: *CFG) !void {
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            var iter: ?*ir.InstructionList.Node = bb.start;

            while (iter) |insn| {
                switch(insn.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => cfg.scope.insns.remove(insn),
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == bb.finish) break;
                iter = insn.next;
            }
        }
    }

    fn walkChildren(self: *SSADestructor, graph: *BitMatrix, visited: *BitMap, seen: *BitMap, node: usize) !void {
        if (seen.isSet(node)) {
            return error.NotImplementedError;
        }

        try seen.set(node);
        try visited.set(node);

        const children = graph.getColumn(node);
        var iter = children.iter();
        while (iter.next()) |child| {
            // If we've already seen this subgraph, just return. We know there
            // aren't any cycles in the subgraph.
            try self.walkChildren(graph, visited, seen, child);
        }
    }

    fn checkForCycles(self: *SSADestructor, cfg: *CFG, matrix: *BitMatrix) !void {
        // We need to keep track of the nodes we've visited
        const visited = try BitMap.init(self.mem, cfg.opndCount());
        defer visited.deinit(self.mem);

        var itr = matrix.iter();
        // For each variable in our matrix
        while (itr.next()) |point| {
            // The y value depends on the x value.  For example
            // `a <- b`, "b" will be the x value and we can get all
            // variables that depend on b by asking for the column (y values)
            // If we've visited this variable already, we can skip it
            if (visited.isSet(point.x)) continue;

            try visited.set(point.x);

            // TODO: we should allocate this once and reuse it but we need
            // a "clear" method on bit maps.
            const seen = try BitMap.init(self.mem, cfg.opndCount());
            defer seen.deinit(self.mem);

            // Otherwise, we need to recursively walk its edges
            try self.walkChildren(matrix, visited, seen, point.x);
        }
    }

    fn serializeCopyGroup(_: *SSADestructor, cfg: *CFG, insn: *ir.InstructionList.Node, group: usize) !void {
        // There shouldn't be any cycles, so we should be able to just insert
        // copies.
        var cursor = insn;
        while (cursor.data.pmov.group == group) {
            const src = cursor.data.pmov.in;
            const dst = cursor.data.pmov.out;
            const old = cursor;

            cursor = cursor.next.?;

            const copy = try cfg.makeMov(dst, src);
            cfg.replace(old.data.pmov.block, old, copy);
            if (@as(ir.InstructionName, cursor.data) != ir.InstructionName.pmov) break;
        }
    }

    fn serializeCopyGroups(self: *SSADestructor, cfg: *CFG, copy_groups: *std.ArrayList(*ir.InstructionList.Node)) !void {
        const bits = cfg.opndCount();
        const matrix = try BitMatrix.init(self.mem, bits, bits);
        defer matrix.deinit(self.mem);

        var current_group: usize = 0;
        var start_of_group = copy_groups.items[0];

        for (copy_groups.items) |pcopy| {
            const src = pcopy.data.pmov.in;
            const dst = pcopy.data.pmov.out;
            matrix.set(src.getID(), dst.getID());
            if (pcopy.data.pmov.group != current_group) {
                try self.checkForCycles(cfg, matrix);
                try self.serializeCopyGroup(cfg, start_of_group, current_group);
                matrix.clear();
                start_of_group = pcopy;
                current_group = pcopy.data.pmov.group;
                std.debug.print("reset\n", .{});
            }
            std.debug.print("group: {d}\n", .{ pcopy.data.pmov.group });
        }

        if (matrix.popCount() > 0) {
            try self.checkForCycles(cfg, matrix);
            try self.serializeCopyGroup(cfg, start_of_group, current_group);
        }
    }

    pub fn destruct(self: *SSADestructor, cfg: *CFG) !void {
        // A list of instructions we need to replace prime variables
        var primed_insns = std.ArrayList(*ir.Instruction).init(self.mem);
        defer primed_insns.deinit();

        var copy_groups = std.ArrayList(*ir.InstructionList.Node).init(self.mem);
        defer copy_groups.deinit();

        // Figure 9.20, part C
        try self.isolatePhi(cfg, &primed_insns, &copy_groups);
        // Figure 9.20, part D
        // Figure 9.20, part E
        try self.insertPhiCopies(cfg, &primed_insns, &copy_groups);

        try self.renameAllVariables(cfg, &primed_insns);
        // Figure 9.20, part F & G
        try self.serializeCopyGroups(cfg, &copy_groups);

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

test "phi isolation adds I/O variable copies" {
    const allocator = std.testing.allocator;

    const code =
\\ x = 10
\\ y = 11
\\ begin
\\   t = x
\\   x = y
\\   y = t
\\ end while i < 100
\\ 
\\ p x
\\ p y
;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try cfg_zig.makeCFG(allocator, scope);
    defer cfg.deinit();

    var destructor = SSADestructor { .mem = allocator };

    try cfg.placePhis();
    try cfg.rename();

    // A list of instructions we need to replace prime variables
    var primed_insns = std.ArrayList(*ir.Instruction).init(allocator);
    defer primed_insns.deinit();

    var copy_groups = std.ArrayList(*ir.InstructionList.Node).init(allocator);
    defer copy_groups.deinit();

    try destructor.isolatePhi(cfg, &primed_insns, &copy_groups);

    // After we isolate phi, there should be copy instructions at the end of
    // BB0 and BB1 to isolate the phi parameters, then there should be
    // instructions after the phi instructions to isolate the phi return value

    // Get the 2 phi instructions from BB1
    const phi1 = cfg.blocks[1].start.next.?;
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi1.data));

    const phi2 = phi1.next.?;
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi2.data));

    // Lets test that the Phi return values are isolated first
    const isolation1 = phi2.next.?;
    try expectIsolatedPhiOutput(phi1.data, isolation1.data);

    const isolation2 = isolation1.next.?;
    try expectIsolatedPhiOutput(phi2.data, isolation2.data);

    // Now lets test that the input values are isolated.
    const bb0 = cfg.blocks[0];
    try expectIsolatedPhiInput(bb0, &[_] ir.Instruction { phi1.data, phi2.data });
    try expectIsolatedPhiInput(cfg.blocks[1], &[_] ir.Instruction { phi1.data, phi2.data });
}

fn expectIsolatedPhiInput(bb: *BasicBlock, phis: []const ir.Instruction) !void {
    var insn = bb.finishInsn();
    if (insn.?.data.isJump()) insn = insn.?.prev;

    while (true) {
        if (insn == bb.start) { // If we hit this start, then there were no pmov
            try std.testing.expect(false);
        }

        if (!insn.?.prev.?.data.isPMov()) {
            break;
        } else {
            insn = insn.?.prev;
        }
    }

    var count: usize = 0;
    while(true) {
        if (insn == bb.finish or !insn.?.data.isPMov()) {
            break;
        }

        try std.testing.expect(insn != null);
        if (insn) |i| {
            // Make sure it's actually a parallel mov
            const isolation_insn = i.data;
            try std.testing.expectEqual(ir.InstructionName.pmov, @as(ir.InstructionName, isolation_insn));
            // The input of the isolation copy should be the pre-primed phi input (the SSA name)
            try std.testing.expectEqual(isolation_insn.pmov.out.prime.orig, isolation_insn.pmov.in);
            // The output of this isolation copy should be one of the inputs of the phi
            const phiinsn = phis[count];
            var found = false;
            for (phiinsn.phi.params.items) |input| {
                if (input == isolation_insn.pmov.out) {
                    found = true;
                    break;
                }
            }
            try std.testing.expect(found);
        }
        insn = insn.?.next;
        count += 1;
    }
}

fn expectIsolatedPhiOutput(phi: ir.Instruction, isolation_insn: ir.Instruction) !void {
    // Make sure it's actually a parallel mov
    try std.testing.expectEqual(ir.InstructionName.pmov, @as(ir.InstructionName, isolation_insn));
    // The output of the phi should be the input of the isolation copy
    try std.testing.expectEqual(phi.getOut(), isolation_insn.pmov.in);
    // The output of the isolation copy should be the pre-primed phi output (the SSA name)
    try std.testing.expectEqual(isolation_insn.pmov.in.prime.orig, isolation_insn.pmov.out);
    try std.testing.expectEqual(ir.OperandType.redef, @as(ir.OperandType, isolation_insn.pmov.out.*));
}

test "destructor fixes all variables" {
    const allocator = std.testing.allocator;

    const code =
\\ x = 10
\\ y = 11
\\ begin
\\   t = x
\\   x = y
\\   y = t
\\ end while i < 100
\\ 
\\ p x
\\ p y
;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try cfg_zig.makeCFG(allocator, scope);
    defer cfg.deinit();

    try cfg.placePhis();
    try cfg.rename();
    try cfg.destructSSA();

    // After SSA destruction, we shouldn't have any prime operands, or renamed operands
    for (cfg.blocks) |block| {
        if (!block.reachable) continue;

        var iter = block.startInsn();
        while (iter) |insn| {
            if (iter == block.finishInsn()) break;

            if (insn.data.outVar()) |ov| {
                try std.testing.expect(ir.OperandType.redef != @as(ir.OperandType, ov.*));
                try std.testing.expect(ir.OperandType.prime != @as(ir.OperandType, ov.*));
            }

            var opitr = insn.data.opIter();
            while (opitr.next()) |op| {
                try std.testing.expect(ir.OperandType.redef != @as(ir.OperandType, op.*));
                try std.testing.expect(ir.OperandType.prime != @as(ir.OperandType, op.*));
            }

            iter = insn.next;
        }
    }
}

test "cycle in parallel copy" {
    const allocator = std.testing.allocator;

    const code =
\\ x = 10
\\ y = 11
\\ begin
\\   t = x
\\   x = y
\\   y = t
\\ end while i < 100
\\ 
\\ p x
\\ p y
;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try cfg_zig.makeCFG(allocator, scope);
    defer cfg.deinit();

    try cfg.placePhis();
    try cfg.rename();

    const bits = cfg.opndCount();
    const matrix = try BitMatrix.init(allocator, bits, bits);
    defer matrix.deinit(allocator);

    // We just want to test that the SSA destructor will blow up when
    // it finds a cyclic parallel copy.  First we'll set up a non-cyclic
    // relationship, then set up a cyclic one and make sure it blows up.
    // The numbers below don't map to actual variables, I'm just using them
    // like this because it's easier to think about names than numbers.
    // a: 0, b: 1, c: 2, d: 3
    // b -> a
    // b -> d
    // c -> b
    matrix.set(1, 0);
    matrix.set(1, 3);
    matrix.set(2, 1);

    var destructor = SSADestructor { .mem = allocator };
    try destructor.checkForCycles(cfg, matrix);

    // now lets introduce a cycle and make sure we get an error
    // e: 4, f: 5, g: 6, h: 7
    // e -> f
    // f -> g
    // g -> h
    // h -> e
    matrix.set(4, 5);
    matrix.set(5, 6);
    matrix.set(6, 7);
    matrix.set(7, 4);

    try std.testing.expectError(error.NotImplementedError, destructor.checkForCycles(cfg, matrix));
}

test "destruction removes all parallel copies" {
    const allocator = std.testing.allocator;

    const code =
\\ x = 10
\\ y = 11
\\ begin
\\   t = x
\\   x = y
\\   y = t
\\ end while i < 100
\\ 
\\ p x
\\ p y
;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try cfg_zig.makeCFG(allocator, scope);
    defer cfg.deinit();

    try cfg.placePhis();
    try cfg.rename();
    try cfg.destructSSA();

    // After SSA destruction, we shouldn't have any prime operands, or renamed operands
    for (cfg.blocks) |block| {
        if (!block.reachable) continue;

        var iter = block.startInsn();
        while (iter) |insn| {
            if (iter == block.finishInsn()) break;
            try std.testing.expect(ir.InstructionName.pmov != @as(ir.InstructionName, insn.data));

            iter = insn.next;
        }
    }
}

test "destruction maintains block endings" {
    const allocator = std.testing.allocator;

    const code =
\\ x = 10
\\ y = 11
\\ begin
\\   t = x
\\   x = y
\\   y = t
\\ end while i < 100
\\ 
\\ p x
\\ p y
;

    const machine = try vm.init(allocator);
    defer machine.deinit(allocator);

    const scope = try cmp.compileString(allocator, machine, code);
    defer scope.deinit();

    const cfg = try cfg_zig.makeCFG(allocator, scope);
    defer cfg.deinit();

    try cfg.placePhis();
    try cfg.rename();
    try cfg.destructSSA();

    // After SSA destruction, we shouldn't have any prime operands, or renamed operands
    for (cfg.blocks) |block| {
        if (!block.reachable) continue;

        var iter = block.startInsn();
        while (iter) |insn| {
            if (iter == block.finishInsn()) break;
            // putlabel should always be first
            if (insn.data.isLabel()) {
                try std.testing.expectEqual(block.startInsn(), insn);
            }

            // Jumps and return should always be last
            if (insn.data.isJump() or insn.data.isReturn()) {
                try std.testing.expectEqual(block.finishInsn(), insn);
            }

            iter = insn.next;
        }
    }
}
