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
const BitMap = std.DynamicBitSetUnmanaged;

pub const SSADestructor = struct {
    mem: std.mem.Allocator,
    group: usize = 0,
    phi_copies: ParallelCopyList,
    copy_groups: std.ArrayList(*ir.InstructionListNode),

    const ParallelCopy = struct {
        source_block: *BasicBlock,
        dest_block: *BasicBlock,
        output: *Op,
        input: *Op,

        fn cmpDest(_: void, a: ParallelCopy, b: ParallelCopy) bool {
            return a.dest_block.name < b.dest_block.name;
        }
    };

    const ParallelCopyList = std.ArrayList(ParallelCopy);

    pub fn init(mem: std.mem.Allocator) !*SSADestructor {
        const self = try mem.create(SSADestructor);
        self.* = SSADestructor {
            .mem = mem,
            .phi_copies = ParallelCopyList.init(mem),
            .copy_groups = std.ArrayList(*ir.InstructionListNode).init(mem),
        };
        return self;
    }

    pub fn deinit(self: *SSADestructor) void {
        self.phi_copies.deinit();
        self.copy_groups.deinit();
        self.mem.destroy(self);
    }

    // Isolate Phi functions by placing parallel copies immediately after the
    // Phi functions, and also in predecessor blocks.  Figure 9.20, part C
    // (part A and B are already done)
    // Figure 9.20, part C
    pub fn isolatePhi(self: *SSADestructor, cfg: *CFG) !void {
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            var predecessor_copies = std.ArrayList(ParallelCopy).init(self.mem);
            defer predecessor_copies.deinit();

            var isolation_copies = std.ArrayList(ParallelCopy).init(self.mem);
            defer isolation_copies.deinit();

            var iter: ?*std.DoublyLinkedList.Node = &bb.start.node;

            while (iter) |insn| {
                const insn_node: *ir.InstructionListNode = @fieldParentPtr("node", insn);

                switch(insn_node.data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => |p| {
                        const current_out = insn_node.data.getOut().?;
                        const prime_out = try cfg.scope.newPrime(current_out);

                        for (0..p.params.items.len) |param_i| {
                            const param = p.params.items[param_i];
                            const prime_in = try cfg.scope.newPrime(param);
                            p.params.items[param_i] = prime_in;

                            try predecessor_copies.append(.{
                                .source_block = bb,
                                .dest_block = cfg.blocks[param.redef.defblock.name],
                                .output = prime_in,
                                .input = param
                            });

                            const def_block = param.getDefinitionBlock();
                            try self.phi_copies.append(.{
                                .source_block = bb,
                                .dest_block = def_block,
                                .output = prime_out,
                                .input = prime_in
                            });
                        }

                        try isolation_copies.append(.{
                            .source_block = bb,
                            .dest_block = bb,
                            .output = prime_out,
                            .input = current_out,
                        });

                        insn_node.data.setOut(prime_out);
                    },
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == &bb.finish.node) break;

                iter = insn.next;
            }

            // Insert parallel copies immediately after the Phi nodes so
            // that we can isolate them from the rest of the instructions
            if (isolation_copies.items.len > 0) {
                // The insn we're on should be the one right after the last Phi.
                std.debug.assert(iter != null);
                std.debug.assert(!@as(*ir.InstructionListNode, @fieldParentPtr("node", iter.?)).data.isPhi());
                std.debug.assert(@as(*ir.InstructionListNode, @fieldParentPtr("node", iter.?.prev.?)).data.isPhi());

                var insn = iter.?.prev.?; // Should be the last Phi in the block

                for (isolation_copies.items) |copy| {
                    insn = &(try bb.insertParallelCopy(cfg.scope, @fieldParentPtr("node", insn), copy.input, copy.output, self.group)).node;
                    try self.copy_groups.append(@fieldParentPtr("node", insn));
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
                    const insn = try copy.dest_block.appendParallelCopy(cfg.scope, copy.output, copy.input, self.group);
                    try self.copy_groups.append(insn);
                }
                self.group += 1;
            }
        }
    }

    // Now that Phi are isolated, we need to rename prime's to compiler
    // generated temps and remove subscript variables.  Figure 9.20, part d
    // Figure 9.20, part D
    pub fn renameAllVariables(self: *SSADestructor, cfg: *CFG) !void {
        // We need to map primes to new temp variables, so lets allocate
        // an array here then allocate new temps as we need them.
        const prime_map: []*Op = try self.mem.alloc(*Op, cfg.scope.primes);
        defer self.mem.free(prime_map);

        for (0..cfg.scope.primes) |i| {
            prime_map[i] = try cfg.scope.newTemp();
        }

        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;
            removePrimesAndAliases(bb, prime_map);
        }
    }

    // Figure 9.20, part C
    pub fn insertPhiCopies(self: *SSADestructor, cfg: *CFG) !void {
        if (self.phi_copies.items.len > 0) {
            std.mem.sort(ParallelCopy, self.phi_copies.items, {}, ParallelCopy.cmpDest);
            var current_block = self.phi_copies.items[0].dest_block.name;

            for (self.phi_copies.items) |copy| {
                if (copy.dest_block.name != current_block) {
                    current_block = copy.dest_block.name;
                    self.group += 1;
                }
                const pcopy = try copy.dest_block.appendParallelCopy(cfg.scope, copy.output, copy.input, self.group);
                try self.copy_groups.append(pcopy);
            }
            self.group += 1;
        }
    }

    // Eliminate phi functions. Figure 9.20 part e
    // Figure 9.20, part E
    pub fn eliminatePhi(_: *SSADestructor, cfg: *CFG) !void {
        for (cfg.blocks) |bb| {
            if (!bb.reachable) continue;

            var iter: ?*std.DoublyLinkedList.Node = &bb.start.node;

            while (iter) |insn| {
                switch(@as(*ir.InstructionListNode, @fieldParentPtr("node", insn)).data) {
                    .putlabel => { }, // Skip putlabel
                    .phi => cfg.scope.insns.remove(insn),
                    // Quit after we've passed phi's
                    else => { break; }
                }

                if (insn == &bb.finish.node) break;
                iter = insn.next;
            }
        }
    }

    fn walkChildren(self: *SSADestructor, graph: *BitMatrix, visited: *BitMap, seen: *BitMap, node: usize) !void {
        if (seen.isSet(node)) {
            return error.NotImplementedError;
        }

        seen.set(node);
        visited.set(node);

        const children = graph.getColumn(node);
        var iter = children.iterator(.{});
        while (iter.next()) |child| {
            // If we've already seen this subgraph, just return. We know there
            // aren't any cycles in the subgraph.
            try self.walkChildren(graph, visited, seen, child);
        }
    }

    fn checkForCycles(self: *SSADestructor, cfg: *CFG, matrix: *BitMatrix) !void {
        // We need to keep track of the nodes we've visited
        var visited = try BitMap.initEmpty(self.mem, cfg.opndCount());
        defer visited.deinit(self.mem);

        for (matrix.buffer, 0..) |ys, x| {
            var y_itr = ys.iterator(.{});

            // For each variable in our matrix
            while (y_itr.next()) |_| {
                // The y value depends on the x value.  For example
                // `a <- b`, "b" will be the x value and we can get all
                // variables that depend on b by asking for the column (y values)
                // If we've visited this variable already, we can skip it
                if (visited.isSet(x)) continue;

                visited.set(x);

                // TODO: we should allocate this once and reuse it but we need
                // a "clear" method on bit maps.
                var seen = try BitMap.initEmpty(self.mem, cfg.opndCount());
                defer seen.deinit(self.mem);

                // Otherwise, we need to recursively walk its edges
                try self.walkChildren(matrix, &visited, &seen, x);
            }
        }
    }

    fn serializeCopyGroup(_: *SSADestructor, cfg: *CFG, insn: *ir.InstructionListNode, group: usize) !void {
        // There shouldn't be any cycles, so we should be able to just insert
        // copies.
        var cursor = insn;
        while (cursor.data.pmov.group == group) {
            const src = cursor.data.pmov.in;
            const dst = cursor.data.pmov.out;
            const old = cursor;

            cursor = @fieldParentPtr("node", cursor.node.next.?);

            const copy = try cfg.makeMov(dst, src);
            cfg.replace(old.data.pmov.block, old, copy);
            if (@as(ir.InstructionName, cursor.data) != ir.InstructionName.pmov) break;
        }
    }

    // Figure 9.20, part F & G
    pub fn serializeCopyGroups(self: *SSADestructor, cfg: *CFG) !void {
        const bits = cfg.opndCount();
        const matrix = try BitMatrix.init(self.mem, bits, bits);
        defer matrix.deinit(self.mem);

        var current_group: usize = 0;
        var start_of_group = self.copy_groups.items[0];

        for (self.copy_groups.items) |pcopy| {
            const src = pcopy.data.pmov.in;
            const dst = pcopy.data.pmov.out;
            matrix.set(src.getID(), dst.getID());
            if (pcopy.data.pmov.group != current_group) {
                try self.checkForCycles(cfg, matrix);
                try self.serializeCopyGroup(cfg, start_of_group, current_group);
                matrix.clear();
                start_of_group = pcopy;
                current_group = pcopy.data.pmov.group;
            }
        }

        if (matrix.count() > 0) {
            try self.checkForCycles(cfg, matrix);
            try self.serializeCopyGroup(cfg, start_of_group, current_group);
        }
    }

    fn removePrimesAndAliases(block: *BasicBlock, prime_map: []*Op) void {
        var iter = block.instructionIter();
        while (iter.next()) |insn_node| {
            var insn = &insn_node.data;

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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    var destructor = try SSADestructor.init(allocator);
    defer destructor.deinit();

    try cfg.compileUntil(.renamed);

    try destructor.isolatePhi(cfg);

    // After we isolate phi, there should be copy instructions at the end of
    // BB0 and BB1 to isolate the phi parameters, then there should be
    // instructions after the phi instructions to isolate the phi return value

    // Get the 2 phi instructions from BB1
    const phi1 = @as(*ir.InstructionListNode, @fieldParentPtr("node", cfg.blocks[1].start.node.next.?));
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi1.data));

    const phi2 = @as(*ir.InstructionListNode, @fieldParentPtr("node", phi1.node.next.?));
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi2.data));

    // Lets test that the Phi return values are isolated first
    const isolation1 = @as(*ir.InstructionListNode, @fieldParentPtr("node", phi2.node.next.?));
    try expectIsolatedPhiOutput(phi1.data, isolation1.data);

    const isolation2 = @as(*ir.InstructionListNode, @fieldParentPtr("node", isolation1.node.next.?));
    try expectIsolatedPhiOutput(phi2.data, isolation2.data);

    // Now lets test that the input values are isolated.
    const bb0 = cfg.blocks[0];
    try expectIsolatedPhiInput(bb0, &[_] ir.Instruction { phi1.data, phi2.data });
    try expectIsolatedPhiInput(cfg.blocks[1], &[_] ir.Instruction { phi1.data, phi2.data });
}

fn expectIsolatedPhiInput(bb: *BasicBlock, phis: []const ir.Instruction) !void {
    var insn = bb.finishInsn();
    if (insn.?.data.isJump()) {
        insn = @fieldParentPtr("node", insn.?.node.prev.?);
    }

    while (true) {
        if (insn == bb.start) { // If we hit this start, then there were no pmov
            try std.testing.expect(false);
        }

        if (!@as(*ir.InstructionListNode, @fieldParentPtr("node", insn.?.node.prev.?)).data.isPMov()) {
            break;
        } else {
            if (insn.?.node.prev) |m| {
                insn = @fieldParentPtr("node", m);
            } else {
                insn = null;
            }
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
            const found = for (phiinsn.phi.params.items) |input| {
                if (input == isolation_insn.pmov.out) break true;
            } else false;
            try std.testing.expect(found);
        }
        if (insn.?.node.next) |m| {
            insn = @fieldParentPtr("node", m);
        } else {
            insn = null;
        }
        count += 1;
    }
}

fn expectPhiOutputCopies(block: *BasicBlock, phis: []const ir.Instruction) !void {
    var insn = block.finishInsn();
    if (insn.?.data.isJump()) {
        insn = @fieldParentPtr("node", insn.?.node.prev.?);
    }

    for (0..(phis.len - 1)) |_| {
        insn = @fieldParentPtr("node", insn.?.node.prev.?);
    }

    for (phis) |phi_insn| {
        // We should have a parallel copy
        try std.testing.expect(insn.?.data.isPMov());
        // The output of the pmov should be the same as the output of the phi
        try std.testing.expectEqual(phi_insn.phi.out, insn.?.data.getOut());
        // The input should be one of the phi parameters
        const in = insn.?.data.pmov.in;
        const found = for (phi_insn.phi.params.items) |item| {
            if (item == in) break true;
        } else false;
        try std.testing.expect(found);
        if (insn.?.node.next) |x| {
            insn = @fieldParentPtr("node", x);
        } else {
            insn = null;
        }
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

test "inserting phi copies actually copies the right thing" {
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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    try cfg.compileUntil(.renamed);

    var destructor = try SSADestructor.init(allocator);
    defer destructor.deinit();

    try destructor.isolatePhi(cfg);
    try destructor.insertPhiCopies(cfg);

    // After we insert Phi Copies, there should be a copy in dominating
    // blocks to the Phi's output param.
    // In this case we have two Phi, x'1 and y'1, so there should be
    // a copy in BB0:
    //   * x'1 <- x'0
    //   * y'1 <- y'0
    //
    // Then at the end of BB1 (before the jump):
    //   * x'1 <- x'2
    //   * y'1 <- y'2
    //
    // Adding these copies make it no longer SSA (since we're copying to the Phi output twice)

    // Get the 2 phi instructions from BB1
    const phi1 = @as(*ir.InstructionListNode, @fieldParentPtr("node", cfg.blocks[1].start.node.next.?));
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi1.data));

    const phi2 = @as(*ir.InstructionListNode, @fieldParentPtr("node", phi1.node.next.?));
    try std.testing.expectEqual(ir.InstructionName.phi, @as(ir.InstructionName, phi2.data));

    try expectPhiOutputCopies(cfg.blocks[0], &[_] ir.Instruction { phi1.data, phi2.data });
    try expectPhiOutputCopies(cfg.blocks[1], &[_] ir.Instruction { phi1.data, phi2.data });
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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    try cfg.compileUntil(.renamed);

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

            if (insn.node.next) |n| {
                iter = @fieldParentPtr("node", n);
            } else {
                break;
            }
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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    try cfg.compileUntil(.renamed);

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

    var destructor = try SSADestructor.init(allocator);
    defer destructor.deinit();
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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    try cfg.compileUntil(.renamed);

    try cfg.destructSSA();

    // After SSA destruction, we shouldn't have any prime operands, or renamed operands
    for (cfg.blocks) |block| {
        if (!block.reachable) continue;

        var iter = block.startInsn();
        while (iter) |insn| {
            if (iter == block.finishInsn()) break;
            try std.testing.expect(ir.InstructionName.pmov != @as(ir.InstructionName, insn.data));

            if (insn.node.next) |n| {
                iter = @fieldParentPtr("node", n);
            } else {
                break;
            }
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

    const cfg = try CFG.build(allocator, scope);
    defer cfg.deinit();

    try cfg.compileUntil(.renamed);

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

            if (insn.node.next) |n| {
                iter = @fieldParentPtr("node", n);
            } else {
                break;
            }
        }
    }
}
