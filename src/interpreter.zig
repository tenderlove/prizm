const std = @import("std");
const ir = @import("ir.zig");
const Scope = @import("scope.zig").Scope;
const CFG = @import("cfg.zig").CFG;
const BasicBlock = @import("basic_block.zig").BasicBlock;
const Insn = ir.InstructionListNode;

pub const Interpreter = struct {
    pub const RuntimeValue = union(enum) {
        nil,
        bool_: bool,
        int: i64,
        str: []const u8,
    };

    pub const Error = error{
        NotImplemented,
        UnknownBuiltin,
        TypeMismatch,
        OutOfMemory,
    };

    pub const Frame = struct {
        alloc: std.mem.Allocator,
        env: []?RuntimeValue,
        current: *BasicBlock,
        prev: ?*BasicBlock = null,

        pub fn init(alloc: std.mem.Allocator, c: *CFG) !Frame {
            const cap = maxInsnId(c) + 1;
            const env = try alloc.alloc(?RuntimeValue, cap);
            @memset(env, null);
            return .{ .alloc = alloc, .env = env, .current = c.head };
        }

        pub fn deinit(self: *Frame) void {
            self.alloc.free(self.env);
        }
    };

    /// Run a CFG to completion. Returns the value passed to the `.leave`
    /// that terminated execution. For now `self` is nil and there are no
    /// other parameters — enough for top-level expression tests.
    pub fn run(alloc: std.mem.Allocator, c: *CFG) Error!RuntimeValue {
        var frame = try Frame.init(alloc, c);
        defer frame.deinit();

        dispatch: while (true) {
            var it = frame.current.instructionIter(.{});
            while (it.next()) |insn| {
                // Aliased phis are dead — nothing points at them post-resolve.
                if (insn.alias != null) continue;

                switch (insn.data) {
                    .loadi => |x| frame.env[insn.id] = .{ .int = @intCast(x.val) },
                    .loadnil => frame.env[insn.id] = .nil,
                    .loadtrue => frame.env[insn.id] = .{ .bool_ = true },
                    .loadfalse => frame.env[insn.id] = .{ .bool_ = false },
                    .loadstr => |x| frame.env[insn.id] = .{ .str = x.val },
                    .getparam => |x| {
                        // Only self (index 0) is supported yet, and we're
                        // always the top-level frame so it's nil.
                        if (x.index != 0) return Error.NotImplemented;
                        frame.env[insn.id] = .nil;
                    },
                    .tst => |x| {
                        frame.env[insn.id] = .{ .bool_ = isTruthy(frame.env[x.in.id].?) };
                    },
                    .phi => |x| {
                        const idx = predIndex(frame.current, frame.prev.?);
                        frame.env[insn.id] = frame.env[x.params.items[idx].id].?;
                    },
                    .call => |x| {
                        frame.env[insn.id] = try callBuiltin(&frame, x.recv, x.name, x.params.items);
                    },
                    .define_method => return Error.NotImplemented,
                    .jump => |x| {
                        frame.prev = frame.current;
                        frame.current = x.target;
                        continue :dispatch;
                    },
                    .cond => |x| {
                        const took_true = isTruthy(frame.env[x.condition.id].?);
                        frame.prev = frame.current;
                        frame.current = if (took_true) x.truthy else x.falsy;
                        continue :dispatch;
                    },
                    .leave => |x| return frame.env[x.in.id].?,
                }
            }
            // A well-formed block always terminates via jump/cond/leave —
            // that dispatches (continue) or returns. Falling out of the
            // inner loop means we walked past a missing terminator.
            unreachable;
        }
    }

    fn maxInsnId(c: *CFG) usize {
        var max: usize = 0;
        for (c.blocks) |b| {
            var it = b.instructionIter(.{});
            while (it.next()) |i| max = @max(max, i.id);
        }
        return max;
    }

    fn isTruthy(v: RuntimeValue) bool {
        return switch (v) {
            .nil => false,
            .bool_ => |b| b,
            else => true,
        };
    }

    fn predIndex(block: *BasicBlock, prev: *BasicBlock) usize {
        for (block.predecessors.items, 0..) |p, i| {
            if (p == prev) return i;
        }
        unreachable;
    }

    fn callBuiltin(frame: *Frame, recv: *Insn, name: []const u8, params: []const *Insn) Error!RuntimeValue {
        const r = frame.env[recv.id].?;
        // Single-char binary ops: + - * / < >.
        // Multi-char ops (== <= >= != <<) come later.
        if (name.len == 1 and params.len == 1) {
            const arg = frame.env[params[0].id].?;
            if (r != .int or arg != .int) return Error.TypeMismatch;
            return switch (name[0]) {
                '+' => .{ .int = r.int + arg.int },
                '-' => .{ .int = r.int - arg.int },
                '*' => .{ .int = r.int * arg.int },
                '/' => .{ .int = @divTrunc(r.int, arg.int) },
                '<' => .{ .bool_ = r.int < arg.int },
                '>' => .{ .bool_ = r.int > arg.int },
                else => Error.UnknownBuiltin,
            };
        }
        return Error.UnknownBuiltin;
    }
};

// ---- tests ----

const Globals = @import("globals.zig").Globals;
const compileString = @import("compiler.zig").compileString;

fn evalString(alloc: std.mem.Allocator, code: []const u8) !Interpreter.RuntimeValue {
    const globals = try Globals.init(alloc);
    defer globals.deinit(alloc);
    const c = try compileString(alloc, globals, code);
    defer c.deinit();
    return try Interpreter.run(alloc, c);
}

test "integer literal" {
    try std.testing.expectEqual(42, (try evalString(std.testing.allocator, "42")).int);
}

test "addition" {
    try std.testing.expectEqual(12, (try evalString(std.testing.allocator, "5 + 7")).int);
}

test "chained arithmetic" {
    try std.testing.expectEqual(20, (try evalString(std.testing.allocator, "3 * 8 - 4")).int);
}

test "less-than" {
    try std.testing.expectEqual(true, (try evalString(std.testing.allocator, "3 < 5")).bool_);
    try std.testing.expectEqual(false, (try evalString(std.testing.allocator, "9 < 5")).bool_);
}

test "if true branch" {
    try std.testing.expectEqual(1, (try evalString(std.testing.allocator, "if 3 < 5 then 1 else 2 end")).int);
}

test "if false branch" {
    try std.testing.expectEqual(2, (try evalString(std.testing.allocator, "a = 5; if 3 > a then 1 else 2 end")).int);
}

test "increment" {
    try std.testing.expectEqual(6, (try evalString(std.testing.allocator, "a = 5; a += 1; a")).int);
}

test "decrement" {
    try std.testing.expectEqual(4, (try evalString(std.testing.allocator, "a = 5; a -= 1; a")).int);
}

test "mult" {
    try std.testing.expectEqual(10, (try evalString(std.testing.allocator, "a = 5; a *= 2; a")).int);
}

test "div" {
    try std.testing.expectEqual(2, (try evalString(std.testing.allocator, "a = 4; a /= 2; a")).int);
}
