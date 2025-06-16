const std = @import("std");
const cmp = @import("compiler.zig");
const cfg = @import("cfg.zig");
const Scope = @import("scope.zig").Scope;
const BasicBlock = cfg.BasicBlock;

pub const InstructionList = std.DoublyLinkedList;

pub const VariableType = enum {
    local,
    temp,
    redef,
    prime,
};

pub const Variable = union(VariableType) {
    local: struct { name: usize, source_name: []const u8 },
    temp: struct {
        name: usize,
        defblock: ?*BasicBlock = null,
    },
    redef: struct { variant: usize, orig: *Operand, defblock: *BasicBlock },
    prime: struct { prime_id: usize, orig: *Operand },
};

pub const OperandType = enum {
    variable,
    immediate,
    label,
    scope,
    string,
};

pub const OperandData = union(OperandType) {
    variable: Variable,
    immediate: struct {
        value: u64,
    },
    label: struct {
        name: usize,
    },
    scope: struct {
        value: *Scope,
    },
    string: struct {
        value: []const u8,
    },
};

pub const Operand = struct {
    id: usize,
    data: OperandData,

    pub fn initImmediate(alloc: std.mem.Allocator, id: usize, value: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .immediate = .{ .value = value } } };
        return opnd;
    }

    pub fn initLabel(alloc: std.mem.Allocator, id: usize, name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .label = .{ .name = name } } };
        return opnd;
    }

    pub fn initLocal(alloc: std.mem.Allocator, id: usize, name: anytype, source_name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .variable = .{ .local = .{ .name = name, .source_name = source_name } } } };
        return opnd;
    }

    pub fn initScope(alloc: std.mem.Allocator, id: usize, scope: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .scope = .{ .value = scope } } };
        return opnd;
    }

    pub fn initRedef(alloc: std.mem.Allocator, id: usize, variant: usize, orig: *Operand, defblock: *BasicBlock) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .variable = .{ .redef = .{ .variant = variant, .orig = orig, .defblock = defblock } } } };
        return opnd;
    }

    pub fn initPrime(alloc: std.mem.Allocator, id: usize, primeid: usize, orig: *Operand) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .variable = .{ .prime = .{ .prime_id = primeid, .orig = orig } } } };
        return opnd;
    }

    pub fn initString(alloc: std.mem.Allocator, id: usize, value: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .string = .{ .value = value } } };
        return opnd;
    }

    pub fn initTemp(alloc: std.mem.Allocator, id: usize, name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .id = id, .data = .{ .variable = .{ .temp = .{ .name = name } } } };
        return opnd;
    }

    pub fn number(self: Operand) usize {
        return switch (self.data) {
            .immediate => unreachable,
            .string => unreachable,
            .scope => unreachable,
            .variable => |v| switch (v) {
                .redef => unreachable,
                .prime => unreachable,
                inline else => |payload| payload.name,
            },
            inline else => |payload| payload.name,
        };
    }

    pub fn getVar(self: *Operand) *Operand {
        return switch (self.data) {
            .variable => |v| switch (v) {
                .temp, .local => self,
                .redef => |r| getVar(r.orig),
                .prime => |p| getVar(p.orig),
            },
            inline else => unreachable,
        };
    }

    pub fn getID(self: Operand) usize {
        return self.id;
    }

    pub fn isTemp(self: Operand) bool {
        return switch (self.data) {
            .variable => |v| switch (v) {
                .temp => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn isVariable(self: Operand) bool {
        return switch (self.data) {
            .variable => true,
            else => false,
        };
    }

    pub fn isPrime(self: Operand) bool {
        return switch (self.data) {
            .variable => |v| switch (v) {
                .prime => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn isRedef(self: Operand) bool {
        return switch (self.data) {
            .variable => |v| switch (v) {
                .redef => true,
                else => false,
            },
            else => false,
        };
    }

    pub fn getDefinitionBlock(self: Operand) *BasicBlock {
        return switch(self.data) {
            .variable => |v| switch (v) {
                .redef => |r| r.defblock,
                .temp => |t| t.defblock.?,
                .prime => |p| p.orig.getDefinitionBlock(),
                else => unreachable,
            },
            else => unreachable,
        };
    }

    pub fn setDefinitionBlock(self: *Operand, block: *BasicBlock) void {
        switch(self.data) {
            .variable => |*v| switch (v.*) {
                .redef => |*r| r.defblock = block,
                .temp => |*t| t.defblock = block,
                else => {},
            },
            else => {}
        }
    }

    pub fn shortName(self: Operand) []const u8 {
        return switch (self.data) {
            .immediate => "I",
            .label => "L",
            .string => "s",
            .scope => "S",
            .variable => |v| switch (v) {
                .local => "l",
                .prime => "P",
                .temp => "t",
                .redef => "r",
            },
        };
    }
};

pub const InstructionName = enum {
    call,
    define_method,
    getself,
    getparam,
    jump,
    jumpif,
    jumpunless,
    leave,
    loadi,
    loadnil,
    mov,
    phi,
    pmov,     // parallel move group
    putlabel,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        const Self = @This();

        out: *Operand,
        recv: *Operand,
        name: *Operand,
        params: std.ArrayList(*Operand),

        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.recv == old) {
                self.recv = new;
            }
            for (0..self.params.items.len) |i| {
                if (self.params.items[i] == old) {
                    self.params.items[i] = new;
                }
            }
        }
    },

    define_method: struct {
        const Self = @This();
        out: *Operand,
        name: *Operand,
        func: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (old == self.name) self.name = new;
            if (old == self.func) self.func = new;
        }
    },

    getself: struct {
        const Self = @This();
        out: *Operand,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    getparam: struct {
        const Self = @This();
        out: *Operand,
        index: usize,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    jump: struct {
        const Self = @This();
        label: *Operand,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    jumpif: struct {
        const Self = @This();
        in: *Operand,
        label: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.in == old) self.in = new;
        }
    },

    jumpunless: struct {
        const Self = @This();
        in: *Operand,
        label: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.in == old) self.in = new;
        }
    },

    leave: struct {
        const Self = @This();
        in: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.in == old) self.in = new;
        }
    },

    loadi: struct {
        const Self = @This();
        out: *Operand,
        val: *Operand,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    loadnil: struct {
        const Self = @This();
        out: *Operand,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    mov: struct {
        const Self = @This();
        out: *Operand,
        in: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.in == old) self.in = new;
        }
    },

    phi: struct {
        const Self = @This();
        out: *Operand,
        params: std.ArrayList(*Operand),
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            for (0..self.params.items.len) |i| {
                if (self.params.items[i] == old) {
                    self.params.items[i] = new;
                }
            }
        }
    },

    pmov: struct {
        const Self = @This();
        out: *Operand,
        in: *Operand,
        block: *BasicBlock,
        group: usize,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.in == old) self.in = new;
        }
    },

    putlabel: struct {
        const Self = @This();
        name: *Operand,
        pub fn replaceOpnd(_: *Self, _: *const Operand, _: *Operand) void {
            unreachable;
        }
    },

    setlocal: struct {
        const Self = @This();
        name: *Operand,
        val: *Operand,
        pub fn replaceOpnd(self: *Self, old: *const Operand, new: *Operand) void {
            if (self.val == old) self.val = new;
        }
    },

    pub fn replaceOpnd(self: *Instruction, old: *const Operand, new: *Operand) void {
        switch (self.*) {
            inline else => |*variant| variant.replaceOpnd(old, new),
        }
    }

    fn nth_field(comptime T: type, comptime F: type, t: *const T, index: usize) ?*const F {
        inline for (std.meta.fields(T), 0..) |field, field_index| {
            if (field.type == (*F)) {
                if (index == field_index) {
                    return @field(t, field.name);
                }
            }
        }
        return null;
    }

    fn nth_union_field(e: *const Instruction, index: usize) ?*const Operand {
        switch (e.*) {
            inline else => |*variant| return nth_field(@TypeOf(variant.*), Operand, variant, index),
        }
    }

    fn nth_list(comptime T: type, comptime F: type, t: *const T, index: usize) ?std.ArrayList(*F) {
        inline for (std.meta.fields(T), 0..) |field, field_index| {
            if (field.type == (std.ArrayList(*F))) {
                if (index == field_index) {
                    return @field(t, field.name);
                }
            }
        }
        return null;
    }

    fn nth_union_list(e: *const Instruction, index: usize) ?std.ArrayList(*Operand) {
        switch (e.*) {
            inline else => |*variant| return nth_list(@TypeOf(variant.*), Operand, variant, index),
        }
    }

    fn item_fields_sub(comptime T: type) u64 {
        var mask: u64 = 0;

        inline for (std.meta.fields(T), 0..) |field, field_index| {
            if (!std.mem.eql(u8, field.name, "out")) {
                mask |= (1 << field_index);
            }
        }
        return mask;
    }

    fn item_fields(e: *const Instruction) usize {
        switch (e.*) {
            inline else => |*variant| return item_fields_sub(@TypeOf(variant.*)),
        }
    }

    fn array_fields_sub(comptime T: type, comptime F: type) u64 {
        var fields: u64 = 0;

        inline for (std.meta.fields(T), 0..) |field, field_index| {
            if (field.type == (std.ArrayList(*F)) and !std.mem.eql(u8, field.name, "out")) {
                fields |= (1 << field_index);
            }
        }
        return fields;
    }

    fn array_fields(e: *const Instruction) usize {
        switch (e.*) {
            inline else => |*variant| return array_fields_sub(@TypeOf(variant.*), Operand),
        }
    }

    const OpIter = struct {
        item_index: usize = 0,
        item_fields: u64,
        array_fields: u64,
        array_index: usize = 0,
        insn: *const Instruction,

        fn advance(self: *OpIter) void {
            self.item_index += 1;
            self.item_fields >>= 1;
            self.array_fields >>= 1;
            self.array_index = 0;
        }

        pub fn next(self: *OpIter) ?*const Operand {
            if (self.item_fields > 0) {
                while ((self.item_fields & 0x1) != 0x1) {
                    self.advance();
                }
                if (self.item_fields & 0x1 == 0x1 and self.array_fields & 0x1 == 0x1) {
                    const item_idx = self.item_index;
                    const ary_idx = self.array_index;
                    const list = self.insn.nth_union_list(item_idx).?;
                    self.array_index += 1;
                    if (ary_idx >= list.items.len) {
                        self.advance();
                        return next(self);
                    } else {
                        return list.items[ary_idx];
                    }
                } else {
                    const idx = self.item_index;
                    self.advance();
                    return self.insn.nth_union_field(idx);
                }
            }
            return null;
        }
    };

    pub fn opIter(self: *const Instruction) OpIter {
        const if_mask = self.item_fields();
        const af_mask = self.array_fields();

        return .{ .insn = self, .item_fields = if_mask, .array_fields = af_mask };
    }

    pub fn isAssignment(self: Instruction) bool {
        return switch (self) {
            .putlabel => false,
            .jump => false,
            .jumpunless => false,
            .jumpif => false,
            .setlocal => false,
            .leave => false,
            else => true,
        };
    }

    pub fn isJump(self: Instruction) bool {
        return switch (self) {
            .jump, .jumpunless, .jumpif => true,
            else => false,
        };
    }

    pub fn isLabel(self: Instruction) bool {
        return switch (self) {
            .putlabel => true,
            else => false,
        };
    }

    pub fn isReturn(self: Instruction) bool {
        return switch (self) {
            .leave => true,
            else => false,
        };
    }

    pub fn isCall(self: Instruction) bool {
        return switch (self) {
            .call => true,
            else => false,
        };
    }

    pub fn isPhi(self: Instruction) bool {
        return switch (self) {
            .phi => true,
            else => false,
        };
    }

    pub fn isPMov(self: Instruction) bool {
        return switch (self) {
            .pmov => true,
            else => false,
        };
    }

    pub fn jumpTarget(self: Instruction) *Operand {
        return switch (self) {
            .jump => |payload| payload.label,
            .jumpif => |payload| payload.label,
            .jumpunless => |payload| payload.label,
            else => unreachable,
        };
    }

    pub fn outVar(self: Instruction) ?*Operand {
        return switch (self) {
            .putlabel => null,
            .jump => null,
            .jumpif => null,
            .jumpunless => null,
            .setlocal => null,
            .leave => null,
            inline else => |payload| payload.out,
        };
    }

    pub fn getOut(self: Instruction) ?*Operand {
        return self.outVar();
    }

    pub fn setOut(self: *Instruction, opnd: *Operand) void {
        switch (self.*) {
            .putlabel, .jump, .jumpif, .jumpunless, .setlocal, .leave => unreachable,
            inline else => |*payload| payload.out = opnd,
        }
    }

    pub fn deinit(self: Instruction) void {
        switch (self) {
            .call => |x| x.params.deinit(),
            .phi => |x| x.params.deinit(),
            .define_method => |x| x.func.data.scope.value.deinit(),
            else => {},
        }
    }
};

pub const InstructionListNode = struct {
    node: std.DoublyLinkedList.Node,
    number: usize = 0,
    data: Instruction,
};

test "can iterate on ops" {
    var out = Operand{
        .id = 0,
        .data = .{ .variable = .{ .temp = .{ .name = 0 } } },
    };
    var in = Operand{
        .id = 1,
        .data = .{ .variable = .{ .temp = .{ .name = 1 } } },
    };
    var insn = Instruction{
        .mov = .{
            .out = &out,
            .in = &in,
        },
    };
    var itr = insn.opIter();
    var list = [_]usize{0};
    var i: u32 = 0;
    while (itr.next()) |op| {
        list[i] = op.id;
        i += 1;
    }
    try std.testing.expectEqual(1, i);
    try std.testing.expectEqual(1, list[0]);
}

test "can iterate on ops with list" {
    var out = Operand{
        .id = 0,
        .data = .{ .variable = .{ .temp = .{ .name = 0 } } },
    };
    var recv = Operand{
        .id = 1,
        .data = .{ .variable = .{ .temp = .{ .name = 1 } } },
    };
    var name = Operand{
        .id = 2,
        .data = .{ .string = .{ .value = "test_method" } },
    };
    const param1 = Operand{
        .id = 3,
        .data = .{ .variable = .{ .temp = .{ .name = 3 } } },
    };
    const param2 = Operand{
        .id = 4,
        .data = .{ .variable = .{ .temp = .{ .name = 4 } } },
    };

    var params = std.ArrayList(*Operand).init(std.testing.allocator);
    defer params.deinit();

    try params.append(@constCast(&param1));
    try params.append(@constCast(&param2));

    var insn = Instruction{
        .call = .{
            .out = &out,
            .recv = &recv,
            .name = &name,
            .params = params,
        },
    };
    var itr = insn.opIter();
    var list = [_]usize{ 0, 0, 0, 0 };
    var i: u32 = 0;
    while (itr.next()) |op| {
        list[i] = op.id;
        i += 1;
    }
    try std.testing.expectEqual(4, i);
    try std.testing.expectEqual(1, list[0]);
    try std.testing.expectEqual(2, list[1]);
    try std.testing.expectEqual(3, list[2]);
    try std.testing.expectEqual(4, list[3]);
}
