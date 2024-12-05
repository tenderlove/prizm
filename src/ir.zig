const std = @import("std");
const cmp = @import("compiler.zig");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const OperandType = enum {
    constant,
    cvar,
    immediate,
    ivar,
    label,
    local,
    param,
    scope,
    string,
    temp,
};

pub const Operand = union(OperandType) {
    constant: struct { id: usize, name: usize, },
    cvar: struct { id: usize, name: usize, },
    immediate: struct { id: usize, value: u64, },
    ivar: struct { id: usize, name: usize, },
    label: struct { id: usize, name: usize, },
    local: struct { id: usize, name: usize, source_name: []const u8 },
    param: struct { id: usize, name: usize, source_name: []const u8 },
    scope: struct { id: usize, value: *cmp.Scope, },
    string: struct { id: usize, value: []const u8, },
    temp: struct { id: usize, name: usize, },

    pub fn initImmediate(alloc: std.mem.Allocator, id: usize, value: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .immediate = .{ .id = id, .value = value } };
        return opnd;
    }

    pub fn initLabel(alloc: std.mem.Allocator, id: usize, name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .label = .{ .id = id, .name = name } };
        return opnd;
    }

    pub fn initLocal(alloc: std.mem.Allocator, id: usize, name: anytype, source_name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .local = .{ .id = id, .name = name, .source_name = source_name } };
        return opnd;
    }

    pub fn initParam(alloc: std.mem.Allocator, id: usize, name: anytype, source_name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .param = .{ .id = id, .name = name, .source_name = source_name } };
        return opnd;
    }

    pub fn initScope(alloc: std.mem.Allocator, id: usize, scope: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .scope = .{ .id = id, .value = scope } };
        return opnd;
    }

    pub fn initString(alloc: std.mem.Allocator, id: usize, value: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .string = .{ .id = id, .value = value } };
        return opnd;
    }

    pub fn initTemp(alloc: std.mem.Allocator, id: usize, name: anytype) !*Operand {
        const opnd = try alloc.create(Operand);
        opnd.* = .{ .temp = .{ .id = id, .name = name } };
        return opnd;
    }

    pub fn number(self: Operand) usize {
        return switch(self) {
            .immediate => unreachable,
            .string => unreachable,
            .scope => unreachable,
            inline else => |payload| payload.name
        };
    }

    pub fn getID(self: Operand) usize {
        return switch(self) {
            inline else => |payload| payload.id
        };
    }

    pub fn isVariable(self: Operand) bool {
        return switch(self) {
            .param, .temp, .local => true,
            inline else => false
        };
    }

    pub fn isParam(self: Operand) bool {
        return switch(self) {
            .param => true,
            inline else => false
        };
    }

    pub fn shortName(self: Operand) [] const u8 {
        return switch(self) {
            .constant => "k",
            .cvar => "c",
            .immediate => "I",
            .ivar => "i",
            .label => "L",
            .local => "l",
            .param => "p",
            .string => "s",
            .scope => "S",
            .temp => "t",
        };
    }
};

pub const InstructionName = enum {
    call,
    define_method,
    getlocal,
    getself,
    jump,
    jumpunless,
    leave,
    loadi,
    loadnil,
    mov,
    phi,
    putlabel,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        out: *Operand,
        recv: *Operand,
        name: *Operand,
        params: std.ArrayList(*Operand),
    },

    define_method: struct {
        out: *Operand,
        name: *Operand,
        func: *Operand,
    },

    getlocal: struct {
        out: *Operand,
        in: *Operand,
    },

    getself: struct {
        out: *Operand,
    },

    jump: struct {
        label: *Operand,
    },

    jumpunless: struct {
        in: *Operand,
        label: *Operand,
    },

    leave: struct {
        in: *Operand,
    },

    loadi: struct {
        out: *Operand,
        val: *Operand,
    },

    loadnil: struct {
        out: *Operand,
    },

    mov: struct {
        out: *Operand,
        in: *Operand,
    },

    phi: struct {
        out: *Operand,
        a: *Operand,
        b: *Operand,
    },

    putlabel: struct {
        name: *Operand,
    },

    setlocal: struct {
        name: *Operand,
        val: *Operand,
    },

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

        fn advance(self:*OpIter) void {
            self.item_index += 1;
            self.item_fields >>= 1;
            self.array_fields >>= 1;
            self.array_index = 0;
        }

        pub fn next(self: *OpIter) ?*const Operand {
            if (self.item_fields > 0) {
                while((self.item_fields & 0x1) != 0x1) {
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

        return .{
            .insn = self,
            .item_fields = if_mask,
            .array_fields = af_mask
        };
    }

    pub fn isAssignment(self: Instruction) bool {
        return switch(self) {
            .putlabel => false,
            .jump => false,
            .jumpunless => false,
            .setlocal => false,
            .leave => false,
            else => true,
        };
    }

    pub fn isJump(self: Instruction) bool {
        return switch(self) {
            .jump, .jumpunless => true,
            else => false
        };
    }

    pub fn isLabel(self: Instruction) bool {
        return switch(self) {
            .putlabel => true,
            else => false
        };
    }

    pub fn isReturn(self: Instruction) bool {
        return switch(self) {
            .leave => true,
            else => false
        };
    }

    pub fn isCall(self: Instruction) bool {
        return switch(self) {
            .call => true,
            else => false
        };
    }

    pub fn jumpTarget(self: Instruction) *Operand {
        return switch(self) {
            .jump => |payload| payload.label,
            .jumpunless => |payload| payload.label,
            else => unreachable
        };
    }

    pub fn outVar(self: Instruction) ?*Operand {
        return switch (self) {
            .putlabel => null,
            .jump => null,
            .jumpunless => null,
            .setlocal => null,
            .leave => null,
            inline else => |payload| payload.out
        };
    }

    pub fn deinit(self: Instruction) void {
        switch(self) {
            .call => |x| x.params.deinit(),
            .define_method => |x| x.func.scope.value.deinit(),
            else  => {}
        }
    }
};

test "can iterate on ops" {
    var out = Operand { .temp = .{ .id = 0, .name = 0 }, };
    var in = Operand { .temp = .{ .id = 1, .name = 1 }, };
    var insn = Instruction {
        .mov = .{
            .out = &out,
            .in = &in,
        },
    };
    var itr = insn.opIter();
    var list = [_]usize { 0 };
    var i: u32 = 0;
    while (itr.next()) |op| {
        list[i] = op.temp.id;
        i += 1;
    }
    try std.testing.expectEqual(1, i);
    try std.testing.expectEqual(1, list[0]);
}

test "can iterate on ops with list" {
    var out = Operand { .temp = .{ .id = 0, .name = 0 }, };
    var recv = Operand { .temp = .{ .id = 1, .name = 1 }, };
    var name = Operand { .temp = .{ .id = 2, .name = 2 }, };
    const param1 = Operand { .temp = .{ .id = 3, .name = 3 }, };
    const param2 = Operand { .temp = .{ .id = 4, .name = 4 }, };

    var params = std.ArrayList(*Operand).init(std.testing.allocator);
    defer params.deinit();

    try params.append(@constCast(&param1));
    try params.append(@constCast(&param2));

    var insn = Instruction {
        .call = .{
            .out = &out,
            .recv = &recv,
            .name = &name,
            .params = params,
        },
    };
    var itr = insn.opIter();
    var list = [_]usize { 0, 0, 0, 0 };
    var i: u32 = 0;
    while (itr.next()) |op| {
        list[i] = op.temp.id;
        i += 1;
    }
    try std.testing.expectEqual(4, i);
    try std.testing.expectEqual(1, list[0]);
    try std.testing.expectEqual(2, list[1]);
    try std.testing.expectEqual(3, list[2]);
    try std.testing.expectEqual(4, list[3]);
}
