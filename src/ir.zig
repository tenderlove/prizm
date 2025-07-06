const std = @import("std");
const cmp = @import("compiler.zig");
const cfg = @import("cfg.zig");
const Scope = @import("scope.zig").Scope;
const BasicBlock = cfg.BasicBlock;
const BitMap = std.DynamicBitSetUnmanaged;

pub const InstructionList = std.DoublyLinkedList;

pub const VariableType = enum {
    local,
    temp,
    redef,
    prime,
    live_range,
    physical_register,
};

pub const RegisterClass = enum {
    parameter, // R0-R3 (for flexible parameter allocation)
    caller_saved, // R0-R3
    callee_saved, // R4-R7
};

pub const RegisterConstraint = union(enum) {
    general_purpose, // Can use any available register
    specific_register: usize, // Must use this exact register (offset in to specific register list)
    register_class: RegisterClass, // Must use one from this class
};

pub const VariableData = union(VariableType) {
    local: struct { id: usize, source_name: []const u8 },
    temp: struct {
        id: usize,
        defblock: ?*BasicBlock = null,
    },
    redef: struct { id: usize, variant: usize, orig: *Variable, defblock: *BasicBlock },
    prime: struct { id: usize, orig: *Variable },
    live_range: struct {
        id: usize,
        variables: BitMap, // All variables in this live range
        constraint: RegisterConstraint,
    },
    physical_register: struct {
        id: usize,
        register: usize,
    },
};

pub const Label = struct {
    id: usize,
};

pub const Variable = struct {
    id: usize,
    data: VariableData,

    pub fn initLocal(alloc: std.mem.Allocator, id: usize, name: anytype, source_name: anytype) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .data = .{ .local = .{ .id = name, .source_name = source_name } } };
        return opnd;
    }

    pub fn initRedef(alloc: std.mem.Allocator, id: usize, local_id: usize, variant: usize, orig: *Variable, defblock: *BasicBlock) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .data = .{ .redef = .{ .id = local_id, .variant = variant, .orig = orig, .defblock = defblock } } };
        return opnd;
    }

    pub fn initPrime(alloc: std.mem.Allocator, id: usize, local_id: usize, orig: *Variable) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .data = .{ .prime = .{ .id = local_id, .orig = orig } } };
        return opnd;
    }

    pub fn initPhysicalRegister(alloc: std.mem.Allocator, id: usize, local_id: usize, register: usize) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .data = .{ .physical_register = .{ .id = local_id, .register = register } } };
        return opnd;
    }

    pub fn initTemp(alloc: std.mem.Allocator, id: usize, local_id: anytype) !*Variable {
        const opnd = try alloc.create(Variable);
        opnd.* = .{ .id = id, .data = .{ .temp = .{ .id = local_id } } };
        return opnd;
    }

    pub fn initLiveRange(alloc: std.mem.Allocator, id: usize, local_id: usize, varcount: usize) !*Variable {
        const opnd = try alloc.create(Variable);
        // Default to general purpose register, set to more specific later
        opnd.* = .{ .id = id, .data = .{ .live_range = .{ .id = local_id, .variables = try BitMap.initEmpty(alloc, varcount), .constraint = .general_purpose } } };
        return opnd;
    }

    pub fn number(self: Variable) usize {
        return switch (self.data) {
            .redef => unreachable,
            .prime => unreachable,
            inline else => |payload| payload.name,
        };
    }

    pub fn addVariable(self: *Variable, id: usize) void {
        return switch (self.data) {
            .live_range => |*lr| lr.variables.set(id),
            else => unreachable,
        };
    }

    pub fn getVar(self: *Variable) *Variable {
        return switch (self.data) {
            .temp, .local => self,
            .redef => |r| getVar(r.orig),
            .prime => |p| getVar(p.orig),
            .live_range => unreachable,
            .physical_register => unreachable,
        };
    }

    pub fn setSpecificRegister(self: *Variable, reg: usize) void {
        switch (self.data) {
            .live_range => |*lr| lr.constraint = .{ .specific_register = reg },
            else => unreachable,
        }
    }

    pub fn getGlobalId(self: Variable) usize {
        return self.id;
    }

    pub fn getLocalId(self: Variable) usize {
        return switch (self.data) {
            inline else => |payload| payload.id,
        };
    }

    pub fn isTemp(self: Variable) bool {
        return switch (self.data) {
            .temp => true,
            else => false,
        };
    }

    pub fn isPrime(self: Variable) bool {
        return switch (self.data) {
            .prime => true,
            else => false,
        };
    }

    pub fn isRedef(self: Variable) bool {
        return switch (self.data) {
            .redef => true,
            else => false,
        };
    }

    pub fn getDefinitionBlock(self: Variable) *BasicBlock {
        return switch (self.data) {
            .redef => |r| r.defblock,
            .temp => |t| t.defblock.?,
            .prime => |p| p.orig.getDefinitionBlock(),
            else => unreachable,
        };
    }

    pub fn setDefinitionBlock(self: *Variable, block: *BasicBlock) void {
        switch (self.data) {
            .redef => |*r| r.defblock = block,
            .temp => |*t| t.defblock = block,
            else => {},
        }
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
    load_stack_param,
    mov,
    phi,
    pmov, // parallel move group
    putlabel,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        const Self = @This();

        out: *Variable,
        recv: *Variable,
        name: []const u8,
        params: std.ArrayList(*Variable),

        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
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
        out: *Variable,
        name: []const u8,
        func: *Scope,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    getself: struct {
        const Self = @This();
        out: *Variable,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    getparam: struct {
        const Self = @This();
        out: *Variable,
        index: usize,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    jump: struct {
        const Self = @This();
        label: Label,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    jumpif: struct {
        const Self = @This();
        in: *Variable,
        label: Label,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.in == old) self.in = new;
        }
    },

    jumpunless: struct {
        const Self = @This();
        in: *Variable,
        label: Label,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.in == old) self.in = new;
        }
    },

    leave: struct {
        const Self = @This();
        in: *Variable,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.in == old) self.in = new;
        }
    },

    loadi: struct {
        const Self = @This();
        out: *Variable,
        val: u64,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    loadnil: struct {
        const Self = @This();
        out: *Variable,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    load_stack_param: struct {
        const Self = @This();
        out: *Variable,
        offset: usize, // Stack index
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    mov: struct {
        const Self = @This();
        out: *Variable,
        in: *Variable,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.in == old) self.in = new;
        }
    },

    phi: struct {
        const Self = @This();
        out: *Variable,
        params: std.ArrayList(*Variable),
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            for (0..self.params.items.len) |i| {
                if (self.params.items[i] == old) {
                    self.params.items[i] = new;
                }
            }
        }
    },

    pmov: struct {
        const Self = @This();
        out: *Variable,
        in: *Variable,
        block: *BasicBlock,
        group: usize,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.in == old) self.in = new;
        }
    },

    putlabel: struct {
        const Self = @This();
        name: Label,
        pub fn replaceOpnd(_: *Self, _: *const Variable, _: *Variable) void {
            unreachable;
        }
    },

    setlocal: struct {
        const Self = @This();
        name: *Variable,
        val: *Variable,
        pub fn replaceOpnd(self: *Self, old: *const Variable, new: *Variable) void {
            if (self.val == old) self.val = new;
        }
    },

    pub fn replaceOpnd(self: *Instruction, old: *const Variable, new: *Variable) void {
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

    fn nth_union_field(e: *const Instruction, index: usize) ?*const Variable {
        switch (e.*) {
            inline else => |*variant| return nth_field(@TypeOf(variant.*), Variable, variant, index),
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

    fn nth_union_list(e: *const Instruction, index: usize) ?std.ArrayList(*Variable) {
        switch (e.*) {
            inline else => |*variant| return nth_list(@TypeOf(variant.*), Variable, variant, index),
        }
    }

    fn item_fields_sub(comptime T: type) u64 {
        var mask: u64 = 0;

        inline for (std.meta.fields(T), 0..) |field, field_index| {
            if (field.type == (*Variable) and !std.mem.eql(u8, field.name, "out")) {
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
            inline else => |*variant| return array_fields_sub(@TypeOf(variant.*), Variable),
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

        pub fn next(self: *OpIter) ?*const Variable {
            if (self.item_fields > 0 or self.array_fields > 0) {
                while ((self.item_fields & 0x1) != 0x1 and (self.array_fields & 0x1) != 0x1) {
                    self.advance();
                }
                if (self.array_fields & 0x1 == 0x1) {
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
                } else if (self.item_fields & 0x1 == 0x1) {
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

    pub fn jumpTarget(self: Instruction) Label {
        return switch (self) {
            .jump => |payload| payload.label,
            .jumpif => |payload| payload.label,
            .jumpunless => |payload| payload.label,
            else => unreachable,
        };
    }

    pub fn outVar(self: Instruction) ?*Variable {
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

    pub fn getOut(self: Instruction) ?*Variable {
        return self.outVar();
    }

    pub fn setOut(self: *Instruction, opnd: *Variable) void {
        switch (self.*) {
            .putlabel, .jump, .jumpif, .jumpunless, .setlocal, .leave => unreachable,
            inline else => |*payload| payload.out = opnd,
        }
    }

    pub fn deinit(self: Instruction) void {
        switch (self) {
            .call => |x| x.params.deinit(),
            .phi => |x| x.params.deinit(),
            .define_method => |x| x.func.deinit(),
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
    var out = Variable{
        .id = 0,
        .data = .{ .temp = .{ .id = 0 } },
    };
    var in = Variable{
        .id = 1,
        .data = .{ .temp = .{ .id = 1 } },
    };
    var insn = Instruction{
        .mov = .{
            .out = &out,
            .in = &in,
        },
    };
    var itr = insn.opIter();
    var count: u32 = 0;
    while (itr.next()) |op| {
        try std.testing.expectEqual(1, op.getGlobalId());
        count += 1;
    }
    try std.testing.expectEqual(1, count);
}

test "can iterate on ops with list" {
    var out = Variable{
        .id = 0,
        .data = .{ .temp = .{ .id = 0 } },
    };
    var recv = Variable{
        .id = 1,
        .data = .{ .temp = .{ .id = 1 } },
    };
    const param1 = Variable{
        .id = 3,
        .data = .{ .temp = .{ .id = 3 } },
    };
    const param2 = Variable{
        .id = 4,
        .data = .{ .temp = .{ .id = 4 } },
    };

    var params = std.ArrayList(*Variable).init(std.testing.allocator);
    defer params.deinit();

    try params.append(@constCast(&param1));
    try params.append(@constCast(&param2));

    var insn = Instruction{
        .call = .{
            .out = &out,
            .recv = &recv,
            .name = "test_method",
            .params = params,
        },
    };
    var itr = insn.opIter();
    const expected_ids = [_]usize{ 1, 3, 4 }; // recv, param1, param2 variable IDs (name is now []const u8)
    var var_count: u32 = 0;
    var total_count: u32 = 0;
    while (itr.next()) |op| {
        try std.testing.expectEqual(expected_ids[var_count], op.getGlobalId());
        var_count += 1;
        total_count += 1;
    }
    try std.testing.expectEqual(3, total_count); // Now 3 instead of 4 since name is not an operand
    try std.testing.expectEqual(3, var_count);
}
