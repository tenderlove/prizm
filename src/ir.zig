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
    constant: struct { name: usize, },
    cvar: struct { name: usize, },
    immediate: struct { value: u64, },
    ivar: struct { name: usize, },
    label: struct { name: usize, },
    local: struct { name: usize, },
    param: struct { name: usize, },
    scope: struct { value: *cmp.Scope, },
    string: struct { value: []const u8, },
    temp: struct { name: usize, },

    pub fn number(self: Operand) usize {
        return switch(self) {
            .immediate => unreachable,
            .string => unreachable,
            .scope => unreachable,
            inline else => |payload| payload.name
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
    label,
    leave,
    loadi,
    loadnil,
    mov,
    phi,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        out: Operand,
        recv: Operand,
        name: Operand,
        params: std.ArrayList(Operand),

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            const total = self.params.items.len + 2;
            var idx: usize = 2;
            fun(self.name, 0, total, ctx);
            fun(self.recv, 1, total, ctx);
            for (self.params.items) |op| {
                fun(op, idx, total, ctx);
                idx += 1;
            }
        }
    },

    define_method: struct {
        out: Operand,
        name: Operand,
        func: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.name, 0, 2, ctx);
            fun(self.func, 1, 2, ctx);
        }
    },

    getlocal: struct {
        out: Operand,
        in: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.in, 0, 1, ctx);
        }
    },

    getself: struct {
        out: Operand,

        pub fn eachOperand(_: @This(), _: fn (Operand, usize, usize, anytype) void, _: anytype) void { }
    },

    jump: struct {
        label: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.label, 0, 1, ctx);
        }
    },

    jumpunless: struct {
        in: Operand,
        label: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.in, 0, 2, ctx);
            fun(self.label, 1, 2, ctx);
        }
    },

    label: struct {
        name: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.name, 0, 1, ctx);
        }
    },

    leave: struct {
        in: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.in, 0, 1, ctx);
        }
    },

    loadi: struct {
        out: Operand,
        val: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.val, 0, 1, ctx);
        }
    },

    loadnil: struct {
        out: Operand,

        pub fn eachOperand(_: @This(), _: fn (Operand, usize, usize, anytype) void, _: anytype) void { }
    },

    mov: struct {
        out: Operand,
        in: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.in, 0, 1, ctx);
        }
    },

    phi: struct {
        out: Operand,
        a: Operand,
        b: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.a, 0, 2, ctx);
            fun(self.b, 1, 2, ctx);
        }
    },

    setlocal: struct {
        name: Operand,
        val: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
            fun(self.name, 0, 2, ctx);
            fun(self.val, 1, 2, ctx);
        }
    },

    pub fn eachOperand(self: Instruction, fun: fn (Operand, usize, usize, anytype) void, ctx: anytype) void {
        switch(self) {
            inline else => |p| p.eachOperand(fun, ctx)
        }
    }

    pub fn isJump(self: Instruction) bool {
        return switch(self) {
            .jump, .jumpunless => true,
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

    pub fn outVar(self: Instruction) ?Operand {
        return switch (self) {
            .label => null,
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
