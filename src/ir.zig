const std = @import("std");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const OperandType = enum {
    constant,
    cvar,
    immediate,
    ivar,
    label,
    local,
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
    string: struct { value: []const u8, },
    temp: struct { name: usize, },

    pub fn number(self: Operand) usize {
        return switch(self) {
            .immediate => unreachable,
            .string => unreachable,
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
            .string => "s",
            .temp => "t",
        };
    }
};

pub const InstructionName = enum {
    call,
    getlocal,
    getself,
    jump,
    jumpunless,
    label,
    leave,
    loadi,
    loadnil,
    phi,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        out: Operand,
        recv: Operand,
        name: Operand,
        params: std.ArrayList(Operand),

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            const total = self.params.items.len + 2;
            var idx: usize = 2;
            fun(self.name, 0, total);
            fun(self.recv, 1, total);
            for (self.params.items) |op| {
                fun(op, idx, total);
                idx += 1;
            }
        }
    },

    getlocal: struct {
        out: Operand,
        in: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.in, 0, 1);
        }
    },

    getself: struct {
        out: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            _ = self;
            _ = fun;
        }
    },

    jump: struct {
        label: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.label, 0, 1);
        }
    },

    jumpunless: struct {
        in: Operand,
        label: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.in, 0, 2);
            fun(self.label, 1, 2);
        }
    },

    label: struct {
        name: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.name, 0, 1);
        }
    },

    leave: struct {
        out: Operand,
        in: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.in, 0, 1);
        }
    },

    loadi: struct {
        out: Operand,
        val: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.val, 0, 1);
        }
    },

    loadnil: struct {
        out: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            _ = self;
            _ = fun;
        }
    },

    phi: struct {
        out: Operand,
        a: Operand,
        b: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.a, 0, 2);
            fun(self.b, 1, 2);
        }
    },

    setlocal: struct {
        out: Operand,
        name: Operand,
        val: Operand,

        pub fn eachOperand(self: @This(), fun: fn (Operand, usize, usize) void) void {
            fun(self.name, 0, 2);
            fun(self.val, 1, 2);
        }
    },

    pub fn eachOperand(self: Instruction, fun: fn (Operand, usize, usize) void) void {
        switch(self) {
            inline else => |p| p.eachOperand(fun)
        }
    }

    pub fn isJump(self: Instruction) bool {
        _ = self;
        return false;
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
            inline else => |payload| payload.out
        };
    }

    pub fn deinit(self: Instruction) void {
        switch(self) {
            .call => |x| x.params.deinit(),
            else  => {}
        }
    }
};
