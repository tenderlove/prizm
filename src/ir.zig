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
    },

    getlocal: struct {
        out: Operand,
        in: Operand,
    },

    getself: struct {
        out: Operand,
    },

    jumpunless: struct {
        in: Operand,
        label: Operand,
    },

    label: struct {
        name: Operand,
    },

    leave: struct {
        out: Operand,
        in: Operand,
    },

    loadi: struct {
        out: Operand,
        val: Operand,
    },

    loadnil: struct {
        out: Operand,
    },

    phi: struct {
        out: Operand,
        a: Operand,
        b: Operand,
    },

    setlocal: struct {
        out: Operand,
        name: Operand,
        val: Operand,
    },

    //pub fn eachParam(self: Instruction, fun: fn (Param)

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
