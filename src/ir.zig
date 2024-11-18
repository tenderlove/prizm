const std = @import("std");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const NameType = enum {
    constant,
    cvar,
    ivar,
    label,
    local,
    temp,
};

pub const Name = union(NameType) {
    constant: struct { name: usize, },
    cvar: struct { name: usize, },
    ivar: struct { name: usize, },
    label: struct { name: usize, },
    local: struct { name: usize, },
    temp: struct { name: usize, },

    pub fn number(self: Name) usize {
        return switch(self) {
            inline else => |payload| payload.name
        };
    }

    pub fn shortName(self: Name) [] const u8 {
        return switch(self) {
            .constant => "k",
            .cvar => "c",
            .ivar => "i",
            .label => "L",
            .local => "l",
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
        out: Name,
        recv: Name,
        name: []const u8,
        params: std.ArrayList(Name),
    },

    getlocal: struct {
        out: Name,
        in: Name,
    },

    getself: struct {
        out: Name,
    },

    jumpunless: struct {
        in: Name,
        label: Name,
    },

    label: struct {
        name: Name,
    },

    leave: struct {
        out: Name,
        in: Name,
    },

    loadi: struct {
        out: Name,
        val: u64,
    },

    loadnil: struct {
        out: Name,
    },

    phi: struct {
        out: Name,
        a: Name,
        b: Name,
    },

    setlocal: struct {
        out: Name,
        name: Name,
        val: Name,
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

    pub fn outVar(self: Instruction) ?Name {
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
