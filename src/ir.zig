const std = @import("std");

pub const InstructionList = std.DoublyLinkedList(Instruction);

pub const NameType = enum {
    temp,
    local,
    ivar,
    cvar,
    constant
};

pub const Name = union(NameType) {
    temp: struct {
        name: usize,
    },

    local: struct {
        name: usize,
    },

    ivar: struct {
        name: usize,
    },

    cvar: struct {
        name: usize,
    },

    constant: struct {
        name: usize,
    },
};

pub const InstructionName = enum {
    call,
    getlocal,
    getmethod,
    getself,
    leave,
    loadi,
    loadnil,
    setlocal,
};

pub const Instruction = union(InstructionName) {
    call: struct {
        out: Name,
        funcreg: Name,
        params: std.ArrayList(Name),
    },

    getlocal: struct {
        out: Name,
        in: Name,
    },

    getmethod: struct {
        out: Name,
        recv: Name,
        ccid: usize,
    },

    getself: struct {
        out: Name,
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

    setlocal: struct {
        out: Name,
        name: Name,
        val: Name,
    },

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
    pub fn deinit(self: Instruction) void {
        switch(self) {
            .call => |x| x.params.deinit(),
            else  => {}
        }
    }
};
