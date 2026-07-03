const std = @import("std");

pub const Globals = struct {
    allocator: std.mem.Allocator,
    strings: std.StringHashMapUnmanaged([]const u8),

    // Looks up a string from the string pool and returns it.
    // If the string isn't found in the string pool, copies it with
    // the vm's allocator, inserts it in to the pool, and returns the copy.
    // All strings in the pool are owned by the VM.
    pub fn getString(self: *Globals, str: []const u8) ![]const u8 {
        const value = self.strings.get(str);
        if (value) |v| {
            return v;
        } else {
            const v = try self.allocator.dupe(u8, str);
            try self.strings.put(self.allocator, v, v);
            return v;
        }
    }

    pub fn deinit(self: *Globals, allocator: std.mem.Allocator) void {
        var it = self.strings.valueIterator();
        while (it.next()) |val| {
            allocator.free(val.*);
        }
        self.strings.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn init(allocator: std.mem.Allocator) !*Globals {
        const self = try allocator.create(Globals);
        const strings = std.StringHashMapUnmanaged([]const u8){};

        self.* = .{
            .allocator = allocator,
            .strings = strings,
        };
        return self;
    }
};
