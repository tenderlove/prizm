//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const prism = @import("root.zig");
const Allocator = std.mem.Allocator;

const c = @cImport({
    @cInclude("prism.h");
});

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // Get the command line args
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 2) {
        var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const path = try std.fs.realpathZ(args.ptr[1], &path_buffer);

        const file = try std.fs.openFileAbsolute(path, .{});
        const file_size = (try file.stat()).size;
        std.debug.print("File: {s} {d}.\n", .{args.ptr[1], file_size});
        defer file.close();

        const src = try file.readToEndAlloc(allocator, file_size);

        const parser = try allocator.alloc(c.pm_parser_t, 1);
        c.pm_parser_init(parser.ptr, src.ptr, file_size, null);
        defer c.pm_parser_free(parser.ptr);

        const root_node = c.pm_parse(parser.ptr);
        var buf = std.ArrayList(u8).init(allocator);
        try prism.prism_pp(&buf, parser.ptr, root_node);
        std.debug.print("{s}", .{ try buf.toOwnedSlice() });

        const buffer = try allocator.alloc(c.pm_buffer_t, 1);
        // defer c.pm_buffer_free(buffer.ptr);

        if (c.pm_buffer_init(buffer.ptr)) {
        c.pm_prettyprint(buffer.ptr, parser.ptr, root_node);
        c.pm_buffer_append_byte(buffer.ptr, 0);
        std.debug.print("hi mom\n", .{});

        std.debug.print("{s}\n", .{ c.pm_buffer_value(buffer.ptr)[0..(c.pm_buffer_length(buffer.ptr))] });
        }

        defer c.pm_node_destroy(parser.ptr, root_node);
    }
    else {
        std.debug.print("Prism version: {d}, {s}.\n", .{args.len, c.pm_version()});
        return;
    }
}
