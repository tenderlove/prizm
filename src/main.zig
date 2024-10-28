//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");

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
        const root = c.pm_parse(parser.ptr);
        std.debug.print("Node type {s}\n", .{ c.pm_node_type_to_str(root.*.type) });
        //defer c.pm_node_destroy(parser.ptr, root);
        //defer c.pm_parser_free(parser.ptr);
    }
    else {
        std.debug.print("Prism version: {d}, {s}.\n", .{args.len, c.pm_version()});
        return;
    }
}
