//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const prism = @import("root.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
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
        defer c.pm_node_destroy(parser.ptr, root_node);

        var scope_node: prism.pm_scope_node_t = .{
            .base = .{
                .type = c.PM_SCOPE_NODE,
            },
            .previous = null,
            .ast_node = null,
            .parameters = null,
            .body = null,
            .locals = .{
                .size = 0,
                .capacity = 0,
                .ids = null
            }
        };
        try prism.scope_node_init(root_node, &scope_node, null);

        const cc = try compiler.init(allocator, parser.ptr);
        defer cc.deinit(allocator);
        const iseq = try cc.compile(@ptrCast(&scope_node));

        const machine = try vm.init(allocator);
        try machine.eval(iseq);
        defer machine.deinit(allocator);

    }
    else {
        std.debug.print("Prism version: {d}, {s}.\n", .{args.len, c.pm_version()});
        return;
    }
}
