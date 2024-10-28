//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const c = @cImport({ @cInclude("prism.h"); });
const Allocator = std.mem.Allocator;

fn prettyprint_location(writer: std.ArrayList(u8).Writer, parser: [*c] const c.pm_parser_t, loc: *const c.pm_location_t) !void {
    const start = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.start, parser.*.start_line);
    const end = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.end, parser.*.start_line);
    try writer.print("({d},{d})-({d},{d})", .{ start.line, start.column, end.line, end.column });
}

fn pretty_print_node(writer: std.ArrayList(u8).Writer, parser: [*c] const c.pm_parser_t, node: [*c] const c.pm_node_t) !void {

    switch (node.*.type) {
        c.PM_PROGRAM_NODE => {
            //const cast = @ptrCast(node, c.pm_program_node_t);
            try writer.print("@ ProgramNode (location: ", .{});
            try prettyprint_location(writer, parser, &(node.*.location));
            try writer.print(")\n", .{});
        },
        else => {}
    }
    return;
}

pub fn prism_pp(buf: *std.ArrayList(u8), parser: [*c] const c.pm_parser_t, node: [*c] const c.pm_node_t) !void {
    try pretty_print_node(buf.writer(), parser, node);
    return;
}
