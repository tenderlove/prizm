//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const c = @cImport({ @cInclude("prism.h"); });
const Allocator = std.mem.Allocator;

const PP = struct {
    writer: std.ArrayList(u8).Writer,
    prefix: std.ArrayList(u8),
    prefix_lengths: std.ArrayList(usize),
    parser: [*c] const c.pm_parser_t,

    fn flush_prefix(pp: *PP) !void {
        try pp.writer.print("{s}", .{ pp.prefix.items });
    }

    fn push_prefix(pp: *PP, str: [] const u8) !void {
        try pp.prefix.appendSlice(str);
        try pp.prefix_lengths.append(str.len);
    }

    fn pop_prefix(pp: *PP) void {
        pp.prefix.items.len -= pp.prefix_lengths.pop();
    }

    fn pp_constant(pp: *PP, constant_id: u32) !void {
        const writer = pp.writer;
        const parser = pp.parser;

        const constant = c.pm_constant_pool_id_to_constant(&parser.*.constant_pool, constant_id);

        const msg = constant.*.start[0..(constant.*.length)];

        try writer.print(":{s}", .{ msg });
    }
};

fn pp_location(pp: *PP, loc: *const c.pm_location_t) !void {
    const writer = pp.writer;
    const parser = pp.parser;

    const start = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.start, parser.*.start_line);
    const end = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.end, parser.*.start_line);
    try writer.print("({d},{d})-({d},{d})", .{ start.line, start.column, end.line, end.column });
}

fn pretty_print_node(pp: *PP, node: [*c] const c.pm_node_t) !void {
    const writer = pp.writer;

    switch (node.*.type) {
        c.PM_PROGRAM_NODE => {
            try writer.print("@ ProgramNode (location: ", .{});
            try pp_location(pp, &(node.*.location));
            try writer.print(")\n", .{});

            // Locals
            try pp.flush_prefix();
            try writer.print("+-- locals: [", .{});
            const cast: [*c] const c.pm_program_node_t = @ptrCast(node);
            const local_size = cast.*.locals.size;
            const ids = cast.*.locals.ids[0..local_size];
            for (ids, 0..local_size) |id, index| {
                if (index != 0) {
                    try writer.print(", ", .{});
                }
                try pp.pp_constant(id);
            }
            try writer.print("]\n", .{});

            // statements
            try pp.flush_prefix();
            try writer.print("+-- statements:\n", .{});
            try pp.push_prefix("    ");
            try pp.flush_prefix();
            try pretty_print_node(pp, @ptrCast(cast.*.statements));
            pp.pop_prefix();
        },
        else => {
            std.debug.print("Need to handle node type: {s}\n", .{c.pm_node_type_to_str(node.*.type)});
        }
    }
    return;
}

pub fn prism_pp(buf: *std.ArrayList(u8), parser: [*c] const c.pm_parser_t, node: [*c] const c.pm_node_t) !void {
    var pp = PP {
        .writer = buf.writer(),
        .prefix = std.ArrayList(u8).init(buf.allocator),
        .prefix_lengths = std.ArrayList(usize).init(buf.allocator),
        .parser = parser,
    };
    try pretty_print_node(&pp, node);
    return;
}
