//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const c = @cImport({
    @cInclude("prism.h");
});
const Allocator = std.mem.Allocator;

const PP = struct {
    writer: std.ArrayList(u8).Writer,
    prefix: std.ArrayList(u8),
    prefix_lengths: std.ArrayList(usize),
    parser: [*c]const c.pm_parser_t,

    fn flush_prefix(pp: *PP) !void {
        try pp.writer.print("{s}", .{pp.prefix.items});
    }

    fn push_prefix(pp: *PP, str: []const u8) !void {
        try pp.prefix.appendSlice(str);
        try pp.prefix_lengths.append(str.len);
    }

    fn pop_prefix(pp: *PP) void {
        pp.prefix.items.len -= pp.prefix_lengths.pop();
    }

    const bytelut_ruby = [_][]const u8{
        "\\a",
        "\\b",
        "\\t",
        "\\n",
        "\\v",
        "\\f",
        "\\r",
    };

    fn append_source(pp: *PP, start: [*c]const u8, len: usize) !void {
        const bytes = start[0..len];
        for (bytes) |byte| {
            if ((byte <= 0x06) or (byte >= 0x0E and byte <= 0x1F) or (byte >= 0x7F)) {
                try pp.writer.print("{x:02}", .{byte});
            } else {
                if (byte >= 7 and byte <= 13) {
                    try pp.writer.print("{s}", .{bytelut_ruby[byte - 7]});
                } else {
                    switch (byte) {
                        '"' => try pp.writer.print("\\\"", .{}),
                        '#' => try pp.writer.writeByte('#'),
                        '\\' => try pp.writer.print("\\\\", .{}),
                        else => try pp.writer.writeByte(byte),
                    }
                }
            }
        }
    }

    fn pp_constant(pp: *PP, constant_id: u32) !void {
        const writer = pp.writer;
        const parser = pp.parser;

        const constant = c.pm_constant_pool_id_to_constant(&parser.*.constant_pool, constant_id);

        const msg = constant.*.start[0..(constant.*.length)];

        try writer.print(":{s}", .{msg});
    }

    fn writeNodeName(pp: *PP, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!void {
        const name = switch (node.*.type) {
            c.PM_CALL_NODE => "@ CallNode (location: ",
            c.PM_CLASS_NODE => "@ ClassNode (location: ",
            c.PM_CONSTANT_READ_NODE => "@ ConstantReadNode (location: ",
            c.PM_DEF_NODE => "@ DefNode (location: ",
            c.PM_LOCAL_VARIABLE_WRITE_NODE => "@ LocalVariableWriteNode (location: ",
            c.PM_PARAMETERS_NODE => "@ ParametersNode (location: ",
            c.PM_PROGRAM_NODE => "@ ProgramNode (location: ",
            c.PM_STATEMENTS_NODE => "@ StatementsNode (location: ",
            else => {
                std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
                return error.NotImplementedError;
            },
        };
        try pp.writer.print("{s}", .{name});
        try pp.print_location(&(node.*.location));
        try pp.writer.print(")\n", .{});
    }

    fn print_node(pp: *PP, node: [*c]const c.pm_node_t) error{NotImplementedError, OutOfMemory}!void {
        try pp.writeNodeName(node);

        switch (node.*.type) {
            c.PM_CLASS_NODE => try pp.visitClassNode(@ptrCast(node)),
            c.PM_CONSTANT_READ_NODE => try pp.visitConstantReadNode(@ptrCast(node)),
            c.PM_DEF_NODE => try pp.visitDefNode(@ptrCast(node)),
            c.PM_PROGRAM_NODE => try pp.visitProgramNode(@ptrCast(node)),
            c.PM_STATEMENTS_NODE => try pp.visitStatementsNode(@ptrCast(node)),
            else => {
                std.debug.print("Need to handle node type: {s}\n", .{c.pm_node_type_to_str(node.*.type)});
            },
        }
        return;
    }

    fn visitProgramNode(pp: *PP, cast: [*c]const c.pm_program_node_t) !void {
        // Locals
        try pp.flush_prefix();
        try pp.writer.print("+-- locals: [", .{});
        const local_size = cast.*.locals.size;
        const ids = cast.*.locals.ids[0..local_size];
        for (ids, 0..local_size) |id, index| {
            if (index != 0) {
                try pp.writer.print(", ", .{});
            }
            try pp.pp_constant(id);
        }
        try pp.writer.print("]\n", .{});

        // statements
        try pp.flush_prefix();
        try pp.writer.print("+-- statements:\n", .{});
        try pp.push_prefix("    ");
        try pp.flush_prefix();
        try pp.print_node(@ptrCast(cast.*.statements));
        pp.pop_prefix();
    }

    fn visitStatementsNode(pp: *PP, cast: [*c]const c.pm_statements_node_t) !void {
        // body
        try pp.flush_prefix();
        try pp.writer.print("+-- body:", .{});
        try pp.writer.print(" (length: {d})\n", .{cast.*.body.size});

        const last_index = cast.*.body.size;

        const nodes = cast.*.body.nodes[0..last_index];
        for (nodes, 0..last_index) |child, idx| {
            try pp.push_prefix("    ");
            defer pp.pop_prefix();

            try pp.flush_prefix();
            try pp.writer.print("+-- ", .{});
            try pp.push_prefix(if (idx == (last_index - 1))
                "    "
                else
                "|   ");
            defer pp.pop_prefix();

            try pp.print_node(@ptrCast(child));
        }
    }

    fn visitClassNode(pp: *PP, cast: [*c]const c.pm_class_node_t) !void {
        // locals
        try pp.flush_prefix();
        try pp.writer.print("+-- locals: [", .{});
        const local_size = cast.*.locals.size;
        const ids = cast.*.locals.ids[0..local_size];
        for (ids, 0..local_size) |id, index| {
            if (index != 0) {
                try pp.writer.print(", ", .{});
            }
            try pp.pp_constant(id);
        }
        try pp.writer.print("]\n", .{});

        // class_keyword_loc
        try pp.print_loc_with_source("+-- class_keyword_loc: ", &cast.*.class_keyword_loc);

        // constant_path
        {
            try pp.flush_prefix();
            try pp.writer.print("+-- constant_path:\n", .{});
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.print_node(@ptrCast(cast.*.constant_path));
        }

        // inheritance_operator_loc
        try pp.print_loc_with_source("+-- inheritance_operator_loc: ", &cast.*.inheritance_operator_loc);

        // superclass
        {
            try pp.flush_prefix();
            try pp.writer.print("+-- superclass:", .{});
            if (cast.*.superclass == null) {
                try pp.writer.print(" nil\n", .{});
            } else {
                try pp.writer.print("\n", .{});
                try pp.push_prefix("|   ");
                defer pp.pop_prefix();
                try pp.flush_prefix();
                try pp.print_node(@ptrCast(cast.*.superclass));
            }
        }

        // body
        {
            try pp.flush_prefix();
            try pp.writer.print("+-- body:", .{});
            if (cast.*.body == null) {
                try pp.writer.print(" nil\n", .{});
            } else {
                try pp.writer.print("\n", .{});
                try pp.push_prefix("|   ");
                defer pp.pop_prefix();
                try pp.flush_prefix();
                try pp.print_node(@ptrCast(cast.*.body));
            }
        }

        // end_keyword_loc
        try pp.print_loc_with_source("+-- end_keyword_loc: ", &cast.*.end_keyword_loc);

        // name
        {
            try pp.flush_prefix();
            try pp.writer.print("+-- name: ", .{});
            try pp.pp_constant(cast.*.name);
            try pp.writer.print("\n", .{});
        }
    }

    fn visitConstantReadNode(pp: *PP, cast: [*c]const c.pm_constant_read_node_t) !void {
        try pp.flush_prefix();
        try pp.writer.print("+-- name: ", .{});
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});
    }

    fn visitDefNode(pp: *PP, cast: [*c]const c.pm_def_node_t) !void {
        try pp.flush_prefix();

        // name
        try pp.writer.print("+-- name: ", .{});
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});

        // name_loc
        try pp.print_loc_with_source("+-- name_loc: ", &cast.*.name_loc);

        // receiver
        try pp.flush_prefix();
        try pp.writer.print("+-- receiver: ", .{});
        if (cast.*.receiver == null) {
            try pp.writer.print("nil\n", .{});
        } else {
            try pp.writer.print("\n", .{});
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.print_node(@ptrCast(cast.*.receiver));
        }

        // parameters
        try pp.flush_prefix();
        try pp.writer.print("+-- parameters: ", .{});
        if (cast.*.parameters == null) {
            try pp.writer.print("nil\n", .{});
        } else {
            try pp.writer.print("\n", .{});
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.print_node(@ptrCast(cast.*.parameters));
        }

        // body
        try pp.flush_prefix();
        try pp.writer.print("+-- body: ", .{});
        if (cast.*.body == null) {
            try pp.writer.print("nil\n", .{});
        } else {
            try pp.writer.print("\n", .{});
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.print_node(@ptrCast(cast.*.body));
        }
    }

    fn print_location(pp: *PP, loc: *const c.pm_location_t) !void {
        const parser = pp.parser;

        const start = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.start, parser.*.start_line);
        const end = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.end, parser.*.start_line);
        try pp.writer.print("({d},{d})-({d},{d})", .{ start.line, start.column, end.line, end.column });
    }

    fn print_loc_with_source(pp: *PP, name: []const u8, location: *const c.pm_location_t) !void{
        try pp.flush_prefix();
        try pp.writer.print("{s}", .{ name });
        if (location.start == null) {
            try pp.writer.print("nil\n", .{});
        } else {
            try pp.print_location(location);
            try pp.writer.print(" = \"", .{});
            const len = location.end - location.start;
            try pp.append_source(location.start, len);
            try pp.writer.print("\"\n", .{});
        }
    }
};

pub fn prism_pp(buf: *std.ArrayList(u8), parser: [*c]const c.pm_parser_t, node: [*c]const c.pm_node_t) !void {
    var pp = PP{
        .writer = buf.writer(),
        .prefix = std.ArrayList(u8).init(buf.allocator),
        .prefix_lengths = std.ArrayList(usize).init(buf.allocator),
        .parser = parser,
    };
    try pp.print_node(node);
    return;
}
