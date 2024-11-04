//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");
const c = @cImport({
    @cInclude("prism.h");
});
const Allocator = std.mem.Allocator;

pub const pm_scope_node_t = extern struct {
    base: c.pm_node_t,
    previous: ?*const pm_scope_node_t,
    ast_node: [*c]const c.pm_node_t,
    parameters: [*c]const c.pm_node_t,
    body: [*c]const c.pm_node_t,
    locals: c.pm_constant_id_list_t,
};

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
            c.PM_ARGUMENTS_NODE => "@ ArgumentsNode (location: ",
            c.PM_CALL_NODE => "@ CallNode (location: ",
            c.PM_CLASS_NODE => "@ ClassNode (location: ",
            c.PM_CONSTANT_READ_NODE => "@ ConstantReadNode (location: ",
            c.PM_DEF_NODE => "@ DefNode (location: ",
            c.PM_LOCAL_VARIABLE_READ_NODE => "@ LocalVariableReadNode (location: ",
            c.PM_LOCAL_VARIABLE_WRITE_NODE => "@ LocalVariableWriteNode (location: ",
            c.PM_PARAMETERS_NODE => "@ ParametersNode (location: ",
            c.PM_PROGRAM_NODE => "@ ProgramNode (location: ",
            c.PM_REQUIRED_KEYWORD_PARAMETER_NODE => "@ RequiredKeywordParameterNode (location: ",
            c.PM_REQUIRED_PARAMETER_NODE => "@ RequiredParameterNode (location: ",
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
            c.PM_ARGUMENTS_NODE => try pp.visitArgumentsNode(@ptrCast(node)),
            c.PM_CALL_NODE => try pp.visitCallNode(@ptrCast(node)),
            c.PM_CLASS_NODE => try pp.visitClassNode(@ptrCast(node)),
            c.PM_CONSTANT_READ_NODE => try pp.visitConstantReadNode(@ptrCast(node)),
            c.PM_DEF_NODE => try pp.visitDefNode(@ptrCast(node)),
            c.PM_PARAMETERS_NODE => try pp.visitParametersNode(@ptrCast(node)),
            c.PM_PROGRAM_NODE => try pp.visitProgramNode(@ptrCast(node)),
            c.PM_REQUIRED_KEYWORD_PARAMETER_NODE => try pp.visitRequiredKeywordParameterNode(@ptrCast(node)),
            c.PM_REQUIRED_PARAMETER_NODE => try pp.visitRequiredParameterNode(@ptrCast(node)),
            c.PM_STATEMENTS_NODE => try pp.visitStatementsNode(@ptrCast(node)),
            else => {
                std.debug.print("Need to handle node type: {s}\n", .{c.pm_node_type_to_str(node.*.type)});
            },
        }
        return;
    }

    fn visitArgumentsNode(pp: *PP, cast: [*c]const c.pm_arguments_node_t) !void {
        _ = pp;
        _ = cast;
    }

    fn visitCallNode(pp: *PP, cast: [*c]const c.pm_call_node_t) !void {
        // flags
        const bits = [_]u8{
            c.PM_CALL_NODE_FLAGS_SAFE_NAVIGATION,
            c.PM_CALL_NODE_FLAGS_VARIABLE_CALL,
            c.PM_CALL_NODE_FLAGS_ATTRIBUTE_WRITE,
            c.PM_CALL_NODE_FLAGS_IGNORE_VISIBILITY};

        const labels = [_][]const u8 {
            " safe_navigation",
            " variable_call",
            " attribute_write",
            " ignore_visibility",
        };

        var found = false;

        try pp.print_header("+-- CallNodeFlags:");
        for (bits, 0..bits.len) |item, index| {
            if ((cast.*.base.flags & item) > 0) {
                if (found) try pp.writer.print(",", .{});
                try pp.writer.print("{s}", .{ labels[index] });
                found = true;
            }
        }

        if (!found) try pp.writer.print(" nil", .{});
        try pp.writer.print("\n", .{});

        // receiver
        try pp.print_header("+-- receiver:");
        try pp.print_child_or_nil(@ptrCast(cast.*.receiver));

        // call_operator_loc
        try pp.print_loc_with_source("+-- call_operator_loc: ", &cast.*.call_operator_loc);

        // name
        try pp.print_header("+-- name: ");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});

        // message_loc
        try pp.print_loc_with_source("+-- message_loc: ", &cast.*.message_loc);

        // opening_loc
        try pp.print_loc_with_source("+-- opening_loc: ", &cast.*.opening_loc);

        // arguments
        try pp.print_header("+-- arguments:");
        try pp.print_child_or_nil(@ptrCast(cast.*.arguments));

        // closing_loc
        try pp.print_loc_with_source("+-- closing_loc: ", &cast.*.closing_loc);

        // block
        try pp.print_header("+-- block:");
        try pp.print_child_or_nil(@ptrCast(cast.*.block));
    }

    fn visitClassNode(pp: *PP, cast: [*c]const c.pm_class_node_t) !void {
        // locals
        try pp.print_header("+-- locals: [");
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
        try pp.print_header("+-- constant_path:");
        try pp.print_child_or_nil(@ptrCast(cast.*.constant_path));

        // inheritance_operator_loc
        try pp.print_loc_with_source("+-- inheritance_operator_loc: ", &cast.*.inheritance_operator_loc);

        // superclass
        try pp.print_header("+-- superclass:");
        try pp.print_child_or_nil(@ptrCast(cast.*.superclass));

        // body
        try pp.print_header("+-- body:");
        try pp.print_child_or_nil(@ptrCast(cast.*.body));

        // end_keyword_loc
        try pp.print_loc_with_source("+-- end_keyword_loc: ", &cast.*.end_keyword_loc);

        // name
        try pp.print_header("+-- name:");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});
    }

    fn visitConstantReadNode(pp: *PP, cast: [*c]const c.pm_constant_read_node_t) !void {
        try pp.print_header("+-- name:");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});
    }

    fn visitDefNode(pp: *PP, cast: [*c]const c.pm_def_node_t) !void {
        // name
        try pp.print_header("+-- name: ");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});

        // name_loc
        try pp.print_loc_with_source("+-- name_loc: ", &cast.*.name_loc);

        // receiver
        try pp.print_header("+-- receiver: ");
        try pp.print_child_or_nil(@ptrCast(cast.*.receiver));

        // parameters
        try pp.print_header("+-- parameters: ");
        try pp.print_child_or_nil(@ptrCast(cast.*.parameters));

        // body
        try pp.print_header("+-- body: ");
        try pp.print_child_or_nil(@ptrCast(cast.*.body));

        // locals
        try pp.print_header("+-- locals: [");

        const len = cast.*.locals.size;
        const locals = cast.*.locals.ids[0..len];
        for (locals, 0..len) |local, idx| {
            if (idx != 0) {
                try pp.writer.print(", ", .{});
            }
            try pp.pp_constant(local);
        }
        try pp.writer.print("]\n", .{});

        // def_keyword_loc
        try pp.print_loc_with_source("+-- def_keyword_loc: ", &cast.*.def_keyword_loc);

        // operator_loc
        try pp.print_loc_with_source("+-- operator_loc: ", &cast.*.operator_loc);

        // lparen_loc
        try pp.print_loc_with_source("+-- lparen_loc: ", &cast.*.lparen_loc);

        // rparen_loc
        try pp.print_loc_with_source("+-- rparen_loc: ", &cast.*.rparen_loc);

        // equal_loc
        try pp.print_loc_with_source("+-- equal_loc: ", &cast.*.equal_loc);

        // end_keyword_loc
        try pp.print_loc_with_source("+-- end_keyword_loc: ", &cast.*.end_keyword_loc);
    }

    fn visitParametersNode(pp: *PP, cast: [*c]const c.pm_parameters_node_t) !void {
        // requireds
        try pp.print_header("+-- requireds:");
        try pp.writer.print(" (length: {d})\n", .{cast.*.requireds.size});
        try pp.print_node_list(cast.*.requireds.nodes, cast.*.requireds.size);

        // optionals
        try pp.print_header("+-- optionals:");
        try pp.writer.print(" (length: {d})\n", .{cast.*.optionals.size});
        try pp.print_node_list(cast.*.optionals.nodes, cast.*.optionals.size);

        // rest
        try pp.print_header("+-- rest:");
        try pp.print_child_or_nil(cast.*.rest);

        // posts
        try pp.print_header("+-- posts:");
        try pp.writer.print(" (length: {d})\n", .{cast.*.posts.size});
        try pp.print_node_list(cast.*.posts.nodes, cast.*.posts.size);

        // keywords
        try pp.print_header("+-- keywords:");
        try pp.writer.print(" (length: {d})\n", .{cast.*.keywords.size});
        try pp.print_node_list(cast.*.keywords.nodes, cast.*.keywords.size);

        // keyword_rest
        try pp.print_header("+-- keyword_rest:");
        try pp.print_child_or_nil(cast.*.keyword_rest);

        // block
        try pp.print_header("+-- block:");
        try pp.print_child_or_nil(@ptrCast(cast.*.block));
    }

    fn visitProgramNode(pp: *PP, cast: [*c]const c.pm_program_node_t) !void {
        // Locals
        try pp.print_header("+-- locals: [");
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
        try pp.print_header("+-- statements:\n");
        try pp.push_prefix("    ");
        defer pp.pop_prefix();
        try pp.flush_prefix();
        try pp.print_node(@ptrCast(cast.*.statements));
    }

    fn visitRequiredKeywordParameterNode(pp: *PP, cast: [*c]const c.pm_required_keyword_parameter_node_t) !void {
        // ParameterFlags
        try pp.print_header("+-- ParameterFlags:");
        if ((cast.*.base.flags & c.PM_PARAMETER_FLAGS_REPEATED_PARAMETER) > 0) {
            try pp.writer.print(" repeated_parameter\n", .{});
        } else {
            try pp.writer.print(" nil\n", .{});
        }

        // name
        try pp.print_header("+-- name: ");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});

        // name_loc
        try pp.print_loc_with_source("+-- name_loc: ", &cast.*.name_loc);
    }

    fn visitRequiredParameterNode(pp: *PP, cast: [*c]const c.pm_required_parameter_node_t) !void {
        // ParameterFlags
        try pp.print_header("+-- ParameterFlags:");
        if ((cast.*.base.flags & c.PM_PARAMETER_FLAGS_REPEATED_PARAMETER) > 0) {
            try pp.writer.print(" repeated_parameter\n", .{});
        } else {
            try pp.writer.print(" nil\n", .{});
        }

        // name
        try pp.print_header("+-- name: ");
        try pp.pp_constant(cast.*.name);
        try pp.writer.print("\n", .{});
    }

    fn visitStatementsNode(pp: *PP, cast: [*c]const c.pm_statements_node_t) !void {
        // body
        try pp.print_header("+-- body:");
        try pp.writer.print(" (length: {d})\n", .{cast.*.body.size});

        try pp.print_node_list(cast.*.body.nodes, cast.*.body.size);
    }

    fn print_child_or_nil(pp: *PP, node: ?*const c.pm_node_t) !void {
        if (node == null) {
            try pp.writer.print(" nil\n", .{});
        } else {
            try pp.writer.print("\n", .{});
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.print_node(node);
        }
    }

    fn print_location(pp: *PP, loc: *const c.pm_location_t) !void {
        const parser = pp.parser;

        const start = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.start, parser.*.start_line);
        const end = c.pm_newline_list_line_column(&parser.*.newline_list, loc.*.end, parser.*.start_line);
        try pp.writer.print("({d},{d})-({d},{d})", .{ start.line, start.column, end.line, end.column });
    }

    fn print_loc_with_source(pp: *PP, name: []const u8, location: *const c.pm_location_t) !void{
        try pp.print_header(name);
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

    fn print_node_list(pp: *PP, list: [*c][*c]c.pm_node_t, length: usize) !void {
        if (length == 0) {
            return;
        }

        for (list[0..length], 0..length) |child, idx| {
            try pp.push_prefix("|   ");
            defer pp.pop_prefix();
            try pp.flush_prefix();
            try pp.writer.print("+-- ", .{});
            try pp.push_prefix(if (idx == (length - 1))
                "    "
                else
                "|   ");
            defer pp.pop_prefix();

            try pp.print_node(@ptrCast(child));
        }
    }

    fn print_header(pp: *PP, str: []const u8) !void {
        try pp.flush_prefix();
        try pp.writer.print("{s}", .{ str });
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

pub fn scope_node_init(node: [*c]const c.pm_node_t, scope: *pm_scope_node_t, prev: ?*pm_scope_node_t) !void {
    scope.*.previous = prev;
    scope.*.ast_node = node;

    switch (node.*.type) {
        c.PM_PROGRAM_NODE => {
            const cast: [*c]const c.pm_program_node_t = @ptrCast(node);
            scope.*.body = @ptrCast(cast.*.statements);
            scope.*.locals = cast.*.locals;
        },
        else => {
            std.debug.print("unknown type {s}\n", .{c.pm_node_type_to_str(node.*.type)});
            return error.NotImplementedError;
        }
    }
}
