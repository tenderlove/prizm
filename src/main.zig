const std = @import("std");
const prism = @import("prism.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const ssa = @import("ssa.zig");
const Allocator = std.mem.Allocator;

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

        const parser = try prism.newParserCtx(allocator);
        prism.initParser(parser, src, file_size, null);
        defer prism.parserDealloc(parser);

        const root_node = prism.pmParse(parser);
        defer prism.pmNodeDestroy(parser, root_node);

        var scope_node = try prism.pmNewScopeNode(root_node);

        const machine = try vm.init(allocator);
        defer machine.deinit(allocator);

        const cc = try compiler.init(allocator, machine, parser);
        defer cc.deinit(allocator);

        const iseq = try cc.compile(@ptrCast(&scope_node));

        _ = iseq;
        // try machine.eval(iseq);

    }
    else {
        return;
    }
}

test {
    @import("std").testing.refAllDecls(@This());
}
