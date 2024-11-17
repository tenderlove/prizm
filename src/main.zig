const std = @import("std");
const prism = @import("prism.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const ssa = @import("ssa.zig");
const CFG = @import("cfg.zig");
const Allocator = std.mem.Allocator;
const yazap = @import("yazap");
const App = yazap.App;
const Arg = yazap.Arg;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var app = App.init(allocator, "prizm", "Prizm");
    defer app.deinit();

    var prizm = app.rootCommand();
    var runcmd = app.createCommand("run", "Run script");
    try runcmd.addArg(Arg.positional("FILE", null, null));

    try prizm.addSubcommand(runcmd);

    const matches = try app.parseProcess();

    if (matches.subcommandMatches("run")) |runcmd_matches| {
        if (runcmd_matches.getSingleValue("FILE")) |f| {
            std.log.info("run file {s}", .{ f });
            return;
        }
    }

    return;

    // Get the command line args
    //const args = try std.process.argsAlloc(allocator);
    //defer std.process.argsFree(allocator, args);

    //if (args.len == 2) {
    //    var path_buffer: [std.fs.max_path_bytes]u8 = undefined;
    //    const path = try std.fs.realpathZ(args.ptr[1], &path_buffer);

    //    // Read the file in
    //    const file = try std.fs.openFileAbsolute(path, .{});
    //    const file_size = (try file.stat()).size;
    //    defer file.close();

    //    const src = try file.readToEndAlloc(allocator, file_size);

    //    // Parse the file
    //    const parser = try prism.Prism.newParserCtx(allocator);
    //    parser.init(src, file_size, null);
    //    defer parser.deinit();

    //    const root_node = parser.parse();
    //    defer parser.nodeDestroy(root_node);

    //    var scope_node = try prism.pmNewScopeNode(root_node);

    //    // Create a new VM
    //    const machine = try vm.init(allocator);
    //    defer machine.deinit(allocator);

    //    // Compile the parse tree
    //    const cc = try compiler.init(allocator, machine, parser);
    //    defer cc.deinit(allocator);

    //    const scope = try cc.compile(&scope_node);
    //    const cfg = try CFG.buildCFG(allocator, scope.insns);

    //    _ = cfg;
    //    // try machine.eval(iseq);

    //}
    //else {
    //    return;
    //}
}

test {
    @import("std").testing.refAllDecls(@This());
}
