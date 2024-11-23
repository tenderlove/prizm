const std = @import("std");
const prism = @import("prism.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");
const ssa = @import("ssa.zig");
const CFG = @import("cfg.zig");
const Allocator = std.mem.Allocator;
const yazap = @import("yazap");
const printer = @import("printer.zig");
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

    var ircmd = app.createCommand("ir", "display IR");
    try ircmd.addArg(Arg.positional("FILE", null, null));
    try prizm.addSubcommand(ircmd);

    var cfgcmd = app.createCommand("cfg", "display CFG");
    try cfgcmd.addArg(Arg.positional("FILE", null, null));
    try prizm.addSubcommand(cfgcmd);

    const matches = try app.parseProcess();

    if (matches.subcommandMatches("run")) |runcmd_matches| {
        if (runcmd_matches.getSingleValue("FILE")) |path| {
            std.log.info("run file {s}", .{ path });

            // Read the file in
            const file = try std.fs.cwd().openFile(path, .{});
            const file_size = (try file.stat()).size;
            defer file.close();

            const src = try file.readToEndAlloc(allocator, file_size);

            // Parse the file
            const parser = try prism.Prism.newParserCtx(allocator);
            parser.init(src, file_size, null);
            defer parser.deinit();

            const root_node = parser.parse();
            defer parser.nodeDestroy(root_node);

            var scope_node = try prism.pmNewScopeNode(root_node);

            // Create a new VM
            const machine = try vm.init(allocator);
            defer machine.deinit(allocator);

            // Compile the parse tree
            const cc = try compiler.init(allocator, machine, parser);
            defer cc.deinit(allocator);

            const scope = try cc.compile(&scope_node);
            const cfg = try CFG.buildCFG(allocator, scope);

            _ = cfg;

            return;
        }
    }

    if (matches.subcommandMatches("ir")) |runcmd_matches| {
        if (runcmd_matches.getSingleValue("FILE")) |path| {
            std.log.info("print IR for {s}", .{ path });

            // Read the file in
            const file = try std.fs.cwd().openFile(path, .{});
            const file_size = (try file.stat()).size;
            defer file.close();

            const src = try file.readToEndAlloc(allocator, file_size);

            // Parse the file
            const parser = try prism.Prism.newParserCtx(allocator);
            parser.init(src, file_size, null);
            defer parser.deinit();

            const root_node = parser.parse();
            defer parser.nodeDestroy(root_node);

            var scope_node = try prism.pmNewScopeNode(root_node);

            // Create a new VM
            const machine = try vm.init(allocator);
            defer machine.deinit(allocator);

            // Compile the parse tree
            const cc = try compiler.init(allocator, machine, parser);
            defer cc.deinit(allocator);

            const scope = try cc.compile(&scope_node);
            try printer.printIR(allocator, scope, std.io.getStdOut().writer().any());

            return;
        }
    }

    if (matches.subcommandMatches("cfg")) |runcmd_matches| {
        if (runcmd_matches.getSingleValue("FILE")) |path| {
            std.log.info("print CFG for {s}", .{ path });

            // Read the file in
            const file = try std.fs.cwd().openFile(path, .{});
            const file_size = (try file.stat()).size;
            defer file.close();

            const src = try file.readToEndAlloc(allocator, file_size);

            // Parse the file
            const parser = try prism.Prism.newParserCtx(allocator);
            parser.init(src, file_size, null);
            defer parser.deinit();

            const root_node = parser.parse();
            defer parser.nodeDestroy(root_node);

            var scope_node = try prism.pmNewScopeNode(root_node);

            // Create a new VM
            const machine = try vm.init(allocator);
            defer machine.deinit(allocator);

            // Compile the parse tree
            const cc = try compiler.init(allocator, machine, parser);
            defer cc.deinit(allocator);

            const scope = try cc.compile(&scope_node);
            try printer.printCFG(allocator, scope, std.io.getStdOut().writer().any());

            return;
        }
    }

    return;
}

test {
    @import("std").testing.refAllDecls(@This());
}
