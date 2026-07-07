const std = @import("std");
const prism = @import("prism.zig");
const Compiler = @import("compiler.zig").Compiler;
const g = @import("globals.zig");
const cfg_z = @import("cfg.zig");
const IonGraph = @import("printers/iongraph.zig").IonGraph;
const ascii_printer = @import("printers/ascii.zig");
const CFG = cfg_z.CFG;
const Scope = @import("scope.zig").Scope;
const Allocator = std.mem.Allocator;
const clap = @import("clap");

const SubCommands = enum {
    run,
    compile,
};

const main_parsers = .{
    .command = clap.parsers.enumeration(SubCommands),
};

const main_params = clap.parseParamsComptime(
    \\-h, --help Display this help and exit
    \\<command>
    \\
);

const MainArgs = clap.ResultEx(clap.Help, &main_params, main_parsers);

pub fn main(init: std.process.Init) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var iter = try init.minimal.args.iterateAllocator(init.gpa);
    defer iter.deinit();

    _ = iter.next();

    // const allocator = arena.allocator();

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &main_params, main_parsers, &iter, .{
        .diagnostic = &diag,
        .allocator = init.gpa,
        .terminating_positional = 0,
    }) catch |err| {
        try diag.reportToFile(init.io, .stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.helpToFile(init.io, .stderr(), clap.Help, &main_params, .{});

    const command = res.positionals[0] orelse return error.MissingCommand;
    switch (command) {
        .run => try runMain(init.io, init.gpa, &iter),
        .compile => try runCompile(init.io, init.gpa, &iter),
    }
}

const CfgFormat = enum { iongraph, ascii };

fn runCompile(io: std.Io, gpa: std.mem.Allocator, iter: *std.process.Args.Iterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help  Display this help and exit.
        \\--ir        Show the IR
        \\--cfg <FORMAT>  Show the CFG in the given format (iongraph or ascii)
        \\<str>       Ruby file to compile
        \\
    );

    const compile_parsers = .{
        .str = clap.parsers.string,
        .FORMAT = clap.parsers.enumeration(CfgFormat),
    };

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, compile_parsers, iter, .{ .diagnostic = &diag, .allocator = gpa }) catch |err| {
        try diag.reportToFile(io, .stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.helpToFile(io, .stderr(), clap.Help, &params, .{});

    const path = res.positionals[0] orelse return error.MissingFile;
    const src = try std.Io.Dir.cwd().readFileAlloc(io, path, gpa, .unlimited);
    defer gpa.free(src);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Parse the file
    const parser = try prism.Prism.newParserCtx(arena.allocator());
    parser.init(src, src.len, null);
    defer parser.deinit();

    const root_node = parser.parse();
    defer parser.nodeDestroy(root_node);

    var scope_node = try prism.pmNewScopeNode(root_node);

    const globals = try g.Globals.init(gpa);
    defer globals.deinit(gpa);

    // Compile the parse tree
    const cc = try Compiler.init(gpa, globals, parser);
    defer cc.deinit(gpa);

    const cfg = try cc.compile(&scope_node);
    defer cfg.deinit();

    if (res.args.cfg) |format| {
        var buf: [4096]u8 = undefined;
        var fw = std.Io.File.stdout().writer(io, &buf);
        const w = &fw.interface;
        switch (format) {
            .iongraph => try IonGraph.print(cfg, gpa, w),
            .ascii => try ascii_printer.print(cfg, w),
        }
        try w.flush();
    }

    return;
}

fn runMain(io: std.Io, gpa: std.mem.Allocator, iter: *std.process.Args.Iterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help  Display this help and exit.
        \\<str>       Ruby file to run
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, iter, .{ .diagnostic = &diag, .allocator = gpa }) catch |err| {
        try diag.reportToFile(io, .stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.helpToFile(io, .stderr(), clap.Help, &params, .{});

    const path = res.positionals[0] orelse return error.MissingFile;
    std.debug.print("run file {s}\n", .{path});
}

fn compileFile(io: std.Io, alloc: std.mem.Allocator, path: []const u8, globals: *g.Globals) !*CFG {
    const src = try std.Io.Dir.cwd().readFileAlloc(io, path, alloc, .unlimited);

    // Parse the file
    const parser = try prism.Prism.newParserCtx(alloc);
    parser.init(src, src.len, null);
    defer parser.deinit();

    const root_node = parser.parse();
    defer parser.nodeDestroy(root_node);

    var scope_node = try prism.pmNewScopeNode(root_node);

    // Compile the parse tree
    const cc = try Compiler.init(alloc, globals, parser);
    defer cc.deinit(alloc);

    return try cc.compile(&scope_node);
}

test {
    @import("std").testing.refAllDecls(@This());
}
