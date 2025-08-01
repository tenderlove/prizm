const std = @import("std");

pub fn addPrismSource(b: *std.Build,
    exe: *std.Build.Step.Compile,
    prism_path: []const u8) void
{
    var iter_dir = std.fs.cwd().openDir(
        prism_path,
        .{ .iterate = true },
    ) catch return;

    defer {
        iter_dir.close();
    }
    var it = iter_dir.iterate();
    while (it.next() catch return) |file| {
        if (file.kind == .directory) {
            const dirname = std.fs.path.join(b.allocator, &.{
                prism_path,
                file.name
            }) catch return;
            addPrismSource(b, exe, dirname);
        }

        if (file.kind != .file) {
            continue;
        }

        const fname = std.fs.path.join(b.allocator, &.{
            prism_path,
            file.name
        }) catch return;

        exe.addCSourceFile(.{ .file = .{ .cwd_relative = fname } });
    }
}

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    const rake = b.addSystemCommand(&.{"rake"});
    const prism_path = std.fs.path.join(b.allocator, &.{b.build_root.path orelse "",
        "prism"
    }) catch return;

    rake.addArg("-C");
    rake.addArg(prism_path);
    rake.addArg("templates");

    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("zigtest", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Import yazap
    const yazap = b.dependency("yazap", .{ });

    const exe = b.addExecutable(.{
        .name = "prizm",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zigtest", .module = mod },
                .{ .name = "yazap", .module = yazap.module("yazap") },
            },
        }),
    });

    exe.addIncludePath(b.path("prism/include"));
    addPrismSource(b, exe, "prism/src");
    exe.linkLibC();

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zigtest", .module = mod },
                .{ .name = "yazap", .module = yazap.module("yazap") },
            },
        }),
    });

    lib_unit_tests.step.dependOn(&rake.step);
    lib_unit_tests.addIncludePath(b.path("prism/include"));
    addPrismSource(b, lib_unit_tests, "prism/src");
    lib_unit_tests.linkLibC();

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "zigtest", .module = mod },
                .{ .name = "yazap", .module = yazap.module("yazap") },
            },
        }),
    });

    exe_unit_tests.step.dependOn(&rake.step);
    exe_unit_tests.addIncludePath(b.path("prism/include"));
    addPrismSource(b, exe_unit_tests, "prism/src");
    exe_unit_tests.linkLibC();

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
