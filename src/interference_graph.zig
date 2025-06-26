const std = @import("std");

const bitmatrix = @import("utils/bitmatrix.zig");
const SymmetricMatrix = bitmatrix.SymmetricMatrix;
const BitMatrix = bitmatrix.BitMatrix;
const BitMap = std.DynamicBitSetUnmanaged;

pub const InterferenceGraph = struct {
    const Self = @This();

    edges: *SymmetricMatrix,
    adjacency: *BitMatrix,

    pub fn init(allocator: std.mem.Allocator, size: usize) !*Self {
        const self = try allocator.create(Self);
        self.edges = try SymmetricMatrix.init(allocator, size, size);
        self.adjacency = try BitMatrix.init(allocator, size, size);
        return self;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        self.edges.deinit(allocator);
        self.adjacency.deinit(allocator);
        allocator.destroy(self);
    }

    pub fn add(self: *const Self, node1: usize, node2: usize) void {
        self.edges.set(node1, node2);
        self.adjacency.set(node1, node2);
        self.adjacency.set(node2, node1); // Set both directions for adjacency
    }

    pub fn interferes(self: *const Self, node1: usize, node2: usize) bool {
        return self.edges.isSet(node1, node2);
    }

    pub fn remove(self: *const Self, node1: usize, node2: usize) void {
        self.edges.unset(node1, node2);
        self.adjacency.unset(node1, node2);
        self.adjacency.unset(node2, node1); // Remove both directions for adjacency
    }

    pub fn clear(self: *const Self) void {
        self.edges.clear();
        self.adjacency.clear();
    }

    pub fn edgeCount(self: *const Self) usize {
        return self.edges.count();
    }

    pub fn getNeighbors(self: *const Self, node: usize) BitMap {
        return self.adjacency.getColumn(node);
    }

    pub fn neighborIterator(self: *const Self, node: usize) BitMap.Iterator(.{}) {
        return self.getNeighbors(node).iterator(.{});
    }
};

test "InterferenceGraph basic operations" {
    const allocator = std.testing.allocator;
    
    var graph = try InterferenceGraph.init(allocator, 5);
    defer graph.deinit(allocator);
    
    // Initially no interferences
    try std.testing.expect(!graph.interferes(0, 1));
    try std.testing.expect(!graph.interferes(1, 0));
    try std.testing.expectEqual(0, graph.edgeCount());
    
    // Add interference between nodes 0 and 1
    graph.add(0, 1);
    try std.testing.expect(graph.interferes(0, 1));
    try std.testing.expect(graph.interferes(1, 0)); // Should be symmetric
    try std.testing.expectEqual(1, graph.edgeCount()); // SymmetricMatrix only counts once
    
    // Add more interferences
    graph.add(1, 2);
    graph.add(3, 4);
    
    try std.testing.expect(graph.interferes(1, 2));
    try std.testing.expect(graph.interferes(2, 1));
    try std.testing.expect(graph.interferes(3, 4));
    try std.testing.expect(graph.interferes(4, 3));
    
    // Non-interfering pairs
    try std.testing.expect(!graph.interferes(0, 2));
    try std.testing.expect(!graph.interferes(0, 3));
    try std.testing.expect(!graph.interferes(2, 4));
}

test "InterferenceGraph neighbor iteration" {
    const allocator = std.testing.allocator;
    
    var graph = try InterferenceGraph.init(allocator, 5);
    defer graph.deinit(allocator);
    
    // Create interference pattern:
    // Node 0 interferes with: 1, 2, 3
    // Node 1 interferes with: 0, 4
    // Node 2 interferes with: 0
    // Node 3 interferes with: 0
    // Node 4 interferes with: 1
    
    graph.add(0, 1);
    graph.add(0, 2);
    graph.add(0, 3);
    graph.add(1, 4);
    
    // Test neighbor iteration for node 0 (should have neighbors: 1, 2, 3)
    var neighbors = std.ArrayList(usize).init(allocator);
    defer neighbors.deinit();
    
    var iter = graph.neighborIterator(0);
    while (iter.next()) |neighbor| {
        try neighbors.append(neighbor);
    }
    
    try std.testing.expectEqual(3, neighbors.items.len);
    try std.testing.expectEqual(1, neighbors.items[0]);
    try std.testing.expectEqual(2, neighbors.items[1]);
    try std.testing.expectEqual(3, neighbors.items[2]);
    
    // Test neighbor iteration for node 1 (should have neighbors: 0, 4)
    neighbors.clearRetainingCapacity();
    iter = graph.neighborIterator(1);
    while (iter.next()) |neighbor| {
        try neighbors.append(neighbor);
    }
    
    try std.testing.expectEqual(2, neighbors.items.len);
    try std.testing.expectEqual(0, neighbors.items[0]);
    try std.testing.expectEqual(4, neighbors.items[1]);
}
