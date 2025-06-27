const std = @import("std");

const bitmatrix = @import("utils/bitmatrix.zig");
const SymmetricMatrix = bitmatrix.SymmetricMatrix;
const BitMatrix = bitmatrix.BitMatrix;
const BitMap = std.DynamicBitSetUnmanaged;

pub const InterferenceGraph = struct {
    const Self = @This();

    edges: *SymmetricMatrix,
    adjacency: *BitMatrix,
    node_count: usize,

    pub fn init(allocator: std.mem.Allocator, node_count: usize) !*Self {
        const self = try allocator.create(Self);
        self.edges = try SymmetricMatrix.init(allocator, node_count, node_count);
        self.adjacency = try BitMatrix.init(allocator, node_count, node_count);
        self.node_count = node_count;
        return self;
    }

    pub fn size(self: *const Self) usize {
        return self.node_count;
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

    pub fn degree(self: *const Self, node: usize) usize {
        return self.getNeighbors(node).count();
    }

    pub fn removeNode(self: *const Self, node: usize) void {
        // Get all neighbors before removing edges
        var neighbor_iter = self.neighborIterator(node);

        // Remove all edges connected to this node
        while (neighbor_iter.next()) |neighbor| {
            self.remove(node, neighbor);
        }
    }

    pub fn clone(self: *const Self, allocator: std.mem.Allocator) !*Self {
        const cloned = try allocator.create(Self);
        cloned.edges = try self.edges.clone(allocator);
        cloned.adjacency = try self.adjacency.clone(allocator);
        cloned.node_count = self.node_count;
        return cloned;
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

test "InterferenceGraph degree calculation" {
    const allocator = std.testing.allocator;

    var graph = try InterferenceGraph.init(allocator, 5);
    defer graph.deinit(allocator);

    // Initially all nodes have degree 0
    try std.testing.expectEqual(0, graph.degree(0));
    try std.testing.expectEqual(0, graph.degree(1));
    try std.testing.expectEqual(0, graph.degree(2));
    try std.testing.expectEqual(0, graph.degree(3));
    try std.testing.expectEqual(0, graph.degree(4));

    // Create interference pattern:
    // Node 0 interferes with: 1, 2, 3 (degree = 3)
    // Node 1 interferes with: 0, 4 (degree = 2)
    // Node 2 interferes with: 0 (degree = 1)
    // Node 3 interferes with: 0 (degree = 1)
    // Node 4 interferes with: 1 (degree = 1)

    graph.add(0, 1);
    graph.add(0, 2);
    graph.add(0, 3);
    graph.add(1, 4);

    // Test degrees
    try std.testing.expectEqual(3, graph.degree(0)); // connected to 1, 2, 3
    try std.testing.expectEqual(2, graph.degree(1)); // connected to 0, 4
    try std.testing.expectEqual(1, graph.degree(2)); // connected to 0
    try std.testing.expectEqual(1, graph.degree(3)); // connected to 0
    try std.testing.expectEqual(1, graph.degree(4)); // connected to 1
}

test "InterferenceGraph node removal" {
    const allocator = std.testing.allocator;

    var graph = try InterferenceGraph.init(allocator, 5);
    defer graph.deinit(allocator);

    // Create interference pattern:
    // Node 0 interferes with: 1, 2, 3 (degree = 3)
    // Node 1 interferes with: 0, 4 (degree = 2)
    // Node 2 interferes with: 0 (degree = 1)
    // Node 3 interferes with: 0 (degree = 1)
    // Node 4 interferes with: 1 (degree = 1)

    graph.add(0, 1);
    graph.add(0, 2);
    graph.add(0, 3);
    graph.add(1, 4);

    // Verify initial degrees
    try std.testing.expectEqual(3, graph.degree(0));
    try std.testing.expectEqual(2, graph.degree(1));
    try std.testing.expectEqual(1, graph.degree(2));
    try std.testing.expectEqual(1, graph.degree(3));
    try std.testing.expectEqual(1, graph.degree(4));

    // Remove node 0 (which is connected to nodes 1, 2, 3)
    graph.removeNode(0);

    // Node 0 should now have degree 0
    try std.testing.expectEqual(0, graph.degree(0));

    // Connected nodes should have their degrees reduced
    try std.testing.expectEqual(1, graph.degree(1)); // was 2, now 1 (lost connection to 0)
    try std.testing.expectEqual(0, graph.degree(2)); // was 1, now 0 (lost connection to 0)
    try std.testing.expectEqual(0, graph.degree(3)); // was 1, now 0 (lost connection to 0)
    try std.testing.expectEqual(1, graph.degree(4)); // unchanged (wasn't connected to 0)

    // Verify specific connections are gone
    try std.testing.expect(!graph.interferes(0, 1));
    try std.testing.expect(!graph.interferes(0, 2));
    try std.testing.expect(!graph.interferes(0, 3));

    // Verify remaining connection is intact
    try std.testing.expect(graph.interferes(1, 4));
}

test "InterferenceGraph clone" {
    const allocator = std.testing.allocator;

    var original = try InterferenceGraph.init(allocator, 4);
    defer original.deinit(allocator);

    // Create a pattern in the original graph
    original.add(0, 1);
    original.add(0, 2);
    original.add(1, 3);

    // Clone the graph
    var cloned = try original.clone(allocator);
    defer cloned.deinit(allocator);

    // Verify the clone has the same structure
    try std.testing.expectEqual(original.size(), cloned.size());
    try std.testing.expectEqual(original.edgeCount(), cloned.edgeCount());

    // Verify all edges are copied
    try std.testing.expect(cloned.interferes(0, 1));
    try std.testing.expect(cloned.interferes(0, 2));
    try std.testing.expect(cloned.interferes(1, 3));

    // Verify non-edges are not present
    try std.testing.expect(!cloned.interferes(0, 3));
    try std.testing.expect(!cloned.interferes(1, 2));
    try std.testing.expect(!cloned.interferes(2, 3));

    // Verify degrees match
    try std.testing.expectEqual(original.degree(0), cloned.degree(0)); // 2
    try std.testing.expectEqual(original.degree(1), cloned.degree(1)); // 2
    try std.testing.expectEqual(original.degree(2), cloned.degree(2)); // 1
    try std.testing.expectEqual(original.degree(3), cloned.degree(3)); // 1

    // Verify independence: modify original, clone should be unchanged
    original.add(2, 3);
    try std.testing.expect(original.interferes(2, 3));
    try std.testing.expect(!cloned.interferes(2, 3)); // Clone unchanged

    // Verify independence: modify clone, original should be unchanged
    cloned.removeNode(0);
    try std.testing.expectEqual(0, cloned.degree(0));
    try std.testing.expectEqual(2, original.degree(0)); // Original unchanged
}
