const std = @import("std");

// Disjoint-set union-find data structure for register coalescing
pub const UnionFind = struct {
    parent: []usize,
    rank: []u8,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, size: usize) !UnionFind {
        const parent = try allocator.alloc(usize, size);
        const rank = try allocator.alloc(u8, size);
        
        // Initialize each element as its own set
        for (parent, 0..) |*p, i| {
            p.* = i;
        }
        @memset(rank, 0);
        
        return UnionFind{
            .parent = parent,
            .rank = rank,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *UnionFind) void {
        self.allocator.free(self.parent);
        self.allocator.free(self.rank);
    }

    // Find with path compression
    pub fn find(self: *UnionFind, x: usize) usize {
        if (self.parent[x] != x) {
            self.parent[x] = self.find(self.parent[x]); // Path compression
        }
        return self.parent[x];
    }

    // Union by rank
    pub fn unite(self: *UnionFind, x: usize, y: usize) void {
        const rootX = self.find(x);
        const rootY = self.find(y);
        
        if (rootX == rootY) return; // Already in same set
        
        // Union by rank
        if (self.rank[rootX] < self.rank[rootY]) {
            self.parent[rootX] = rootY;
        } else if (self.rank[rootX] > self.rank[rootY]) {
            self.parent[rootY] = rootX;
        } else {
            self.parent[rootY] = rootX;
            self.rank[rootX] += 1;
        }
    }

    // Check if two elements are in the same set
    pub fn connected(self: *UnionFind, x: usize, y: usize) bool {
        return self.find(x) == self.find(y);
    }
};

// Tests for UnionFind data structure
test "UnionFind basic operations" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 5);
    defer uf.deinit();
    
    // Initially, each element should be in its own set
    try std.testing.expectEqual(0, uf.find(0));
    try std.testing.expectEqual(1, uf.find(1));
    try std.testing.expectEqual(2, uf.find(2));
    try std.testing.expectEqual(3, uf.find(3));
    try std.testing.expectEqual(4, uf.find(4));
    
    // No elements should be connected initially
    try std.testing.expect(!uf.connected(0, 1));
    try std.testing.expect(!uf.connected(1, 2));
    try std.testing.expect(!uf.connected(2, 3));
    try std.testing.expect(!uf.connected(3, 4));
}

test "UnionFind union operations" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 5);
    defer uf.deinit();
    
    // Union 0 and 1
    uf.unite(0, 1);
    try std.testing.expect(uf.connected(0, 1));
    try std.testing.expect(!uf.connected(0, 2));
    
    // Union 2 and 3
    uf.unite(2, 3);
    try std.testing.expect(uf.connected(2, 3));
    try std.testing.expect(!uf.connected(1, 2));
    
    // Union the two groups: (0,1) and (2,3)
    uf.unite(1, 3);
    try std.testing.expect(uf.connected(0, 1));
    try std.testing.expect(uf.connected(2, 3));
    try std.testing.expect(uf.connected(0, 2));
    try std.testing.expect(uf.connected(1, 3));
    try std.testing.expect(uf.connected(0, 3));
    
    // Element 4 should still be separate
    try std.testing.expect(!uf.connected(0, 4));
    try std.testing.expect(!uf.connected(1, 4));
    try std.testing.expect(!uf.connected(2, 4));
    try std.testing.expect(!uf.connected(3, 4));
}

test "UnionFind path compression" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 6);
    defer uf.deinit();
    
    // Create a chain: 0 -> 1 -> 2 -> 3 -> 4 -> 5
    uf.unite(0, 1);
    uf.unite(1, 2);
    uf.unite(2, 3);
    uf.unite(3, 4);
    uf.unite(4, 5);
    
    // All elements should be in the same set
    const root = uf.find(0);
    try std.testing.expectEqual(root, uf.find(1));
    try std.testing.expectEqual(root, uf.find(2));
    try std.testing.expectEqual(root, uf.find(3));
    try std.testing.expectEqual(root, uf.find(4));
    try std.testing.expectEqual(root, uf.find(5));
    
    // After path compression, all elements should point directly to root
    // (This is an implementation detail, but shows path compression is working)
    try std.testing.expectEqual(root, uf.parent[0]);
    try std.testing.expectEqual(root, uf.parent[1]);
    try std.testing.expectEqual(root, uf.parent[2]);
    try std.testing.expectEqual(root, uf.parent[3]);
    try std.testing.expectEqual(root, uf.parent[4]);
    try std.testing.expectEqual(root, uf.parent[5]);
}

test "parents are a unique set" {
    const allocator = std.testing.allocator;

    var uf = try UnionFind.init(allocator, 7);
    defer uf.deinit();

    // Create a chain: 0 -> 1 -> 2
    uf.unite(0, 1);
    uf.unite(1, 2);

    // Create a chain: 3 -> 4 -> 5
    uf.unite(3, 4);
    uf.unite(4, 5);

    var map: u64 = 0;

    for (uf.parent) |p| {
        map |= (@as(u64, 1) << @as(u6, @intCast(p)));
    }

    try std.testing.expectEqual(@as(u32, 3), @popCount(map));
}

test "UnionFind multiple disjoint sets" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 9);
    defer uf.deinit();
    
    // Create three separate groups: {0,1,2}, {3,4,5}, {6,7,8}
    uf.unite(0, 1);
    uf.unite(1, 2);
    
    uf.unite(3, 4);
    uf.unite(4, 5);
    
    uf.unite(6, 7);
    uf.unite(7, 8);
    
    // Within each group, all elements should be connected
    try std.testing.expect(uf.connected(0, 1));
    try std.testing.expect(uf.connected(0, 2));
    try std.testing.expect(uf.connected(1, 2));
    
    try std.testing.expect(uf.connected(3, 4));
    try std.testing.expect(uf.connected(3, 5));
    try std.testing.expect(uf.connected(4, 5));
    
    try std.testing.expect(uf.connected(6, 7));
    try std.testing.expect(uf.connected(6, 8));
    try std.testing.expect(uf.connected(7, 8));
    
    // Between groups, no elements should be connected
    try std.testing.expect(!uf.connected(0, 3));
    try std.testing.expect(!uf.connected(2, 4));
    try std.testing.expect(!uf.connected(1, 6));
    try std.testing.expect(!uf.connected(5, 7));
    try std.testing.expect(!uf.connected(2, 8));
}

test "UnionFind union with same element" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 3);
    defer uf.deinit();
    
    // Union an element with itself should be a no-op
    uf.unite(0, 0);
    uf.unite(1, 1);
    
    // Should still be separate sets
    try std.testing.expect(!uf.connected(0, 1));
    try std.testing.expect(!uf.connected(1, 2));
    
    // Elements should be connected to themselves
    try std.testing.expect(uf.connected(0, 0));
    try std.testing.expect(uf.connected(1, 1));
    try std.testing.expect(uf.connected(2, 2));
}

test "UnionFind repeated unions" {
    const allocator = std.testing.allocator;
    
    var uf = try UnionFind.init(allocator, 4);
    defer uf.deinit();
    
    // Union the same pair multiple times
    uf.unite(0, 1);
    uf.unite(0, 1);
    uf.unite(1, 0);
    
    // Should still work correctly
    try std.testing.expect(uf.connected(0, 1));
    try std.testing.expect(!uf.connected(0, 2));
    try std.testing.expect(!uf.connected(1, 2));
    
    // Union with a third element
    uf.unite(1, 2);
    uf.unite(1, 2); // Repeat
    
    try std.testing.expect(uf.connected(0, 1));
    try std.testing.expect(uf.connected(0, 2));
    try std.testing.expect(uf.connected(1, 2));
    try std.testing.expect(!uf.connected(0, 3));
}
