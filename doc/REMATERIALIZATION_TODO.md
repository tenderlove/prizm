# Rematerialization Optimization TODO

## Problem Description

In the current register allocator test case, LR2 and LR3 (from immediate loads `x = 123` and `y = 456`) create excessive register pressure:

- **LR2** (`x₀`): Interferes with LR0, LR1, LR3, LR4, LR5, LR6
- **LR3** (`y₀`): Interferes with LR0, LR1, LR2, LR4, LR6

These values have long live ranges spanning the entire if-statement, forcing the register allocator to either:
1. Keep them in registers (reducing available registers for other computations)
2. Spill them to memory (expensive load/store operations)

## The Rematerialization Solution

**Rematerialization** is an optimization where instead of keeping a value live in a register across a long live range, you regenerate the value at each use site. This is particularly effective for:

- Immediate constants (`loadi 123`, `loadi 456`)
- Simple computations that are cheaper to recompute than to spill/reload
- Values with very long live ranges that create significant register pressure

### Example Transformation

**Before:**
```ruby
x = 123      # LR2 starts here - long live range
y = 456      # LR3 starts here - long live range
if foo
  y += 3     # LR2, LR3 still live (register pressure)
else
  y += 2     # LR2, LR3 still live (register pressure)
end
puts x + y   # LR2, LR3 finally used
```

**After Rematerialization:**
```ruby
if foo
  y = 456    # Rematerialized closer to use
  y += 3
else
  y = 456    # Rematerialized closer to use
  y += 2
end
x = 123      # Rematerialized at use site
puts x + y
```

## Implementation Plan

### 1. Analysis Phase - Identify Rematerialization Candidates

Create `RematerializationAnalysis` pass that identifies:
- Instructions that produce "cheap to recompute" values
  - `loadi` (immediate loads)
  - Simple arithmetic (`add`, `sub` with constants)
  - `getself`, `getparam` (register-to-register moves)
- Live ranges that span many basic blocks
- Values that create high register pressure (interfere with many other live ranges)

### 2. Cost Model

Implement cost functions:
```zig
fn isRematerializationCandidate(instruction: *Instruction) bool {
    return switch (instruction.*) {
        .loadi => true,
        .getself => true, 
        .getparam => true,
        .mov => |mov| mov.in.isConstant(), // moves of constants
        else => false,
    };
}

fn calculateSpillCost(live_range: *Variable) f32 {
    // Estimate cost based on:
    // - Number of interference edges
    // - Distance between definition and uses
    // - Loop nesting level
}

fn calculateRematerializationCost(instruction: *Instruction, use_sites: []UseRef) f32 {
    // Cost = instruction_cost * number_of_uses
    // vs. spill_cost + reload_cost * number_of_uses
}
```

### 3. Live Range Splitting

Modify the register allocator to:
- Split live ranges that are good rematerialization candidates
- Insert rematerialization instructions at use sites
- Remove original definitions when they're no longer needed
- Update SSA form properly (may need to convert back from SSA locally)

### 4. Integration Points

Hook into the CFG compilation pipeline:
```
Parse → Compile → SSA → Rematerialization → Register Allocation → Code Gen
```

- Add rematerialization pass after SSA renaming but before interference graph construction
- Update the interference graph to reflect shorter live ranges
- Ensure phi functions are handled correctly (may need special cases)

### 5. Implementation Steps

1. **Create `src/rematerialization.zig`**
   - `RematerializationPass` struct
   - Candidate identification algorithms
   - Cost model functions
   - Live range splitting logic

2. **Integrate with CFG compilation pipeline**
   - Add `.rematerialized` state to `CFG.State` enum
   - Hook into `compileUntil()` method
   - Add between `.renamed` and `.register_allocated`

3. **Update register allocator**
   - Handle live ranges that have been split
   - Update interference graph construction
   - Ensure phi function handling works with rematerialized values

4. **Add comprehensive tests**
   - Test immediate loads (`loadi`)
   - Test simple arithmetic chains
   - Test complex control flow with multiple use sites
   - Test edge cases (phi functions, loops, nested ifs)

5. **Performance measurement**
   - Count interference edges before/after
   - Measure register spill/reload operations
   - Track code size changes (more instructions vs. fewer spills)

## Expected Benefits

- **Reduced register pressure**: Fewer long-lived values competing for registers
- **Fewer spill operations**: Less memory traffic during register allocation
- **Better register allocation**: More registers available for complex computations
- **Improved code quality**: Particularly beneficial for code with many immediate values

## References

- "Engineering a Compiler" by Cooper & Torczon (Section on rematerialization)
- LLVM's rematerialization pass implementation
- GCC's live range splitting and rematerialization optimizations

## Future Extensions

- **Partial rematerialization**: Only rematerialize on some paths
- **Loop-aware rematerialization**: Consider loop nesting costs
- **Profile-guided decisions**: Use runtime profiling to guide cost model
- **Advanced candidates**: Extend to more complex computations (addresses, etc.)