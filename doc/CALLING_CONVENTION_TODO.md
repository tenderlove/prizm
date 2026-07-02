# Calling Convention Support with Register Constraints TODO

## Overview
Implement register constraints to support calling conventions where specific live ranges must be assigned to exact registers (e.g., `getparam(0)` must be in R0, `getparam(1)` must be in R1).

## 1. Register Constraint System

### Define Register Constraints
```zig
pub const RegisterConstraint = union(enum) {
    // Ordered from easiest to hardest to color (for allocation priority)
    general_purpose,                      // Can use any available register (easiest)
    register_class: RegisterClass,        // Must use one from this class (medium)
    specific_register: PhysicalRegister,  // Must use this exact register (hardest)
};

pub const RegisterClass = enum {
    parameter,      // R0-R3 (for flexible parameter allocation)
    caller_saved,   // R0-R3  
    callee_saved,   // R4-R7
};
```

## 2. Live Range Classification with Specific Constraints

### Parameter-Specific Assignment
```zig
const MAX_PARAM_REGISTERS = 4; // R0-R3 for parameters

fn classifyLiveRange(cfg: *CFG, lr: *Var) RegisterConstraint {
    // Find the defining instruction for this live range
    for (instructions) |insn| {
        if (insn.getOut() == lr) {
            switch (insn.*) {
                .getparam => |param| {
                    if (param.index < MAX_PARAM_REGISTERS) {
                        // getparam(0) → R0, getparam(1) → R1, etc.
                        const required_reg: PhysicalRegister = @enumFromInt(param.index);
                        return .{ .specific_register = required_reg };
                    } else {
                        // Parameters beyond register limit are transformed to load_stack_param
                        // This is handled during instruction transformation, not via constraints
                        return .general_purpose;
                    }
                },
                .call => {
                    // Call results must be in R0 (return register)
                    return .{ .specific_register = .R0 };
                },
                else => return .general_purpose,
            }
        }
    }
    return .general_purpose;
}
```

### Add Constraint Field to Live Ranges
- Extend `VariableData.live_range` to include `constraint: RegisterConstraint`
- Set during live range creation based on defining instruction analysis

## 3. Constraint-Aware Register Allocator

### Priority-Based Selection
Process live ranges in constraint priority order:
1. **Specific register constraints** (highest priority)
2. **Register class constraints** 
3. **General purpose** (lowest priority)

### Modified ChaitinAllocator
```zig
fn select(self: *ChaitinAllocator, allocator: std.mem.Allocator, worklist: LiveRangeList) !RegisterMapping {
    // Sort worklist by constraint priority
    sortByConstraintPriority(worklist);
    
    for (worklist.items) |lr| {
        const constraint = lr.getRegisterConstraint();
        
        switch (constraint) {
            .specific_register => |reg| {
                // Deterministic assignment - no choice
                if (isRegisterConflicting(reg, lr)) {
                    return error.RegisterConstraintConflict;
                }
                register_mapping.items[lr.getLocalId()] = getPhysicalRegisterVar(reg);
            },
            .register_class => |class| {
                // Choose from allowed register set
                const allowed_regs = getRegistersForClass(class);
                const reg = pickAvailableFromSet(allowed_regs, lr) orelse 
                    return error.NoAvailableRegistersInClass;
                register_mapping.items[lr.getLocalId()] = getPhysicalRegisterVar(reg);
            },
            .general_purpose => {
                // Use existing Chaitin coloring logic
                const reg = pickAnyAvailableRegister(lr) orelse 
                    return error.OutOfRegisters;
                register_mapping.items[lr.getLocalId()] = getPhysicalRegisterVar(reg);
            }
        }
    }
}
```

## 4. Register Reuse Strategy

### Natural Reuse Through Liveness
Parameter registers automatically become available when their live ranges end:

**Example Function**:
```ruby
def foo(x, y)    # x→R0, y→R1 (specific constraints)
  a = x + 1      # x's live range ends, R0 becomes available  
  b = compute()  # b could reuse R0 (general purpose)
  return a + b   # return→R0 (specific constraint, may conflict with b)
end
```

**Register Allocation Flow**:
1. `getparam(0)` → R0 (mandatory)
2. `getparam(1)` → R1 (mandatory) 
3. `b` → R0 (available after x's range ends)
4. Return expression → R0 (mandatory, may require moving b)

### Conflict Resolution
When specific constraints conflict with existing allocations:
- **Insert move instructions** to resolve conflicts
- **Spill/reload** if no moves can resolve the conflict
- **Error out** if constraint cannot be satisfied (indicates calling convention violation)

## 5. Implementation Details

### Constraint Detection
```zig
fn analyzeInstructionConstraints(cfg: *CFG) void {
    for (cfg.blocks) |bb| {
        var iter = bb.instructionIter(.{});
        while (iter.next()) |insn| {
            if (insn.getOut()) |out_var| {
                const constraint = determineConstraint(insn);
                setLiveRangeConstraint(out_var, constraint);
            }
        }
    }
}
```

### Integration with Existing Pipeline
1. **After live range construction**: Analyze and set constraints
2. **Before register allocation**: Sort by constraint priority
3. **During allocation**: Respect constraints in selection phase
4. **After allocation**: Insert moves for constraint conflicts

## 6. Key Implementation Insight

### Parameter Register Mapping
The critical insight is that `getparam(0)` **must** map to R0, `getparam(1)` **must** map to R1, etc. This is not just a "parameter register class" constraint - it's a **specific register constraint**. The register allocator becomes deterministic for these cases.

### Register Availability
Once a parameter's live range ends, its register becomes available for general allocation. The liveness analysis naturally handles this reuse without any special logic needed.

## 7. Benefits

### Precise Calling Convention Support
- **Deterministic parameter placement**: `getparam(0)` always → R0
- **Flexible general allocation**: Other live ranges can reuse parameter registers
- **Extensible**: Easy to add new constraint types

### Efficient Register Usage
- No artificial register pools or restrictions
- Natural reuse through liveness analysis
- Minimal move instruction insertion

### Error Handling
- Clear constraint violation detection
- Helps debug calling convention issues
- Prevents silent parameter passing bugs

## 8. Stack Parameter Handling

### Problem: More Parameters Than Registers
When a function has more parameters than available parameter registers (> 4 parameters), the excess parameters must be passed on the stack according to the calling convention.

### Solution: Stack Parameter Constraints
- **Parameters 0-3**: Use register constraints (R0-R3)
- **Parameters 4+**: Use stack parameter constraints with calculated offsets

### Stack Parameter Implementation
```zig
// In the register allocator
switch (constraint) {
    .stack_parameter => |param_index| {
        // Create a stack slot reference instead of a register
        const stack_offset = calculateStackOffset(param_index);
        register_mapping.items[lr.getLocalId()] = createStackSlotVar(stack_offset);
    },
    // ... other constraint cases
}
```

### Stack Offset Calculation
```zig
fn calculateStackOffset(param_index: usize) usize {
    // Parameters 4+ are stored on stack
    // Stack layout: [param4][param5][param6]... (growing downward)
    const stack_param_index = param_index - MAX_PARAM_REGISTERS;
    return stack_param_index * @sizeOf(usize); // Word size
}
```

### Stack Access Instructions
Stack parameters require different instruction generation:
- **Register parameters**: Direct register access
- **Stack parameters**: Memory load/store instructions

```zig
// Example IR generation for stack parameters
.getparam => |param| {
    if (param.index < MAX_PARAM_REGISTERS) {
        // Regular register parameter - already in register
        return param.out;
    } else {
        // Stack parameter - generate load instruction
        const stack_offset = calculateStackOffset(param.index);
        return generateStackLoad(stack_offset, param.out);
    }
}
```

### Calling Convention Compliance
This approach ensures compliance with standard calling conventions:
- **Register parameters**: Fast access, no memory operations
- **Stack parameters**: Standard stack layout, compatible with C calling conventions
- **Mixed scenarios**: Functions with both register and stack parameters work correctly

### Example: Function with 6 Parameters
```ruby
def foo(a, b, c, d, e, f)  # More than 4 parameters
  # a → R0, b → R1, c → R2, d → R3 (register parameters)
  # e → stack[0], f → stack[1] (stack parameters)
  return a + b + c + d + e + f
end
```

**Register Allocation**:
- `getparam(0)` → R0 (specific register constraint)
- `getparam(1)` → R1 (specific register constraint)
- `getparam(2)` → R2 (specific register constraint)
- `getparam(3)` → R3 (specific register constraint)
- `getparam(4)` → stack[0] (stack parameter constraint)
- `getparam(5)` → stack[1] (stack parameter constraint)

## 9. Implementation Order

1. **Add RegisterConstraint types and data structures**
   - Define enums and unions in `register_allocator.zig`
   - Add constraint field to `VariableData.live_range`

2. **Implement constraint analysis for instructions**
   - Add `classifyLiveRange()` function
   - Analyze `getparam`, `call`, and other constraint-generating instructions

3. **Extend live range data structure with constraint field**
   - Modify live range creation to set constraints
   - Update live range initialization functions

4. **Modify ChaitinAllocator to handle constraint priorities**
   - Add constraint priority sorting
   - Implement constraint-aware selection logic

5. **Add conflict detection and resolution logic**
   - Detect when specific constraints conflict
   - Implement move insertion for resolution

6. **Integrate constraint analysis into compilation pipeline**
   - Call constraint analysis after live range construction
   - Ensure constraints are set before register allocation

7. **Test with various parameter passing scenarios**
   - Simple parameter functions
   - Complex parameter interactions
   - Return value handling

## 9. Files to Modify

- **`src/register_allocator.zig`**: Core constraint and allocation logic
- **`src/ir.zig`**: Add constraint field to live range data structure
- **`src/scope.zig`**: Update live range creation to set constraints
- **Tests**: Add parameter passing and calling convention tests

## 10. Future Extensions

This constraint system can be extended to support:
- **Multiple calling conventions** (different constraint sets)
- **Architecture-specific constraints** (floating-point registers, vector registers)
- **ABI compliance** (stack parameter handling, return value placement)
- **Optimization opportunities** (constraint-aware instruction selection)

The design provides a solid foundation for comprehensive calling convention support while maintaining the efficiency and correctness of the existing register allocator.