# Worklist-based Type Inference for Errand

## Overview

This document describes the implementation of a worklist-based type inference system for Errand's PreIR (Pre-Intermediate Representation), adapted from the DK (Dunfield-Krishnaswami) algorithm used in System F-omega.

## Architecture

### Core Components

1. **ErrandType** - Simplified type system for Errand
2. **Worklist** - Manages type constraints and variable bindings
3. **ErrandInference** - Main inference engine
4. **Judgment** - Type inference judgments (Sub, Inf, Chk)

### Type System

```rust
pub enum ErrandType {
    Int,           // Integer type
    Float,         // Floating point type  
    Bool,          // Boolean type
    String,        // String type
    Unit,          // Unit/void type
    Arrow(Box<ErrandType>, Box<ErrandType>), // Function type T1 -> T2
    Var(String),   // Type variable α
    ETVar(String), // Existential type variable ^α
    Struct(String), // Struct type (simplified)
}
```

### Supported Instructions

The inference system handles all Errand `Instr` variants:

- **Literals**: `Int`, `Float`, `Bool`, `String`, `Symbol`, `Unit`
- **Variables**: `VarRef`, `VarDecl`
- **Operations**: `BinOp`, `UnOp`
- **Control Flow**: `IfStatement`, `WhileLoop` (ForLoop unsupported)
- **Functions**: `FnCall`, `FuncDecl`
- **Structures**: `StructDecl`
- **Regions**: `Region`, `Return`

### Key Features

#### Imperative Construct Handling

- **Variable Declarations**: Return `Unit` type
- **While Loops**: 
  - Return `Unit` type
  - Condition must be `Bool` (not yet enforced)
- **If Statements**: Both branches must have compatible types
- **Regions**: Type determined by return location

#### Binary Operations

Comprehensive support for all Errand binary operators:

- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `^` → `Int -> Int -> Int`
- **Comparison**: `<`, `<=`, `>`, `>=`, `==`, `!=` → `Int -> Int -> Bool`  
- **Logical**: `&&`, `||` → `Bool -> Bool -> Bool`
- **Bitwise**: `&`, `|` → `Int -> Int -> Int`
- **Special**: `.` (field access), `=` (assignment)

#### Unary Operations

- **Negation**: `-` → `Int -> Int`
- **Logical Not**: `!` → `Bool -> Bool`

## Usage

### Basic Type Inference

```rust
use errand::backend::worklist::{ErrandInference, ErrandType};
use errand::backend::preir::{Instr, LiteralPl};

let mut inference = ErrandInference::new();
let instr = Instr::Literal(LiteralPl::Int(42));

match inference.infer_instr(&instr) {
    Ok(ty) => println!("Inferred type: {:?}", ty),
    Err(e) => println!("Type error: {:?}", e),
}
```

### Type Checking

```rust
let mut inference = ErrandInference::new();
let instr = Instr::Literal(LiteralPl::String("hello".to_string()));
let expected = ErrandType::String;

match inference.check_instr(&instr, &expected) {
    Ok(_) => println!("Type check passed"),
    Err(e) => println!("Type error: {:?}", e),
}
```

### With Context

```rust
use std::collections::HashMap;

let mut var_context = HashMap::new();
var_context.insert("x".to_string(), ErrandType::Int);

let mut func_context = HashMap::new();  
func_context.insert("add".to_string(), ErrandType::Arrow(
    Box::new(ErrandType::Int),
    Box::new(ErrandType::Arrow(
        Box::new(ErrandType::Int),
        Box::new(ErrandType::Int)
    ))
));

let mut inference = ErrandInference::with_context(var_context, func_context);
```

## Differences from System F-omega

### Simplified Features

1. **No Higher-Kinded Types**: Only simple types and function types
2. **No Polymorphism**: No universal quantification (∀α. T)
3. **No Pattern Matching**: Replaced with imperative conditionals
4. **No Algebraic Data Types**: Simple struct types only

### Imperative Extensions

1. **Mutable Variables**: Variable declarations and assignments
2. **Loops**: While loops with Unit return type
3. **Instruction Sequences**: Region-based control flow
4. **Side Effects**: Function calls and variable mutations

### Index-based References

Unlike tree-structured CoreTerms, Errand's PreIR uses flat instruction sequences with index-based references (`instr_index`). The current implementation simplifies this by not fully tracking cross-instruction constraints.

## Limitations and Future Work

### Current Limitations

1. **Index Constraints**: Operand type constraints not fully implemented
2. **Region Typing**: Simplified region type inference
3. **Function Types**: Basic function signature support
4. **Struct Fields**: No field-level type checking
5. **For Loops**: Explicitly unsupported per requirements

### Future Enhancements

1. **Full Constraint Tracking**: Implement operand type constraints using instruction indices
2. **Advanced Region Typing**: Proper typing for instruction sequences
3. **Struct Field Access**: Type-safe field operations
4. **Generic Functions**: Basic polymorphism support
5. **Effect System**: Track side effects and mutations

## Integration

The worklist module is integrated into Errand's backend:

```rust
// In src/backend/mod.rs
pub mod worklist;

// Usage in other modules
use crate::backend::worklist::{ErrandInference, ErrandType};
```

## Testing

Run the example to see the inference system in action:

```bash
cargo run --example worklist_example
```

## Comparison to Original

| Feature | System F-omega CoreTerm | Errand Instr |
|---------|------------------------|--------------|
| **Paradigm** | Functional | Imperative |
| **Type System** | Higher-kinded types | Simple types |
| **Control Flow** | Pattern matching | Conditionals/loops |  
| **Data Organization** | Tree structure | Flat instruction sequence |
| **Evaluation** | Substitution-based | Stack/register machine |
| **Polymorphism** | Full System F | None (future work) |

This implementation successfully adapts the sophisticated type theory concepts from System F-omega to work with Errand's practical imperative language design.
