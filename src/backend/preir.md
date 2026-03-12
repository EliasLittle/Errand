# PreIR Design

PreIR is an intermediate representation used in the Errand compiler between the typed AST and later backend passes. It is a flat, index-based IR suitable for type checking (worklist inference) and future lowering.

## Data Layout

```
PreIR
├── main: Instr          -- Root region (entry point)
└── instructions: Vec<Instr>   -- Flat instruction pool
```

- **`main`**: A `Region` instruction that defines the program entry point. It references a slice of `instructions` and a return location.
- **`instructions`**: A single flat vector of all instructions. Instructions refer to each other by **index** (`instr_index` = `i64`).

Index-based references mean: to get the value of instruction `%5`, look up `instructions[5]`.

---

## Instruction Types

### Value-Producing Instructions

These produce a value at their index (referenced as `%N`).

| Instruction | Data | Description |
|-------------|------|-------------|
| **Literal** | `LiteralPl` | Constant value |
| **VarRef** | `VarRefData` | Reference to a variable by name |
| **UnOp** | `UnOpPl` | Unary operation |
| **BinOp** | `BinOpPl` | Binary operation |
| **FnCall** | `FnCallPl` | Function call |
| **IfStatement** | `IfStatementData` | Conditional expression |
| **WhileLoop** | `WhileLoopData` | While loop expression |
| **ForLoop** | `ForLoopData` | For loop expression |
| **Return** | `ReturnData` | Return from function |
| **Region** | `RegionData` | Block of instructions (sequence) |

### Declaration Instructions

These define names; they do not produce a value in the same way. Formatted without `%N =`.

| Instruction | Data | Description |
|-------------|------|-------------|
| **VarDecl** | `VarDeclData` | Variable declaration/assignment |
| **FuncDecl** | `FuncData` | Function definition |
| **StructDecl** | `StructData` | Struct definition |

---

## Instruction Data (Arguments / Options)

### Literal (`LiteralPl`)

| Variant | Payload | Example |
|---------|---------|---------|
| `Int` | `i64` | `42` |
| `Float` | `f64` | `3.14` |
| `Boolean` | `bool` | `true` |
| `String` | `String` | `"hello"` |
| `Symbol` | `String` | `:foo` |
| `Unit` | — | `()` |

### VarRef (`VarRefData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Variable name |

### UnOp (`UnOpPl`)

| Field | Type | Description |
|-------|------|-------------|
| `op` | `UnaryOperator` | Negate, Not, etc. |
| `operand` | `instr_index` | Index of operand instruction |

### BinOp (`BinOpPl`)

| Field | Type | Description |
|-------|------|-------------|
| `op` | `BinaryOperator` | Add, Sub, Mul, Div, Mod, Pow, Eq, Lt, Gt, And, Or, etc. |
| `left` | `instr_index` | Left operand |
| `right` | `instr_index` | Right operand |

### FnCall (`FnCallPl`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Function name |
| `arguments` | `Vec<instr_index>` | Argument instruction indices |

### IfStatement (`IfStatementData`)

| Field | Type | Description |
|-------|------|-------------|
| `condition` | `instr_index` | Condition (must be boolean) |
| `then_branch` | `instr_index` | Then branch (Region or value) |
| `else_branch` | `Option<instr_index>` | Else branch (optional) |

### WhileLoop (`WhileLoopData`)

| Field | Type | Description |
|-------|------|-------------|
| `condition` | `instr_index` | Loop condition |
| `body` | `instr_index` | Loop body (Region) |

### ForLoop (`ForLoopData`)

| Field | Type | Description |
|-------|------|-------------|
| `iterator` | `String` | Loop variable name |
| `range` | `instr_index` | Range expression (e.g. list) |
| `body` | `instr_index` | Loop body (Region) |

### Return (`ReturnData`)

| Field | Type | Description |
|-------|------|-------------|
| `value` | `Option<instr_index>` | Return value (None = unit) |

### Region (`RegionData`)

| Field | Type | Description |
|-------|------|-------------|
| `instr_start` | `instr_index` | Start index (inclusive) |
| `instr_end` | `instr_index` | End index (exclusive) |
| `return_loc` | `instr_index` | Index of instruction whose value is the region's result |

A Region represents a block: instructions `[instr_start..instr_end)` are executed in order; the value of the region is the value of the instruction at `return_loc`.

### VarDecl (`VarDeclData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Variable name |
| `value` | `instr_index` | Value instruction |

### FuncDecl (`FuncData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Function name |
| `parameters` | `Vec<Parameter>` | Parameters (from AST: `id`, `type_expr`) |
| `body_index` | `i64` | Index of body instruction (usually Region) |

### StructDecl (`StructData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Struct name |
| `fields` | `Vec<FieldDefinition>` | Fields (from AST: `id`, `field_type`) |

---

## Compilation Order (from `preir_gen`)

1. **First pass**: Emit `FuncDecl` and `StructDecl` for all top-level definitions. Function bodies are compiled and their indices stored in `body_index`.
2. **Second pass**: Compile remaining top-level expressions (main program) into a Region. The main Region's `instr_start`..`instr_end` spans these instructions; `return_loc` is the last one (or a Unit literal if empty).

---

## Pipeline Position

```
Parse → Lower → Type inference → Typeof eval → PreIR gen → [Optional: worklist type check] → IR lowering → Codegen
```

PreIR is produced by `compile_preir()` from the typed AST. It is consumed by the worklist type checker (`--type-check-preir`) and can be dumped for debugging (`--dump-ir`).
