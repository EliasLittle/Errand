# FIR Design

FIR is an intermediate representation used in the Errand compiler between the typed AST and later backend passes. It is a flat, index-based IR suitable for type checking (worklist inference) and future lowering.

## Data Layout

```
FIR
├── main: Instr          -- Root region (entry point)
└── instructions: Vec<Instr>   -- Flat instruction pool
```

- **`main`**: A `Region` instruction that defines the program entry point. It references a slice of `instructions` and a return location.
- **`instructions`**: A single flat vector of all instructions. Instructions refer to each other by **index** (`InstrIndex` = `i64`).

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
| **ForLoop** | `ForLoopData` | Reserved IR shape; **not emitted** by `fir_gen` today — `for` is lowered to `WhileLoop` plus `Region` (see below) |
| **Return** | `ReturnData` | Return from function |
| **Region** | `RegionData` | Block of instructions (sequence) |
| **EnumVariantAccess** | `EnumVariantData` | Unit variant reference `E::V` |
| **EnumVariantConstruct** | `EnumVariantConstructData` | Variant construction with payload |
| **Match** | `MatchData` | `match` on enum with tag-resolved arms |
| **Typeof** | `InstrIndex` | Intrinsic: operand index; resolved during analysis / SIR |

### Declaration Instructions

These define names; they do not produce a value in the same way. Formatted without `%N =`.

| Instruction | Data | Description |
|-------------|------|-------------|
| **VarDecl** | `VarDeclData` | Variable declaration/assignment |
| **FuncDecl** | `FuncData` | Function definition |
| **StructDecl** | `StructData` | Struct definition |
| **EnumDecl** | `EnumData` | Enum definition (ordered variants → integer tags) |

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
| `operand` | `InstrIndex` | Index of operand instruction |

### BinOp (`BinOpPl`)

| Field | Type | Description |
|-------|------|-------------|
| `op` | `BinaryOperator` | Add, Sub, Mul, Div, Mod, Pow, Eq, Lt, Gt, And, Or, etc. |
| `left` | `InstrIndex` | Left operand |
| `right` | `InstrIndex` | Right operand |

### FnCall (`FnCallPl`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Function name |
| `arguments` | `Vec<InstrIndex>` | Argument instruction indices |

### IfStatement (`IfStatementData`)

| Field | Type | Description |
|-------|------|-------------|
| `condition` | `InstrIndex` | Condition (must be boolean) |
| `then_branch` | `InstrIndex` | Then branch (Region or value) |
| `else_branch` | `Option<InstrIndex>` | Else branch (optional) |

### WhileLoop (`WhileLoopData`)

| Field | Type | Description |
|-------|------|-------------|
| `condition` | `InstrIndex` | Loop condition |
| `body` | `InstrIndex` | Loop body (Region) |

### ForLoop (`ForLoopData`)

| Field | Type | Description |
|-------|------|-------------|
| `iterator` | `String` | Loop variable name |
| `range` | `InstrIndex` | Range expression (e.g. list) |
| `body` | `InstrIndex` | Loop body (Region) |

The current FIR generator does **not** emit `Instr::ForLoop`. Source `for` loops are desugared in [`fir_gen.rs`](fir_gen.rs) into index variables, `length` / `get`, and a `WhileLoop` wrapped in a `Region`. The `ForLoop` variant remains for possible future use or other pipeline paths; the worklist type checker reports it as unsupported if it appears.

### Return (`ReturnData`)

| Field | Type | Description |
|-------|------|-------------|
| `value` | `Option<InstrIndex>` | Return value (None = unit) |

### Region (`RegionData`)

| Field | Type | Description |
|-------|------|-------------|
| `instr_start` | `InstrIndex` | Start index (inclusive) |
| `instr_end` | `InstrIndex` | End index (exclusive) |
| `return_loc` | `InstrIndex` | Index of instruction whose value is the region's result |

A Region represents a block: instructions `[instr_start..instr_end)` are executed in order; the value of the region is the value of the instruction at `return_loc`.

### VarDecl (`VarDeclData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Variable name |
| `value` | `InstrIndex` | Value instruction |

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
| `type_params` | `Vec<Id>` | Generic type parameters |
| `fields` | `Vec<FieldDefinition>` | Fields (from AST: `id`, `field_type`) |

### EnumDecl (`EnumData`)

| Field | Type | Description |
|-------|------|-------------|
| `name` | `String` | Enum name |
| `type_params` | `Vec<Id>` | Generic type parameters |
| `variants` | `Vec<EnumVariantInfo>` | Variants in declaration order (index = tag) |

### Match (`MatchData`)

| Field | Type | Description |
|-------|------|-------------|
| `scrutinee` | `InstrIndex` | Value being matched |
| `enum_name` | `String` | Enum for layout / tag lookup |
| `arms` | `Vec<MatchArmData>` | Arms with optional tag and bindings |

### Typeof (instruction variant)

Single field: operand `InstrIndex`. Lowered from a `typeof(...)` call in the AST; not a normal `FnCall`.

---

## Compilation Order (`compile_fir` in `fir_gen`)

1. **Pass 1 — declarations**: For each top-level `StructDefinition` / `EnumDefinition`, emit `StructDecl` / `EnumDecl` (metadata only; no function bodies).
2. **Pass 2 — synthetic struct constructors**: For each struct, emit the compiler-generated constructor as a `FuncDecl` (body compiled in this pass).
3. **Pass 3 — user functions**: For each top-level `FunctionDefinition`, emit `FuncDecl` with compiled body.
4. **Pass 4 — main program**: Compile remaining top-level expressions (everything that is not a struct, enum, or function definition) in order; wrap them in `main` as a `Region` whose `instr_start`..`instr_end` spans those instructions. `return_loc` is the last such instruction, or a fresh `Literal(Unit)` if there are none.

Nested expressions (inside bodies) use the same lowering helpers; nested struct/enum definitions inside expressions also emit duplicate decl instructions into the flat pool when they appear in the AST.

---

## Pipeline Position

```
Parse → Lower → Type inference → … → FIR gen (`compile_fir`) → [Optional: worklist type check] → SIR / lowering → Codegen
```

FIR is produced by `compile_fir()` from the AST (typically after typing). It is consumed by the worklist type checker (`--type-check-fir`) and can be dumped for debugging (`--dump-ir`).
