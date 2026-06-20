# Agent Guidelines for the Errand Compiler

## Multiple Dispatch

Errand supports **multiple dispatch**: functions are identified by both their name
and the types of all their parameters.  This is reflected throughout the compiler
pipeline:

- **SIR functions** are keyed by `(name, Vec<param_type_name>)` — the
  `functions: HashMap<String, HashMap<Vec<String>, SIRFunctionInfo>>` map in
  `SIRModule`.
- **Parameter type resolution** must always use the type annotations declared on
  the *current* overload's `FuncDecl`, never by searching for any `FuncDecl` with
  a matching name.  A name-only lookup will collapse all overloads onto the same
  key, overwriting foreign declarations with user-defined wrappers and causing
  infinite recursion at runtime.
- **Call-site dispatch** resolves the callee by matching the function name against
  the inferred types of the arguments at the call site.

When adding or refactoring any part of the compiler that deals with function
lookup, parameter typing, or code generation, always verify that two overloads of
the same function (e.g. `free(a: Int)` and `free(a: String)`) remain distinct
entries throughout the pipeline.

## Eliminating `lower.rs`

The long-term goal is to **gradually eliminate `src/frontend/lower.rs`** by moving
each transformation it performs into the appropriate later compiler phase:

- Desugaring that only needs name information → FIR generation (`fir_gen.rs`)
- Transformations that require type information → analysis (`analysis.rs`) or SIR
  generation (`sir_gen.rs`)
- Recognition of syntactic patterns (e.g. enum variant access) → FIR generation

**Do not add new transformations to `lower.rs`.** Remaining frontend desugar is
limited to `printf` string temporaries and implicit `return` on functions; other
desugaring lives in `fir_gen.rs` (or later phases). New features should place
their lowering logic in the backend from the start.

## Naming: clarity over brevity

Prefer **readable names** over cryptic abbreviations. Do not shorten identifiers
when the savings are a few characters and the full word (or a standard compound
like `constructor_function_id`) reads clearly at a glance. Reserve very short
names for well-established locals with tiny scope (e.g. loop indices) or where the
type or API convention is universally understood.
