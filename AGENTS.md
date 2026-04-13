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
