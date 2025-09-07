# Errand Language

## Goal

The goal of this project is to create a new programming language that features a simple yet powerful syntax that can be simultaneously great for high level and low level programming. 

It will accomplish this by utilizing
- A strong type system
- Multiple dispatch (like Julia)
- Combination of functional and imperative programming

## Status

- **Lexer**: A lexer that tokenizes the source code into meaningful tokens. (Completed)
- **Parser**: A parser that constructs an Abstract Syntax Tree (AST) from the tokens.
- **Interpreter**: A working tree-walking interpreter that supports a subset of the language.
- **Compiler**: Absolute most bare-bones Cranelift based compiler works!!!

## How to use
Compile with: `cargo run --target aarch64-apple-darwin -- --file tests/addOne/addOne.err`
Link with: `clang -o tests/addOne/addOne tests/addOne/addOne.err.bin`

Congrats you have a binary!

The binary currently will just return the result as the exit code, which can be verified with: `./tests/addOne/addOne; echo "Exit code: $?"`

## Live Features

### Supported Features
- **While loops**
- **Control flow**
- **Functions** (including recursive functions)
- **Variable Scopes**: Global and local variables resolved correctly.
- **Basic Tree-Walking**: The interpreter can execute a subset of the language.
- **Type Inference**: Basic type inference and struct type definitions.
- **Object File Generation**: Cranelift backend can now produce object files.

### Planned Features
- **Lambda functions**
- **Inline functions**
- **Types**
    - Partially implemented
- **Operators as functions**: `a op b` == `(op)(a, b)` 
- **Error recovery in Parser**
- **Pattern Matching**
- **Linear Types**
- **Generational References**

## Roadmap

#### Language Features
- [x] While loops
- [x] Recursive functions
- [x] Type inference and struct type definitions
- [ ] Lambda functions
- [ ] Inline functions
- [ ] Types
- [ ] Have all operators as functions 
    - `a op b` == `(op)(a, b)` 

#### Parser
- [ ] Add error recovery

#### Interpreter
- [x] Resolve variable scopes
- [x] Environment class for scope management
- [x] Basic tree-walking interpreter
- [x] Recursive function support
- [ ] Test more thoroughly
- [ ] Add more features

#### Compiler
- [x] Cranelift backend (object file generation)
- [ ] Proper type inference and checking
- [ ] Improved multiple dispatch
- [ ] More optimizations and features


## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests to help improve the project.
