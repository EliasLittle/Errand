### Most Important
- Functional
	- Pattern matching
	- First class functions, recursion
	- Differentiate between pure and not-pure functions
		- Use a Julia `func!()` style syntax (?)
	- Support Cons operator
- Multiple Dispatch
	- Currying support w/ Multiple Dispatch
- Strongly, statically typed
	- Hindley-Milner Type Inference Algorithm
- Compiled
	- To a binary
	- JIT compiler too for repl like experience
- Easy syntax
	- Pipelines
- Supports [Traits](https://ahsmart.com/pub/holy-traits-design-patterns-and-best-practice-book/)
- Easy vectorization
	- Julia-style broadcasting
	- Look at array-programming languages (?)
- Data Types
	- Variable and Static sized arrays
		- Maybe Array vs Matrix
		- List vs Vector
	- [Tagged Unions](https://en.wikipedia.org/wiki/Tagged_union?wprov=sfti1) (aka Variants or Enums)
	- 

### Nice to have:
- Unicode support
- NuShell style error messages
- Effect Typing (like [Koka](https://koka-lang.github.io/koka/doc/book.html#why-effects))
- interoperable with other languages (like PyCall in Julia)
- Optionally dynamically typed (JIT) compilation for fast iteration
	- Still have option to have strongly-statically typed for safety
- good concurrency/threading support
- Native color printing + [Rich](https://github.com/Textualize/rich) like package in standard library
- compile to wasm
- Truly inline comments (not sure if this is actually a good idea)
	- Resume code after the comment
	- i.e. `X {temp var} += 3 {num times per week} * 7 {days per week}`  where {} indicates a comment block
- Auto-parallelize 
	- Often uses [egraph](https://github.com/MikeInnes/DataFlow.jl) and could be combined with Equivalence Saturation (see below)
	- See [research from Stanford](https://legion.stanford.edu/pdfs/parallelizer2019.pdf)
	- Common to [Dataflow languages](https://en.wikipedia.org/wiki/Dataflow_programming)
	- Read about [Futhark](https://futhark-lang.org/)

### Experimental ideas:
* Use different syntax depending on whether you want to cast or type check an assignment
	* i.e. if we have `x::Int64 = 3 + 0im`  it's unclear whether we want to cast this as an int and assign or throw an error. 
	* Instead could use `x::Int64 #= 3 + 0im`  v.s. `x::Int64 |= 3 + 0im`  for the two different desires
* Utilize equivalence saturation
	* See [Egg](https://egraphs-good.github.io/)
* Add variable constraints via type generation (Not sure if this works)
	* Only works with strong typing and Multiple Dispatch
	* In essence you might want your variable `x` to never exceed 12.
	* You could write this as `let x < 12` and then generate a type such as `x::Int_lt_12` where `Int_lt_12 :< Int`
	* This would mean that by default it would use any integer function
	* Could you then possibly generate functions specifically for `Int_lt_12` that efficiently throw an error if this constraint would break?
* rule based data filtering
  - See [Oso](https://www.osohq.com/post/logic-into-sql)
  - Possible syntax: rule?(a,b,c) is left=right
    - Like Julia's func!(x) syntax
- First class support for graphs (?stretch?)