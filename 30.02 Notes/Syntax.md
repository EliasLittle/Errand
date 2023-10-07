**Default to Julia syntax** with some exceptions/expansions listed below

##### Declaration
`let x = 7` ? Do I want `let` or just `x = 7`?

##### Match Statement
```
match __ with
	1         -> "One"
	a::String -> "String"
	_         -> Error()
```

##### Function Declaration
```
fn hello_world()
	return "Hello World!"
end

fn foo(bar) = return bar+" & baz"

x -> return x*2
```

##### Comments
```
-- single line comment

--- small block comment here ---

---------------------------
  Multiline comment here
---------------------------
```

##### Variants
```
Struct color
| Red
| Green
| Blue
End

Struct card_value 
| Ace
| King
| Queen
| Jack
| Number of Int 
End
```

##### Options (Built in variant)
```
Some(x) or None
```

##### Lists
```
L = [1,2,3]
L2 = 0 ||- L # [0,1,2,3]
```

##### Pipes:
* Basic - `1 |> x -> x+1`
Given `fn c(a,b) = ...`
* Inline vars
	* `expr_a |a> expr_b |b> fn_c(a,b)
	* Maybe use `$0:$9` or just `'a':'z'` to limit vars?
* or ordered with output after `>`
	* `expr_a | expr_b |> fn_c
* Positional
	* `expr_a |> fn_c(1, _)`
* Singular var
	* `expr_a |> fn_d(@, 3)`
 
For currying with multiple dispatch:
give blanks with types, this way function is specific, but with partial application.
side affect is positional specificity. 
`fn(_::int32, 4, a, _::String, "hello")`
