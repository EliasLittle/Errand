
> I kind of consider algebraic data types (the combination of this style of enum (sum types) and structs (product types)) paired with pattern matching as the core feature set that any modern language must support. It makes for really simple, easy to follow code that’s much less error prone than the alternatives. — pdpi (Hacker News)

Set-theoretic type class system like Julia.

Type products ~ Struct
Type sum ~ Enum

Type intersection ~ a <: b or b <: a or a ∩ b
? Type Union ~ a >: b 


Rust traits are essentially just implicit type unions. They take a function and the universe of types and return which types satisfy that function. A function indexed on a trait instead of a type just implies any input who’s type is in the list returned by the trait 