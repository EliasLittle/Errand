<Comment(" Basic variable declarations and functions")>
<Newline>
<Identifier("x")>
<Assignment>
<Number(7.0)>
<Newline>
<Identifier("y")>
<Assignment>
<StringLiteral("Hello")>
<Newline>
<Comment(" Function declarations using different styles")>
<Newline>
<Function>
<Identifier("add")>
<LParen>
<Identifier("a")>
<Comma>
<Identifier("b")>
<RParen>
<Newline>
<Return>
<Identifier("a")>
<Plus>
<Identifier("b")>
<Newline>
<End>
<Newline>
<Newline>
<Comment(" TODO: Support inline functions")>
<Newline>
<Comment(" fn multiply(a, b) = return a * b")>
<Newline>
<Newline>
<Comment(" Lambda function")>
<Newline>
<Comment(" double = x -> return x * 2")>
<Newline>
<Newline>
<Comment(" Testing function calls")>
<Newline>
<Identifier("result")>
<Assignment>
<Identifier("add")>
<LParen>
<Number(5.0)>
<Comma>
<Number(3.0)>
<RParen>
<Newline>
<Identifier("doubled")>
<Assignment>
<Identifier("double")>
<LParen>
<Identifier("result")>
<RParen>
<Newline>
<Newline>
<Comment(" Testing string concatenation")>
<Newline>
<Identifier("greeting")>
<Assignment>
<StringLiteral("Hello")>
<Identifier("name")>
<Assignment>
<StringLiteral("World")>
<Identifier("message")>
<Assignment>
<Identifier("greeting")>
<Plus>
<StringLiteral(" ")>
<Plus>
<Identifier("name")>
<Newline>
<Newline>
<Comment(" Different comment styles")>
<Newline>
<Comment(" Single line comment")>
<Newline>
<Newline>
<BlockComment(" Block Comment Header ", "\n  This demonstrates a proper\n  multiline comment format\n", " Block Comment Footer ")>
<Newline>
<Newline>
<Comment(" Testing print statements")>
<Newline>
<Print>
<LParen>
<Identifier("message")>
<RParen>
<Newline>
<Print>
<LParen>
<Identifier("doubled")>
<RParen>
<Newline>
