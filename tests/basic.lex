Received file path: tests/basic.err
Starting to lex the file: tests/basic.err
<Comment(" Basic variable declarations and functions")>
<Newline>
<Identifier("x")>
<Equal>
<Number(7.0)>
<Newline>
<Identifier("y")>
<Equal>
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
<Function>
<Identifier("multiply")>
<LParen>
<Identifier("a")>
<Comma>
<Identifier("b")>
<RParen>
<Equal>
<Return>
<Identifier("a")>
<Multiply>
<Identifier("b")>
<Newline>
<Newline>
<Comment(" Lambda function")>
<Newline>
<Identifier("double")>
<Equal>
<Identifier("x")>
<Arrow>
<Return>
<Identifier("x")>
<Multiply>
<Number(2.0)>
<Newline>
<Newline>
<Comment(" Testing function calls")>
<Newline>
<Identifier("result")>
<Equal>
<Identifier("add")>
<LParen>
<Number(5.0)>
<Comma>
<Number(3.0)>
<RParen>
<Newline>
<Identifier("doubled")>
<Equal>
<Identifier("double")>
<LParen>
<Identifier("result")>
<RParen>
<Newline>
<Newline>
<Comment(" Testing string concatenation")>
<Newline>
<Identifier("greeting")>
<Equal>
<StringLiteral("Hello")>
<Identifier("name")>
<Equal>
<StringLiteral("World")>
<Identifier("message")>
<Equal>
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
Lexer processed tokens successfully.
