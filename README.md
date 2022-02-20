## TinyLanguageProject
### Cooper Standard 

This project is a small language written in Haskell. Requires stack and atleast haskell 4.7. Inspired by work for one of my Computer Science courses I extended the sample exercises into a fully fledged programming language. The syntax is inspired by natural language and requires explicit parenthesis to reduce ambiguity.

If you are unfamiliar with stack, you can start the environment by executing `stack ghci` from the root directory of TinyLanguage. To run a program, you can call `compileAndRun` with a string containing a program, or call `compileAndRunFile` with a string containing the path to a file (for example: `compileAndRun "testProgram.txt"`). To exit the execution environment execute `:q`

Here's a description of the syntax and semantics. There are two basic types, IntegerType and BooleanType, and a few derived types like PairType or ClosureType. All of the constructs can be seen in `TinyDefinitions.hs`. The reserved keywords are: `add`, `subtract`, `multiply`, `divide`, `remainder`, `and`, `or`, `not`, `equals`, `let`, `in`, `be`, `pair`, `first`, `second`, `lambda`, `call`, `with`, `if`, `then`, and `else`. All complex grammar must be wrapped in parenthesis. All integer literals must be non-negative, negative numbers are possible through subtraction. The final value after execution will be returned. For example all of the following will produce `IntegerType 3`:
- `3`
- `3 add 0`
- `(6 add (0 subtract 3))`
- `(if true then 3 else 0)`
- `(let x be (lambda n in n) in (call x with 3))`
- `(let x be 3 in x)`

The `add` keyword produces the sum of two IntegerTypes and is infix. `5 add 2`

The `subtract` keyword produces the difference of two IntegerTypes and is infix. `5 subtract 2`

The `divide` keyword produces the result of integer division of two IntegerTypes and is infix. `5 divide 2`

The `remainder` keyword produces the remainder of two IntegerTypes and is infix. `5 remainder 2`

The `equals` keyword produces the a boolean representing the equality of two IntegerTypes or two BoolTypes and is infix. `5 equals 2` or `true equals true`

The `and` keyword produces the logical and of two BoolTypes and is infix. `true and false`

The `or` keyword produces the logical or of two BoolTypes and is infix. `true or false`

The `not` keyword produces the logical not of a BoolType and is prefix. `not false`

The `pair` keyword constructs a PairType of two ValueTypes and is prefix. The elements of the pair do not need to be of the same type. `(pair <expression> <expression>)`

The `first` keyword extracts the first value from a PairType and is prefix. `(first <PairType>)`

The `second` keyword extracts the second value from a PairType and is prefix. `(second <PairType>)`

The `if` keyword allows for conditional logic. `if <condition> then <expression> else <expression>`. The 'condition' must evaluate to a BoolType value but the two 'expressions' do not need to be of the same type.  


The assignment keyword `let` only assigns in local scope, there are no global variables. The syntax is `(let <identifier> be <ValueType> in <body expression>)`. The identifier cannot be an integer, 'true', 'false' or a reserved keyword. The ValueType can be complex but must resolve to a value at final execution, or the program will be terminated. For example both of the following are valid:
- `(let x be (if true then 3 else 0) in (x add 1))`
- `(let x be (true) in (let y be (false and x) in y))`


Functions are defined with the `lambda` keyword. The syntax is `(lambda <identifier> in <body expression>)`. For example: "`(lambda arg in (arg add 1))`" Functions need to be assigned values to be executed, otherwise the final value is a closure containing the expression and identifier label. If you attempt the run the example above the output is "`ClosureType (Closure "arg" (AdditionNode (IdNode "arg") (ValueNode (IntegerType 1))) [])`" but if instead you run:"`(let func be (lambda arg in (arg add 1)) in (call func with 3))`" the output is `IntegerType 4`. Notice the `call` keyword. This, along with `with`, is the syntax for function execution. 

Execute functions like so `(call <function identifier> with <value>)`. All functions need to have one arguement, but that arguement does not have to be used in the function body. Recursion is easy to produce with this syntax, loops are more verbose. Here is an example of code to produce the factorial of 10 using recursion: "`(let f be (lambda n in (if (n equals 0) then 1 else (n multiply (call f with (n subtract 1))))) in (call f with 10))`". Notice how you are able to reference the identifier `f` within it's own function definition, this is because when `f` is evaluated its scope has already been extended to include that identifier. This is one of the benefits of how functions are defined. The environment is extended before execution of the expression after `in`, that way constructs can be self referential if necessary. 

The `in` keyword defines the scope of an identifier. For example: "`(let x be 4 in (x add 2))`" or "`(lambda fn in (true or false))`"

For a more detailed understanding of the parsing and execution enironment feel free to read through `TinyParser.hs` and `TinyEvaluator.hs` or look through the example programs in `ExamplePrograms.txt` or the tests in `test/Spec.hs`.



