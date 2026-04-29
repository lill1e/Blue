# Blue
## Grammar
```
Identifier ::= [a-zA-Z-\_][a-zA-Z0-9-\_]\+
String     ::= " .+ "
FieldRHS   ::= Identifier | (Identifier "as" Identifier) | (FieldRHS "|" FieldRHS)
Field      ::= Identifier "::=" FieldRHS <EOL>
TokenField ::= Identifier "=" String <EOL>
Grammar    ::= (Field | TokenField)+
```

## Description
- `TokenField`s are the building blocks of Blue. They bind a named `Identifier` to a `String` representing a regular expression that parses each token.
- `Field`s are the components of the AST created by the generated parser. They are made up of other `Field`s or `TokenField`s, and may use the `|` operator to represent the OR case.
  They may also bind `Field` or `TokenField` `Identifier`s to a name using the `as` keyword, so that it is accessible in the generated AST.
- A `.blue` file is made up of repeated statements of `TokenField`s and `Field`s in any order.
- To use Blue, inside any Racket file, the user must `(require "../blue.rkt")`, and then use the `generate` macro to create the AST and parser. The `generate` macro takes two arguments:
    1. A prefixed name for the language
    2. The path to the corresponding `.blue` file
- Once the `generate` macro has been ran, the user can use the `<prefix>-<field name>-parse` function, which takes a string, to parse the string into the corresponding AST.
    - The generated `struct`s for each `Field` contain all binded values using the `as` keyword, as well as the possible `variant` of the OR case, if the RHS of the `Field` contains a `|`
        - The variant is 0-indexed, meaning the first OR case has a `variant` of `0`, the second has a `variant` of 1, and so on
    - Bound variables may be accessed using the `<prefix>-<field name>-<variable name>` function to access variables bound with `as`. If the variable bound is not present, `'()` will be set on the struct.
