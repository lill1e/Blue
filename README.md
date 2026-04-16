# Blue
## Grammar
```
Identifier ::= [a-zA-Z-\_][a-zA-Z0-9-\_]\+
String     ::= " .+ "
FieldRHS   ::= Identifier | String | (FieldRHS "|" FieldRHS)
Field      ::= Identifier "::=" FieldRHS <EOL>
Grammar    ::= Field+
```
