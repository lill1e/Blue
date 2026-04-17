# Blue
## Grammar
```
Identifier ::= [a-zA-Z-\_][a-zA-Z0-9-\_]\+
String     ::= " .+ "
FieldRHS   ::= Identifier | String | (Identifier "as" Identifier) | (FieldRHS "|" FieldRHS)
Field      ::= Identifier "::=" FieldRHS <EOL>
TokenField ::= Identifier "=" String <EOL>
Grammar    ::= (Field | TokenField)+
```
