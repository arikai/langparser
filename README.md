# Langparser

Simple implementation of BASIC-esque grammar [below](#Grammar) using Parsec.
Essentially a toy project to try the lib.

# Grammar
```
<Program> :: = <Variable declaration> <Calculation>.
<Variable declaration> :: = Var <Variable list>
<Variable list> :: = <Identifier> | <Identifier>, <Variable list>

<Calculation> :: = <List of operators>
<Operator list> :: = <Operator> | <Operator> <Operator list>
<Operator> :: = <Assignment> | <Complex operator>
<Assignment> :: = <Identifier> = <Expression>
<Complex operator> :: = <Loop statement>
<Loop statement> :: = WHILE <Expression> DO <Operator>

<Expression> :: = <Unary operator.> <Subexpression> | <Subexpression>
<Subexpression> :: = (<Expression>) | <Operand> | <Subexpression> <Binary operator> <Subexpression>
<Unary operator.> :: = "-" | "not"
<Binary operator> :: = "-" | "+" | "*" | "/" | "<" | ">" | "=="
<Operand> :: = <Identifier> | <Const>
<Identifier> :: = <Letter> <Identifier> | <Letter>
<Constant> :: = <Digit> <Constant> | <Number>
<Letter> :: = a..z
<Digit> :: = 0..9
```
