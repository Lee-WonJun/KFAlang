module AST

type Ops = Add | Sub | Mul | Div

type Expr =
    | Number of int
    | Variable of string
    | BinaryOp of Expr * Ops * Expr

type Statement =
    | VariableAssignment of string * Expr
    | Sleep of int
    | WhileStatement of string * Block
    | Output of string
    | Break
    | Return of string
and Block = Statement list
