module AST

type Ops = Add | Sub | Mul | Div

type Expr =
    | Number of int
    | Variable of string
    | BinaryOp of Expr * Ops * Expr

type Statement =
    | VariableDeclaration of string * Expr
    | VariableAssignment of string * Expr
    | IfStatement of string * Block
    | Goto of int
    | Output of string
    | Return of string
and Block = Statement list
