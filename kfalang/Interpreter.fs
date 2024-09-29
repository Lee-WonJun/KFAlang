module Interpreter

open AST
open System.Collections.Generic
open System

type Program = Statement list
type SoftwareState = {
    Variables: Dictionary<string, int>
}

type Signal = NormalSign | BreakSign

let interpret (program: Program) =
    let state = {
        Variables = Dictionary()
    }

    let rec exec program = 
        match program with
        | [] -> NormalSign 
        | VariableAssignment(var, expr) :: rest ->
            let value = evalExpr expr
            if state.Variables.ContainsKey(var) then
                state.Variables.[var] <- value
            else
                state.Variables.Add(var, value)
            exec rest

        | WhileStatement(var, block) :: rest ->
            let mutable inside_continue = true
            while inside_continue && state.Variables.[var] <> 0 do
                exec block |> function 
                | Signal.BreakSign -> inside_continue <- false
                | _ -> ()
            exec rest

        | Sleep(seconds) :: rest ->
            System.Threading.Thread.Sleep(seconds * 1000)
            exec rest

        | Output(var) :: rest -> 
            let ascil = state.Variables.[var] |> Convert.ToChar
            printf "%c" ascil
            exec rest

        | Return(var) :: _ -> // 프로그램 완전 종료
             System.Environment.Exit(state.Variables.[var])
             Signal.BreakSign
        | Break :: _ -> // 프로그램 완전 종료
            Signal.BreakSign


    and evalExpr expr =
        match expr with
        | Number(n) -> n
        | Variable(var) -> state.Variables.[var]
        | BinaryOp(x, op, y) ->
            let x' = evalExpr x
            let y' = evalExpr y
            match op with
            | Add -> x' + y'
            | Sub -> x' - y'
            | Mul -> x' * y'
            | Div -> x' / y'

    exec program
    state
    
