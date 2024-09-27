module TestsInterpreter

open Xunit
open AST
open Interpreter
open System.Collections.Generic

[<Fact>]
let ``Test Variable Declaration`` () =
    let program = [
        VariableAssignment("x", Number(5))
    ]
    let state = interpret program
    Assert.True(state.Variables.ContainsKey("x"))
    Assert.Equal(5, state.Variables.["x"])

[<Fact>]
let ``Test Variable Assignment`` () =
    let program = [
        VariableAssignment("x", Number(5))
        VariableAssignment("x", Number(10))
    ]
    let state = interpret program
    Assert.True(state.Variables.ContainsKey("x"))
    Assert.Equal(10, state.Variables.["x"])

[<Fact>]
let ``Test While Statement`` () =
    let program = [
        VariableAssignment("x", Number(5))
        WhileStatement("x", [
            VariableAssignment("x", BinaryOp(Variable("x"), Sub, Number(1)))
        ])
    ]
    let state = interpret program
    Assert.Equal(0, state.Variables.["x"])

[<Fact>]
let ``Test Sleep Statement`` () =
    let program = [
        Sleep(2)
    ]
    let startTime = System.DateTime.Now
    interpret program |> ignore
    let endTime = System.DateTime.Now
    let elapsedTime = endTime - startTime
    Assert.True(elapsedTime.TotalSeconds >= 2.0)

