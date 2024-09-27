module Tests

open Xunit
open AST
open FParsec
open Parser

let assertParseSuccess parser input (expected:Statement) =
    match run parser input with
    | Success(result, _, _) ->
        Assert.Equal(expected, result)
    | Failure(errorMsg, _, _) ->
        Assert.False(true, $"Parsing failed with error: {errorMsg}")

[<Fact>]
let ``Test Variable Declaration Parser`` () =
    let input = "누군가 내 임기 도중 이뤄냈던 업적에 대해 점수를 매겨보라고 한다면 10점 만점에 90점 정도는 된다고 대답하고 싶다"
    let expected = VariableDeclaration("업적", Number(9))
    assertParseSuccess varDeclare input expected


[<Fact>]
let ``Parsing a variable declaration should return the correct AST`` () =
    let input = "누군가 내 임기 도중 이뤄냈던 업적에 대해 점수를 매겨보라고 한다면 10점 만점에 8점 정도는 된다고 대답하고 싶다"
    let expected = VariableDeclaration("업적", Number(0))
    assertParseSuccess varDeclare input expected

[<Fact>]
let ``Parsing a variable assignment should return the correct AST`` () =
    let input = "새로운 축구대표팀 감독으로 홍명보"
    let expected = VariableAssignment("감독", Variable("홍명보"))
    assertParseSuccess varAssign input expected

[<Fact>]
let ``Parsing a variable assignment with an expression should return the correct AST`` () =
    let input = "새로운 축구대표팀 감독으로 머니볼 뭐.. 업적 그.. 무언가 저.. 팀 네.. 홍명보"
    
    let expectedOps = 
        BinaryOp 
            (BinaryOp
                (BinaryOp
                    (BinaryOp (Variable "머니볼", Add, Variable "업적"), Sub, Variable "무언가"), Div, Variable "팀"), Mul, Variable "홍명보")
    let expected = VariableAssignment("감독", expectedOps)
    assertParseSuccess varAssign input expected

[<Fact>]
let ``parse Goto statement`` () =
    let input = "제가 통화 안 하고 동의를 받지 않았다는 것에는 절대 동의하지 못하겠습니다 전 전력강화위원과는 10분 30초 통화했습니다"
    let expected = Goto(630)
    assertParseSuccess goto input expected

[<Fact>]
let ``parse Output statement`` () =
    let input = "의원님께서 혹시 머니볼이라는 영화 보신 적이.."
    let expected = Output("머니볼")
    assertParseSuccess output input expected

[<Fact>]
let ``parse Return statement`` () =
    let input = "결과적으로는 제 안에 있는 무언가가 나오기 시작했습니다"
    let expected = Return("무언가")
    assertParseSuccess returnValue input expected

[<Fact>]
let ``Parse ifBlock statement`` () =
    let input = """골 먹고 전부 다 손 들고. 이게 팀이야??
누군가 내 임기 도중 이뤄냈던 업적에 대해 점수를 매겨보라고 한다면 10점 만점에 8점 정도는 된다고 대답하고 싶다
새로운 축구대표팀 감독으로 홍명보
전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!
"""
    let expected = IfStatement("팀", [
        VariableDeclaration("업적", Number(0))
        VariableAssignment("감독", Variable("홍명보"))
    ])

    assertParseSuccess ifBlock input expected
