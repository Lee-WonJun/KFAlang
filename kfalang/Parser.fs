module Parser

open AST
open FParsec
open System

let trim (s:String) = s.Trim()

let statement, statementRef = createParserForwardedToRef<Statement, unit>()

let ws = spaces
let pnumber = pint32 .>> ws

// 문장식 ID 파서 Helper
let identifierSentenceFactory endSentence =
    manyCharsTill anyChar (pstring endSentence) .>> ws

// 독립적 Id 파서
let identifierIndependent =
    many1Satisfy (fun c -> not (Char.IsWhiteSpace c))

// 변수 선언 파서
let varDeclare =
    parse {
        let! _ = pstring "누군가 내 임기 도중 이뤄냈던" .>> ws
        let! var = identifierSentenceFactory "에 대해 점수를 매겨보라고 한다면"  |>> trim 
        let! val1 = pnumber .>> pstring "점 만점에" .>> ws
        let! val2 = pnumber .>> pstring "점 정도는 된다고 대답하고 싶다" .>> ws
        let value = int (float val2 / float val1)
        return VariableAssignment(var, Number(value))
    }


// 연산자 파서
let op =
    choice [
        pstring "뭐.." >>% Ops.Add
        pstring "그.." >>% Ops.Sub
        pstring "저.." >>% Ops.Div
        pstring "네.." >>% Ops.Mul
    ] .>> ws

// 표현식 파서
let exprParser s =
    let term = (identifierIndependent |>> Variable .>> ws) <|> (pnumber |>> Number .>> ws)
    chainl1 term (op |>> (fun o x y -> BinaryOp(x, o, y))) s

// 변수 할당 파서
let varAssign =
    parse {
        let! _ = pstring "새로운 축구대표팀" .>> ws
        let! var = identifierSentenceFactory "으로"  |>> trim 
        let! expr = exprParser .>> ws
        return VariableAssignment(var, expr)
    }

// GOTO 문 파서
let sleep =
    parse {
        let! _ = pstring "제가 통화 안 하고 동의를 받지 않았다는 것에는 절대 동의하지 못하겠습니다" .>> ws
        let! _ = pstring "전 전력강화위원과는" .>> ws
        let! line1 = pnumber .>> pstring "분" .>> ws
        let! line2 = pnumber .>> pstring "초 통화했습니다" .>> ws
        let line = line1 * 60 + line2
        return Sleep(line)
    }

// 출력 파서
let output =
    parse {
        let! _ = pstring "의원님께서 혹시" .>> ws
        let! var = identifierSentenceFactory "이라는 영화 보신 적이.."  |>> trim 
        return Output(var)
    }

// 리턴 파서
let returnValue =
    parse {
        let! _ = pstring "결과적으로는 제 안에 있는" .>> ws
        let! var = identifierSentenceFactory "가 나오기 시작했습니다"  |>> trim 
        return Return(var)
    }


let whileBlock =
    parse {
        let! _ = pstring "골 먹고 전부 다 손 들고. 이게" .>> ws
        let! var = identifierSentenceFactory "이야??"  |>> trim 
        let! block = many (ws >>. statement .>> optional newline)
        let! _ = pstring "전부 다 넘어지면 아! 아! 내가 분명히 얘기했지!" .>> newline
        return WhileStatement(var, block)
    }

do statementRef := choice [
        attempt whileBlock
        attempt varDeclare
        attempt varAssign
        attempt sleep
        attempt output
        attempt returnValue
        ]

// 전체 프로그램 파서
let program =
    ws >>. many (statement .>> optional newline) .>> eof