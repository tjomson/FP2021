module ImpParser

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    // Assignement 7.1
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    // Assignment 7.2
    let whitespaceChar = satisfy System.Char.IsWhiteSpace
    let pletter        = satisfy System.Char.IsLetter
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    // Assignment 7.3
    let (.>*>.) a b = a .>> spaces .>>. b
    let (.>*>) a b  = a .>> spaces .>> b
    let (>*>.) a b  = a >>. spaces >>. b

    // Assignement 7.4
    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}'
    let apostrophise p = pchar ''' >>. p .>> pchar '''

    // Assignment 7.5
    let implode cs =
        let folder = fun c s -> (string c) + s
        List.foldBack folder cs ""

    let tupleToList (c: char, l:char list) =
        c :: l

    let punderscore = pchar '_'

    let pid = (pletter <|> punderscore) .>>. (palphanumeric <|> punderscore |> many) |>> tupleToList |>> implode
    
    // Assignment 7.6
    let unop a b =
        a >*>. b  
    
    // Assignment 7.7
    let binop a b c = 
        b .>*> a .>*>. c


    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    // Assignment 7.8
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"

    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    

    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    

    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    // let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"
    
    let NegParse = pchar '-'  >>. pint32 |>> (fun x -> Mul (N -1, N x))

    let NParse   = pint32 |>> N <?> "Int"
    
    // let VParse = pletter .>>. many1 palphanumeric |>> tupleToList |>> implode |>> V <?> "Variable"

    let VParse = pid |>> V <?> "Variable"
    
    let ParParse = parenthesise TermParse

    let PVParse = pPointValue >*>. ParParse |>> PV <?> "PointValue"
    
    let CharacterParse, cref = createParserForwardedToRef<cExp>()

    let CTIParse = pCharToInt >*>. parenthesise CharacterParse |>> CharToInt <?> "CharToInt"


    do aref := choice [NegParse; PVParse; NParse; CTIParse; ParParse; VParse]

    let AexpParse = TermParse 

    // Assignment 7.9
    let CexpParse = CharacterParse

    let CParse = pchar ''' >>. anyChar .>> pchar ''' |>> C

    let CVParse = pCharValue >*>. parenthesise TermParse |>> CV <?> "CharacterValue"

    let TUParse = pToUpper  >*>. parenthesise CharacterParse |>> ToUpper <?> "ToUpper"

    let TLParse = pToLower  >*>. parenthesise CharacterParse |>> ToLower <?> "ToLower"

    let ITCParse = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    
    do cref := choice [TUParse; TLParse; ITCParse; CVParse; CParse]

    // Assignment 7.10
    
    let AndOrParse, aoref = createParserForwardedToRef<bExp>()
    let EqualityParse, eref = createParserForwardedToRef<bExp>()
    let BooleanParse, bref = createParserForwardedToRef<bExp>()

    let BexpParse = AndOrParse

    let TParse = pTrue |>> (fun _ -> TT) <?> "True"
    let FParse = pFalse |>> (fun _ -> FF) <?> "False"
    let ConjParse = EqualityParse .>*> pstring "/\\" .>*>. AndOrParse |>> Conj <?> "Conjunction"
    let DisParse = EqualityParse .>*> pstring "\\/" .>*>. AndOrParse |>> (fun (x,y) -> Not (Conj(Not x, Not y))) <?> "Disjunction"
    let NotParse = pchar '~' >*>. BooleanParse |>> Not <?> "Not"
    // let EqualParse = ProdParse .>*> pchar '=' .>*>. TermParse |>> AEq  <?> "Equal"
    let EqualParse = binop (pchar '=') ProdParse TermParse |>> (fun (x,y) -> x .=. y) <?> "Equal"
    // let NotEqualParse = ProdParse .>*> pstring "<>" .>*>. TermParse |>> (fun (x,y) -> Not (AEq (x,y))) <?> "Not"
    let NotEqualParse = binop (pstring "<>") ProdParse TermParse |>> (fun (x,y) -> x .<>. y) <?> "NotEqual"
    // let LessThanParse = ProdParse .>*> pchar '<' .>*>. TermParse |>> ALt  <?> "LessThan"
    let LessThanParse = binop (pchar '<') ProdParse TermParse |>> (fun (x,y) -> x .<. y) <?> "LessThan"
    // let LessEqualParse = ProdParse .>*> pstring "<=" .>*>. TermParse |>> (fun (x, y) -> Not (Conj(Not (ALt(x, y)), Not (AEq(x, y))))) <?> "LessEqual"
    let LessEqualParse = binop (pstring "<=") ProdParse TermParse |>> (fun (x, y) -> x .<=. y) <?> "LessEqual"
    // let GreaterThanParse = ProdParse .>*> pchar '>' .>*>. TermParse |>> (fun (x, y) -> Conj (Not (AEq (x,y)), Not (ALt (x, y))))  <?> "GreaterThan"
    let GreaterThanParse = binop (pchar '>') ProdParse TermParse |>> (fun (x, y) -> x .>. y) <?> "GreaterThan"
    // let GreaterEqualParse = ProdParse .>*> pstring ">=" .>*>. TermParse |>> (fun (x, y) -> Not (ALt (x, y)))  <?> "GreaterEqual"
    let GreaterEqualParse = binop (pstring ">=") ProdParse TermParse |>> (fun (x,y) -> x .>=. y) <?> "GreaterEqual"
    let IsDigitParse = pIsDigit >*>. CharacterParse |>> IsDigit <?> "IsDigit"
    let IsLetterParse = pIsLetter >*>. CharacterParse |>> IsLetter <?> "IsLetter"
    let BParParse = parenthesise AndOrParse 

    do aoref := choice [DisParse; ConjParse; EqualityParse;]

    do eref := choice [EqualParse; NotEqualParse; LessThanParse; LessEqualParse; GreaterThanParse; GreaterEqualParse; BooleanParse;]

    do bref := choice [NotParse; IsDigitParse; IsLetterParse; BParParse; TParse; FParse]

    // Assignment 7.11
    let stmntParse, sref = createParserForwardedToRef<stm>()
    let stateParse, stateref = createParserForwardedToRef<stm>()

    let DeclareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"

    let AssParse = pid .>*> (pstring ":=") .>*>. TermParse |>> (fun (x, y) -> Ass (x, y)) <?> "Assignment"

    let SkipParse = pstring "Skip" |>> (fun _ -> Skip) <?> "Skip"

    let ITEParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlyBrackets stmntParse .>*> pelse .>*>. curlyBrackets stmntParse |>> (fun ((a, b), c) -> ITE (a, b, c)) <?> "IfThenElse"

    let ITParse = pif >*>. parenthesise BexpParse .>*> pthen .>*>. curlyBrackets stmntParse |>> (fun (a, b) -> ITE (a, b, Skip)) <?> "IfThen"

    let WhileParse = pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. curlyBrackets stmntParse |>> (fun (a, b) -> While (a, b)) <?> "While"

    let SeqParse = stateParse .>*> pchar ';' .>*>. stmntParse |>> (fun (a, b) -> Seq (a, b)) <?> "Sequence"

    do sref := choice [SeqParse; stateParse]

    do stateref := choice [DeclareParse; SkipParse; AssParse; ITEParse; ITParse; WhileParse;]
    

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    // Assignment 7.12
    let parseSquareFun (sqp: squareProg): square = 
        Map.map (fun _ y -> (run stmntParse y) |> getSuccess |> stmntToSquareFun) sqp

    // Assignment 7.13
    let parseBoardFun (s: string) (m: Map<int, 'a>) = 
        stmntToBoardFun (run stmntParse s |> getSuccess) m

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    // Assignment 7.14
    let parseBoardProg (bp : boardProg): board = 
        let m' = Map.map (fun _ y -> parseSquareFun y) bp.squares
        {
            center = bp.center;
            defaultSquare = m'.[bp.usedSquare]
            squares = parseBoardFun bp.prog m'
        }
