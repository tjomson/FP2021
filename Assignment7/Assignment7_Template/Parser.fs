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
    
    let VParse = pletter .>>. many1 palphanumeric |>> tupleToList |>> implode |>> V <?> "Variable"
    
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
    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"

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

    let parseSquareFun _ = failwith "not implemented"

    let parseBoardFun _ = failwith "not implemented"

    type boardFun = coord -> square option
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (bp : boardProg) = failwith "not implemented"

