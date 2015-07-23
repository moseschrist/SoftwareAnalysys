// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open BJKCore

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | WIDENING
  | STRING of (string)
  | CMD
  | EOF
  | PROG
  | LBRACE
  | RBRACE
  | SKIP
  | ASSIGN
  | OR
  | ASSUME
  | CHOOSE
  | ELSE
  | IF
  | DO
  | WHILE
  | TRUE
  | FALSE
  | EQUAL
  | NOTEQUAL
  | GREAT
  | GREATEQUAL
  | LESS
  | LESSEQUAL
  | FIX of (int)
  | VAR of (int)
  | PLUS
  | MINUS
  | TIMES
  | RPAREN
  | LPAREN
  | SEMI
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_WIDENING
    | TOKEN_STRING
    | TOKEN_CMD
    | TOKEN_EOF
    | TOKEN_PROG
    | TOKEN_LBRACE
    | TOKEN_RBRACE
    | TOKEN_SKIP
    | TOKEN_ASSIGN
    | TOKEN_OR
    | TOKEN_ASSUME
    | TOKEN_CHOOSE
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_DO
    | TOKEN_WHILE
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_EQUAL
    | TOKEN_NOTEQUAL
    | TOKEN_GREAT
    | TOKEN_GREATEQUAL
    | TOKEN_LESS
    | TOKEN_LESSEQUAL
    | TOKEN_FIX
    | TOKEN_VAR
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_SEMI
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startPROG
    | NONTERM_PROG
    | NONTERM_COMMAND
    | NONTERM_EXP
    | NONTERM_BOOLEXP

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | WIDENING  -> 0 
  | STRING _ -> 1 
  | CMD  -> 2 
  | EOF  -> 3 
  | PROG  -> 4 
  | LBRACE  -> 5 
  | RBRACE  -> 6 
  | SKIP  -> 7 
  | ASSIGN  -> 8 
  | OR  -> 9 
  | ASSUME  -> 10 
  | CHOOSE  -> 11 
  | ELSE  -> 12 
  | IF  -> 13 
  | DO  -> 14 
  | WHILE  -> 15 
  | TRUE  -> 16 
  | FALSE  -> 17 
  | EQUAL  -> 18 
  | NOTEQUAL  -> 19 
  | GREAT  -> 20 
  | GREATEQUAL  -> 21 
  | LESS  -> 22 
  | LESSEQUAL  -> 23 
  | FIX _ -> 24 
  | VAR _ -> 25 
  | PLUS  -> 26 
  | MINUS  -> 27 
  | TIMES  -> 28 
  | RPAREN  -> 29 
  | LPAREN  -> 30 
  | SEMI  -> 31 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_WIDENING 
  | 1 -> TOKEN_STRING 
  | 2 -> TOKEN_CMD 
  | 3 -> TOKEN_EOF 
  | 4 -> TOKEN_PROG 
  | 5 -> TOKEN_LBRACE 
  | 6 -> TOKEN_RBRACE 
  | 7 -> TOKEN_SKIP 
  | 8 -> TOKEN_ASSIGN 
  | 9 -> TOKEN_OR 
  | 10 -> TOKEN_ASSUME 
  | 11 -> TOKEN_CHOOSE 
  | 12 -> TOKEN_ELSE 
  | 13 -> TOKEN_IF 
  | 14 -> TOKEN_DO 
  | 15 -> TOKEN_WHILE 
  | 16 -> TOKEN_TRUE 
  | 17 -> TOKEN_FALSE 
  | 18 -> TOKEN_EQUAL 
  | 19 -> TOKEN_NOTEQUAL 
  | 20 -> TOKEN_GREAT 
  | 21 -> TOKEN_GREATEQUAL 
  | 22 -> TOKEN_LESS 
  | 23 -> TOKEN_LESSEQUAL 
  | 24 -> TOKEN_FIX 
  | 25 -> TOKEN_VAR 
  | 26 -> TOKEN_PLUS 
  | 27 -> TOKEN_MINUS 
  | 28 -> TOKEN_TIMES 
  | 29 -> TOKEN_RPAREN 
  | 30 -> TOKEN_LPAREN 
  | 31 -> TOKEN_SEMI 
  | 34 -> TOKEN_end_of_input
  | 32 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startPROG 
    | 1 -> NONTERM_PROG 
    | 2 -> NONTERM_PROG 
    | 3 -> NONTERM_COMMAND 
    | 4 -> NONTERM_COMMAND 
    | 5 -> NONTERM_COMMAND 
    | 6 -> NONTERM_COMMAND 
    | 7 -> NONTERM_COMMAND 
    | 8 -> NONTERM_COMMAND 
    | 9 -> NONTERM_COMMAND 
    | 10 -> NONTERM_COMMAND 
    | 11 -> NONTERM_EXP 
    | 12 -> NONTERM_EXP 
    | 13 -> NONTERM_EXP 
    | 14 -> NONTERM_EXP 
    | 15 -> NONTERM_EXP 
    | 16 -> NONTERM_EXP 
    | 17 -> NONTERM_BOOLEXP 
    | 18 -> NONTERM_BOOLEXP 
    | 19 -> NONTERM_BOOLEXP 
    | 20 -> NONTERM_BOOLEXP 
    | 21 -> NONTERM_BOOLEXP 
    | 22 -> NONTERM_BOOLEXP 
    | 23 -> NONTERM_BOOLEXP 
    | 24 -> NONTERM_BOOLEXP 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 34 
let _fsyacc_tagOfErrorTerminal = 32

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | WIDENING  -> "WIDENING" 
  | STRING _ -> "STRING" 
  | CMD  -> "CMD" 
  | EOF  -> "EOF" 
  | PROG  -> "PROG" 
  | LBRACE  -> "LBRACE" 
  | RBRACE  -> "RBRACE" 
  | SKIP  -> "SKIP" 
  | ASSIGN  -> "ASSIGN" 
  | OR  -> "OR" 
  | ASSUME  -> "ASSUME" 
  | CHOOSE  -> "CHOOSE" 
  | ELSE  -> "ELSE" 
  | IF  -> "IF" 
  | DO  -> "DO" 
  | WHILE  -> "WHILE" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | EQUAL  -> "EQUAL" 
  | NOTEQUAL  -> "NOTEQUAL" 
  | GREAT  -> "GREAT" 
  | GREATEQUAL  -> "GREATEQUAL" 
  | LESS  -> "LESS" 
  | LESSEQUAL  -> "LESSEQUAL" 
  | FIX _ -> "FIX" 
  | VAR _ -> "VAR" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | RPAREN  -> "RPAREN" 
  | LPAREN  -> "LPAREN" 
  | SEMI  -> "SEMI" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | WIDENING  -> (null : System.Object) 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | CMD  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | PROG  -> (null : System.Object) 
  | LBRACE  -> (null : System.Object) 
  | RBRACE  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | ASSUME  -> (null : System.Object) 
  | CHOOSE  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | WHILE  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | EQUAL  -> (null : System.Object) 
  | NOTEQUAL  -> (null : System.Object) 
  | GREAT  -> (null : System.Object) 
  | GREATEQUAL  -> (null : System.Object) 
  | LESS  -> (null : System.Object) 
  | LESSEQUAL  -> (null : System.Object) 
  | FIX _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 8us; 65535us; 0us; 2us; 15us; 8us; 21us; 9us; 24us; 10us; 27us; 11us; 30us; 12us; 33us; 13us; 40us; 14us; 13us; 65535us; 6us; 7us; 17us; 47us; 36us; 47us; 54us; 43us; 55us; 44us; 56us; 45us; 57us; 46us; 59us; 48us; 60us; 49us; 61us; 50us; 62us; 51us; 63us; 52us; 64us; 53us; 2us; 65535us; 17us; 18us; 36us; 37us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 12us; 26us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 3us; 1us; 2us; 5us; 1us; 1us; 1us; 3us; 1us; 4us; 1us; 4us; 4us; 4us; 13us; 14us; 15us; 2us; 5us; 5us; 2us; 5us; 6us; 2us; 5us; 7us; 2us; 5us; 7us; 2us; 5us; 8us; 2us; 5us; 8us; 2us; 5us; 10us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 12us; 4us; 13us; 13us; 14us; 15us; 4us; 13us; 14us; 14us; 15us; 4us; 13us; 14us; 15us; 15us; 4us; 13us; 14us; 15us; 16us; 9us; 13us; 14us; 15us; 17us; 18us; 19us; 20us; 21us; 22us; 4us; 13us; 14us; 15us; 17us; 4us; 13us; 14us; 15us; 18us; 4us; 13us; 14us; 15us; 19us; 4us; 13us; 14us; 15us; 20us; 4us; 13us; 14us; 15us; 21us; 4us; 13us; 14us; 15us; 22us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 16us; 1us; 17us; 1us; 18us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 23us; 1us; 24us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 8us; 10us; 12us; 14us; 16us; 21us; 24us; 27us; 30us; 33us; 36us; 39us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 78us; 80us; 82us; 84us; 86us; 88us; 90us; 92us; 94us; 96us; 98us; 103us; 108us; 113us; 118us; 128us; 133us; 138us; 143us; 148us; 153us; 158us; 160us; 162us; 164us; 166us; 168us; 170us; 172us; 174us; 176us; 178us; 180us; 182us; |]
let _fsyacc_action_rows = 67
let _fsyacc_actionTableElements = [|7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 0us; 49152us; 2us; 16386us; 3us; 3us; 31us; 15us; 0us; 16385us; 0us; 16387us; 1us; 32768us; 8us; 6us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 16388us; 26us; 54us; 27us; 55us; 28us; 56us; 0us; 16389us; 2us; 32768us; 6us; 22us; 31us; 15us; 2us; 32768us; 6us; 25us; 31us; 15us; 2us; 32768us; 6us; 28us; 31us; 15us; 2us; 32768us; 6us; 31us; 31us; 15us; 2us; 32768us; 6us; 34us; 31us; 15us; 1us; 16394us; 31us; 15us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 1us; 32768us; 30us; 17us; 5us; 32768us; 16us; 65us; 17us; 66us; 24us; 42us; 25us; 41us; 30us; 57us; 1us; 32768us; 29us; 19us; 1us; 32768us; 14us; 20us; 1us; 32768us; 5us; 21us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 0us; 16390us; 1us; 32768us; 5us; 24us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 1us; 32768us; 12us; 26us; 1us; 32768us; 5us; 27us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 0us; 16391us; 1us; 32768us; 5us; 30us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 1us; 32768us; 9us; 32us; 1us; 32768us; 5us; 33us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 0us; 16392us; 1us; 32768us; 30us; 36us; 5us; 32768us; 16us; 65us; 17us; 66us; 24us; 42us; 25us; 41us; 30us; 57us; 1us; 32768us; 29us; 38us; 0us; 16393us; 1us; 32768us; 24us; 40us; 7us; 32768us; 0us; 39us; 7us; 4us; 10us; 35us; 11us; 29us; 13us; 23us; 15us; 16us; 25us; 5us; 0us; 16395us; 0us; 16396us; 2us; 16397us; 27us; 55us; 28us; 56us; 1us; 16398us; 28us; 56us; 0us; 16399us; 4us; 32768us; 26us; 54us; 27us; 55us; 28us; 56us; 29us; 58us; 9us; 32768us; 18us; 59us; 19us; 60us; 20us; 61us; 21us; 62us; 22us; 63us; 23us; 64us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16401us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16402us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16403us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16404us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16405us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 16406us; 26us; 54us; 27us; 55us; 28us; 56us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 0us; 16400us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 3us; 32768us; 24us; 42us; 25us; 41us; 30us; 57us; 0us; 16407us; 0us; 16408us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 8us; 9us; 12us; 13us; 14us; 16us; 20us; 24us; 25us; 28us; 31us; 34us; 37us; 40us; 42us; 50us; 52us; 58us; 60us; 62us; 64us; 72us; 73us; 75us; 83us; 85us; 87us; 95us; 96us; 98us; 106us; 108us; 110us; 118us; 119us; 121us; 127us; 129us; 130us; 132us; 140us; 141us; 142us; 145us; 147us; 148us; 153us; 163us; 167us; 171us; 175us; 179us; 183us; 187us; 191us; 195us; 199us; 203us; 204us; 208us; 212us; 216us; 220us; 224us; 228us; 229us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 1us; 3us; 3us; 8us; 8us; 8us; 4us; 3us; 1us; 1us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 3us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 65535us; 65535us; 65535us; 16391us; 65535us; 65535us; 65535us; 65535us; 65535us; 16392us; 65535us; 65535us; 65535us; 16393us; 65535us; 65535us; 16395us; 16396us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16400us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16407us; 16408us; |]
let _fsyacc_reductions ()  =    [| 
# 279 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : BJKCore.Cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startPROG));
# 288 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                                                  _1
                   )
# 39 "Parser.fsy"
                 : BJKCore.Cmd));
# 299 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "Parser.fsy"
                                                 _1
                   )
# 40 "Parser.fsy"
                 : BJKCore.Cmd));
# 310 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                                                         BJKCore.Skip
                   )
# 41 "Parser.fsy"
                 : 'COMMAND));
# 320 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                                                                  BJKCore.Asgn(_1,_3)
                   )
# 42 "Parser.fsy"
                 : 'COMMAND));
# 332 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                                                                  BJKCore.Seq(_1,_3)
                   )
# 43 "Parser.fsy"
                 : 'COMMAND));
# 344 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'BOOLEXP)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                                                  BJKCore.Seq(BJKCore.Loop(BJKCore.Seq(BJKCore.Assume(_3),_7)),BJKCore.AssumeNot(_3))
                   )
# 44 "Parser.fsy"
                 : 'COMMAND));
# 356 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "Parser.fsy"
                                                                                  BJKCore.Choice(_3,_7)
                   )
# 45 "Parser.fsy"
                 : 'COMMAND));
# 368 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            let _7 = (let data = parseState.GetInput(7) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 46 "Parser.fsy"
                                                                                  BJKCore.Choice(_3,_7)
                   )
# 46 "Parser.fsy"
                 : 'COMMAND));
# 380 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'BOOLEXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                                                  BJKCore.Assume(_3)
                   )
# 47 "Parser.fsy"
                 : 'COMMAND));
# 391 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'COMMAND)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                                                         BJKCore.Seq(BJKCore.Widening(_2),_3)
                   )
# 48 "Parser.fsy"
                 : 'COMMAND));
# 403 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "Parser.fsy"
                                             BJKCore.Var(_1)
                   )
# 49 "Parser.fsy"
                 : 'EXP));
# 414 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "Parser.fsy"
                                                  BJKCore.Fix (_1)
                   )
# 50 "Parser.fsy"
                 : 'EXP));
# 425 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                                  BJKCore.Plus(_1,_3)
                   )
# 51 "Parser.fsy"
                 : 'EXP));
# 437 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                  BJKCore.Minus(_1,_3)
                   )
# 52 "Parser.fsy"
                 : 'EXP));
# 449 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                  BJKCore.Times(_1,_3)
                   )
# 53 "Parser.fsy"
                 : 'EXP));
# 461 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "Parser.fsy"
                                               _2
                   )
# 54 "Parser.fsy"
                 : 'EXP));
# 472 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 55 "Parser.fsy"
                                                     BJKCore.Equal(_1,_3)
                   )
# 55 "Parser.fsy"
                 : 'BOOLEXP));
# 484 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                     BJKCore.NotEqual(_1,_3)
                   )
# 56 "Parser.fsy"
                 : 'BOOLEXP));
# 496 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                     BJKCore.Great(_1,_3)
                   )
# 57 "Parser.fsy"
                 : 'BOOLEXP));
# 508 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                     BJKCore.GreatEqual(_1,_3)
                   )
# 58 "Parser.fsy"
                 : 'BOOLEXP));
# 520 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                     BJKCore.Less(_1,_3)
                   )
# 59 "Parser.fsy"
                 : 'BOOLEXP));
# 532 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'EXP)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                     BJKCore.LessEqual(_1,_3)
                   )
# 60 "Parser.fsy"
                 : 'BOOLEXP));
# 544 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                               BJKCore.True
                   )
# 61 "Parser.fsy"
                 : 'BOOLEXP));
# 554 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                               BJKCore.False
                   )
# 62 "Parser.fsy"
                 : 'BOOLEXP));
|]
# 565 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 35;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let PROG lexer lexbuf : BJKCore.Cmd =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
