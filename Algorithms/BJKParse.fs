module BJKParse
(* this module represents the BJK Core language parser interface.
   It allows parsing a string or an input file. *)
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

(* Parses a string. Returns a Core.Cmd - the AST matching the parsed program. *)
let aString(x : string) : BJKCore.Cmd = 
    let lexbuf = LexBuffer<_>.FromString x
    Lexer.varString := ""
    Lexer.varIndex := 0
    Lexer.varTable.Clear()
    let y = try (Parser.PROG Lexer.tokenize) lexbuf 
            with e ->
                let pos = lexbuf.EndPos
                let line = pos.Line
                let column = pos.Column
                let message = e.Message
                let lastToken = new System.String(lexbuf.Lexeme)
                let () = Util.fail("Parse failed at line " + line.ToString() + ", column " + column.ToString() + "  :\nLast token: " + lastToken + "\n")
                BJKCore.Skip
                   
    y
    
(* Parses a file. Returns Core.Cmd - the AST matching the parsed program. *)
let aFile(filename : string) : BJKCore.Cmd =
    let text = File.ReadAllText(filename);
    aString(text)


 (* Goes over an AST and breaks all nested assignment commands to a sequence of regular assignment commands. 
    All other command types remain unchanged. *)
let astBreakNestedExpressions(ast) = 
    (* breaks an assignment command into a sequence of assignment commands which are semantically identical
       in term of effect on the store *)
    let breakAsgnNestedExpression(c : BJKCore.Cmd,newVar : int) : BJKCore.Cmd =      
        let rec bne(c : BJKCore.Cmd) : BJKCore.Cmd = 
            match c with
                | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Fix y1, BJKCore.Fix y2))    -> BJKCore.Asgn(i,BJKCore.Fix (y1+y2))
                | BJKCore.Asgn(i,BJKCore.Plus(e, BJKCore.Fix y))                  -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Fix y, e))                  -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Plus(e, BJKCore.Var y))                  -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var y, e))                  -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Plus(e1, e2))                            -> BJKCore.Seq(BJKCore.Seq(bne(BJKCore.Asgn(i,e1)),bne(BJKCore.Asgn(i+1,e2))),
                                                                                       BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var i, BJKCore.Var (i+1))))
                | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Fix y1, BJKCore.Fix y2))   -> BJKCore.Asgn(i,BJKCore.Fix (y1-y2))
                | BJKCore.Asgn(i,BJKCore.Minus(e, BJKCore.Fix y))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Fix y, e))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Minus(e, BJKCore.Var y))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var y, e))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Minus(e1, e2))                           -> BJKCore.Seq(BJKCore.Seq( bne(BJKCore.Asgn(i,e1)),bne(BJKCore.Asgn(i+1,e2))),
                                                                                       BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var i, BJKCore.Var (i+1))))
                | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Fix y1, BJKCore.Fix y2))   -> BJKCore.Asgn(i,BJKCore.Fix (y1*y2))
                | BJKCore.Asgn(i,BJKCore.Times(e, BJKCore.Fix y))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Fix y, e))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var i,BJKCore.Fix y)))
                | BJKCore.Asgn(i,BJKCore.Times(e, BJKCore.Var y))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var y, e))                 -> BJKCore.Seq(bne(BJKCore.Asgn(i,e)),
                                                                                       BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var i,BJKCore.Var y)))
                | BJKCore.Asgn(i,BJKCore.Times(e1, e2))                           -> BJKCore.Seq( BJKCore.Seq( bne(BJKCore.Asgn(i,e1)),bne(BJKCore.Asgn(i+1,e2))),
                                                                                       BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var i, BJKCore.Var (i+1))))
                | _                                                               -> c

        match c with
            | BJKCore.Asgn(i,BJKCore.Fix y)                                 -> c
            | BJKCore.Asgn(i,BJKCore.Var _)                                 -> c
            | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Fix y1, BJKCore.Fix y2))  -> BJKCore.Asgn(i, BJKCore.Fix (y1+y2))
            | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var _, BJKCore.Fix _))    -> c
            | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Fix _, BJKCore.Var _))    -> c
            | BJKCore.Asgn(i,BJKCore.Plus(BJKCore.Var _, BJKCore.Var _))    -> c
            | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Fix y1, BJKCore.Fix y2)) -> BJKCore.Asgn(i, BJKCore.Fix (y1-y2))
            | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var _, BJKCore.Fix _))   -> c
            | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Fix _, BJKCore.Var _))   -> c
            | BJKCore.Asgn(i,BJKCore.Minus(BJKCore.Var _, BJKCore.Var _))   -> c
            | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Fix y1, BJKCore.Fix y2)) -> BJKCore.Asgn(i, BJKCore.Fix (y1*y2))
            | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var _, BJKCore.Fix _))   -> c
            | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Fix _, BJKCore.Var _))   -> c
            | BJKCore.Asgn(i,BJKCore.Times(BJKCore.Var _, BJKCore.Var _))   -> c
            | BJKCore.Asgn(i,e)                                             -> BJKCore.Seq(bne(BJKCore.Asgn(newVar,e)),BJKCore.Asgn(i,BJKCore.Var newVar))
            | _                                                             -> c 
            
    let rec breakNestedExp = function
            | (BJKCore.Plus(e1, e2),hv)    -> BJKCore.Plus(breakNestedExp(e1,hv),breakNestedExp(e2,hv+1))
            | (BJKCore.Minus(e1, e2),hv)   -> BJKCore.Minus(breakNestedExp(e1,hv),breakNestedExp(e2,hv+1)) 
            | (BJKCore.Times(e1, e2),hv)   -> BJKCore.Times(breakNestedExp(e1,hv),breakNestedExp(e2,hv+1))
            | (BJKCore.Var(i),hv)          -> BJKCore.Var(i)
            | (BJKCore.Fix(i),hv)          -> BJKCore.Fix(i)

    let breakNestedBoolExpretion(b : BJKCore.BoolExp,newVar : int) : BJKCore.BoolExp =      
        match b with
            | BJKCore.Equal(e1, e2)      -> BJKCore.Equal(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.NotEqual(e1, e2)   -> BJKCore.NotEqual(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.Great(e1, e2)      -> BJKCore.Great(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.GreatEqual(e1, e2) -> BJKCore.GreatEqual(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.Less(e1, e2)       -> BJKCore.Less(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.LessEqual(e1, e2)  -> BJKCore.LessEqual(breakNestedExp(e1, newVar),breakNestedExp(e2, newVar+1))
            | BJKCore.True               -> b
            | BJKCore.False              -> b

    (* goes over an AST and breaks all nested assignment commands to a sequence of regular assignment commands.
      'hv' is a variable index greater than all variable indices referenced in any of the handled expressions. *)
    let rec breakNestedExpressions = function
        | (BJKCore.Skip,hv)          -> BJKCore.Skip
        | (BJKCore.Seq(c1,c2),hv)    -> BJKCore.Seq(breakNestedExpressions(c1,hv),breakNestedExpressions(c2,hv))
        | (BJKCore.Loop(c),hv)       -> BJKCore.Loop(breakNestedExpressions(c, hv))
        | (BJKCore.Choice(c1,c2),hv) -> BJKCore.Choice(breakNestedExpressions(c1,hv),breakNestedExpressions(c2,hv))
        | (BJKCore.Assume(b),hv)     -> BJKCore.Assume(breakNestedBoolExpretion(b,hv))
        | (cmd,hv)                   -> breakAsgnNestedExpression(cmd, hv);

    breakNestedExpressions(ast,1 + BJKCore.highest_var(ast))
    
(* a function for printing of the variable name to index mapping which was generated by the last call to 'parse' *)
let parserLastVarMapping() = !(Lexer.varString)
let parserLastVarToIndexMap() = 
    let vt = Lexer.varTable
    let vt' = new Dictionary<int, string>() 
    for k in Lexer.varTable.Keys do
        vt'.Add(vt.[k], k)
    vt'


        

