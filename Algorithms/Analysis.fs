module Analysis
open Microsoft.FSharp.Math

(*
 * Blueprint for the part where the analysis algorithm is implemented.
 *)

(************************************************************)
(* definitions of the abstract domain and its operatrs.    *)
(************************************************************)
module AbstractDomain = 
    type IntvBound = | FinitBound of int              (* the bound is integer *)
                     | Inf of string      (* the bound is -+infinite *)
                     | NegInf of string
    
    type Intv = {a: IntvBound; b:IntvBound}       
    
    let Bottom = "_"
    let Top = "T"

(************************************************************)
(* definitions of abstract states.                          *)
(************************************************************)
module AbstractSemantics = 
//    type PState = AbstractDomain.PntgLattice array   (* an F# array type *)
    type PState = int array   (* an F# array type *)
    type _PState = {Intervals:AbstractDomain.Intv array; RelationMatrix: matrix}
    


    let  initLattice n = 
        { Intervals= Array.create n ({AbstractDomain.a= AbstractDomain.NegInf("-inf"); AbstractDomain.b=AbstractDomain.Inf("inf")});
            RelationMatrix = Matrix.zero n n}
    
    let UpdatePState(s:_PState) (varNum:int) (newInterval:AbstractDomain.Intv) (relationVector: vector) =
        s

    let rec updatePState (c:BJKCore.Cmd) (pst:PState) = 
       match c with
            | BJKCore.Skip          -> pst
            | BJKCore.Asgn(id,e)    -> match e with
//                                            | BJKCore.Expr.Fix(i)       -> updateVarPState i (AbstractDomain.FinitBound i) (AbstractDomain.FinitBound i) pst
                                            | BJKCore.Expr.Fix(i)       -> pst
                                            | BJKCore.Expr.Var(i)       -> pst
                                            | BJKCore.Expr.Plus(e1, e2) -> pst
                                            | BJKCore.Expr.Minus(e1,e2) -> pst
                                            | BJKCore.Expr.Times(e1,e2) -> pst
            | BJKCore.Choice(s1,s2) -> pst
            | BJKCore.Assume(b)     -> match b with
                                            | BJKCore.BoolExp.True               -> pst
                                            | BJKCore.BoolExp.False              -> pst
                                            | BJKCore.BoolExp.Equal(e1, e2)      -> pst
                                            | BJKCore.BoolExp.NotEqual(e1, e2)   -> pst
                                            | BJKCore.BoolExp.Great(e1, e2)      -> pst
                                            | BJKCore.BoolExp.GreatEqual(e1, e2) -> pst
                                            | BJKCore.BoolExp.Less(e1, e2)       -> pst
                                            | BJKCore.BoolExp.LessEqual(e1, e2)  -> pst
            | BJKCore.AssumeNot(b)  -> match b with
                                            | BJKCore.BoolExp.True               -> pst
                                            | BJKCore.BoolExp.False              -> pst
                                            | BJKCore.BoolExp.Equal(e1, e2)      -> pst
                                            | BJKCore.BoolExp.NotEqual(e1, e2)   -> pst
                                            | BJKCore.BoolExp.Great(e1, e2)      -> pst
                                            | BJKCore.BoolExp.GreatEqual(e1, e2) -> pst
                                            | BJKCore.BoolExp.Less(e1, e2)       -> pst
                                            | BJKCore.BoolExp.LessEqual(e1, e2)  -> pst
            | BJKCore.Seq(s1,s2)    -> pst
            | BJKCore.Loop(c)       -> pst
            | BJKCore.While(b,s)    -> pst



type resultType = 
    {
        info: string
    }
    member x.toString() = x.info.ToString()


(*  main analysis function. currently quite stupid *)
let analyse(c : BJKCore.Cmd) : resultType = 

    let numVars = BJKCore.highest_var c

    let rec abstractInt(c: BJKCore.Cmd, s: AbstractSemantics.PState) : AbstractSemantics.PState =
        match c with 
                    |  BJKCore.Seq(c1,c2)        -> [| 1; 2 |]
                    |  BJKCore.Choice(c1,c2)     -> [| 1; 2 |]
                    |  BJKCore.Assume(b)         -> [| 1; 2 |]
                    |  BJKCore.AssumeNot(b)      -> [| 1; 2 |]
                    |  BJKCore.Skip              -> [| 1; 2 |]
                    |  BJKCore.Asgn(i, e)        -> [| 1; 2 |]
                    |  BJKCore.Loop(c)           -> [| 1; 2 |]
                    |  BJKCore.While(b, c)       -> [| 1; 2 |]

    let rec printCode(c: BJKCore.Cmd) : string =
        match c with 
                    |  BJKCore.Seq(c1,c2)        -> printCode(c1) + "\n" + printCode(c2) + "\n"
                    |  BJKCore.Choice(c1,c2)     -> "Choose {\n" + printCode(c1) + "\n};\n" + "or {\n" + printCode(c2) + "\n};\n"
                    |  BJKCore.Assume(b)         -> "Assume (" + b.ToString() + ");\n"
                    |  BJKCore.AssumeNot(b)      -> "Assume (" + b.ToString() + ");\n"
                    |  BJKCore.Skip              -> "Skip;\n"
                    |  BJKCore.Asgn(i, e)        -> i.ToString() + " := " + e.ToString() + ";\n"
                    |  BJKCore.Loop(c)           -> "Loop*\n"
                    |  BJKCore.While(b, c)       -> "While (" + b.ToString() + ") {\n" + printCode(c) + "\n};\n"





//    { info = "Hello \r\n 42" } : resultType
    {  info = BJKCore.pprti "" "\r\n" c } : resultType


