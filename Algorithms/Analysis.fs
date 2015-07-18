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
                     | Inf      (* the bound is -+infinite *)
                     | NegInf
    
    let IntvBoundAdd (I1:IntvBound, I2:IntvBound) = 
            match(I1,I2) with
                (FinitBound a1, FinitBound a2) -> FinitBound(a1+a2)
               | (Inf , _) -> Inf
               | (NegInf, _) -> NegInf
               | (_ , Inf) -> Inf
               | (_ , NegInf) -> NegInf
          
    let IntvBoundSub (I1:IntvBound, I2:IntvBound) = 
            match(I1,I2) with
                (FinitBound a1, FinitBound a2) -> FinitBound(a1-a2)
               | (Inf , _) -> Inf
               | (NegInf, _) -> NegInf
               | (_ , Inf) -> Inf
               | (_ , NegInf) -> NegInf

    type Intv = {a: IntvBound; b:IntvBound}       
    
    
    let IntvAdd (I1:Intv) (I2:Intv) =
        let {a=a1;b=b1} = I1
        let {a=a2; b=b2} = I2
        {a=IntvBoundAdd(a1,a2); b=IntvBoundAdd(b1,b2)}    
    
    let IntvSub (I1:Intv) (I2:Intv) =
        let {a=a1;b=b1} = I1
        let {a=a2; b=b2} = I2
        {a=IntvBoundSub(a1,a2); b=IntvBoundSub(b1,b2)}    
    
    

    

    let TopInterval = {a=Inf("inf"); b=NegInf("-inf")}
    let TopMatrix n = Matrix.zero n n
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
        { Intervals= Array.create n AbstractDomain.TopInterval;
            RelationMatrix = AbstractDomain.TopMatrix n}

    
    
    let UpdatePState(s:_PState) (varNum:int) (newInterval:AbstractDomain.Intv) (relationVector: vector) =
        let {Intervals=i; RelationMatrix=r} = s
        i.SetValue(newInterval, varNum)
        for ind in 1 .. r.Column(varNum).Length do
            r.Column(varNum).InternalValues.SetValue(relationVector.Item(ind),ind)
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

    
    { info = "Hello \r\n 42" } : resultType
 //   {  info = BJKCore.pprti "" "\r\n" c } : resultType


