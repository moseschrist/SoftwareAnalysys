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
    
                        static member (+) (left: IntvBound, right:IntvBound) = 
                                  match(left,right) with
                                    (FinitBound a1, FinitBound a2) -> FinitBound(a1+a2)
                                   | (Inf , _) -> Inf
                                   | (NegInf, _) -> NegInf
                                   | (_ , Inf) -> Inf
                                   | (_ , NegInf) -> NegInf
    
                        static member (-) (left: IntvBound, right:IntvBound) = 
                                  match(left,right) with
                                    (FinitBound a1, FinitBound a2) -> FinitBound(a1-a2)
                                   | (Inf , _) -> Inf
                                   | (NegInf, _) -> NegInf
                                   | (_ , Inf) -> NegInf
                                   | (_ , NegInf) -> Inf
                        static member (*) (left: IntvBound, right:IntvBound) =
                                  match(left,right) with
                                    (FinitBound a1, FinitBound a2) -> FinitBound(a1*a2)
                                    | (Inf, _) -> Inf
                                    | (_, Inf) -> Inf
                                    | (NegInf, _) -> NegInf
                                    | (_, NegInf) -> NegInf
    
    let IntvBoundGreaterEqualThan(left:IntvBound, right:IntvBound) =
        match (left,right) with
            (FinitBound a, FinitBound b) -> a>=b
            | (Inf, _) -> true
            | (_ , Inf) -> false
            | (NegInf, _) -> false
            | (_, NegInf) -> true
            
    let IntvBoundLessEqualThan(left:IntvBound, right:IntvBound) =
        match (left,right) with
            (FinitBound a, FinitBound b) -> a<=b
            | (_, Inf) -> true
            | (Inf, _) -> false
            | (NegInf, _) -> true
            | (_, NegInf) -> false
            

    
    let IntvBoundMax(I1: IntvBound, I2:IntvBound) =
        match(I1,I2) with
            (FinitBound a1, FinitBound a2) -> FinitBound(max(a1)(a2))
            | (Inf, _) -> Inf
            | (_ , Inf) -> Inf
            | (NegInf, a) -> a
            | (a, NegInf) -> a

    let IntvBoundMin(I1: IntvBound, I2:IntvBound) =
        match(I1,I2) with
            (FinitBound a1, FinitBound a2) -> FinitBound(min(a1)(a2))
            | (Inf, a) -> a
            | (a , Inf) -> a
            | (NegInf, _) -> NegInf
            | (_, NegInf) -> NegInf


    
    type Intv = {a: IntvBound; b:IntvBound}       
                static member (+) (left: Intv, right: Intv) =
                    let {a=a1;b=b1} = left
                    let {a=a2; b=b2} = right
                    {a=a1+a2; b=b1+b2}    
    
                static member (-) (left:Intv, right:Intv) =
                    let {a=a1;b=b1} = left
                    let {a=a2; b=b2} = right
                    {a=a1+a2; b=b1+b2}
        
    let IntvMultiply (I1:Intv, I2:Intv) =
        let {a=a1;b=b1} = I1
        let {a=a2; b=b2} = I2
        {   a=IntvBoundMin(
                IntvBoundMin(a1*a2
                    ,a1*b2),
                IntvBoundMin(b1*a2,
                    b1*b2));  
            b=IntvBoundMax(
                IntvBoundMax(a1*a2
                    ,a1*b2),
                IntvBoundMax(b1*a2,
                    b1*b2))}

    
    let IntvJoin(I1,I2) =
        let {a=a1;b=b1} = I1
        let {a=a2; b=b2} = I2
        {a=IntvBoundMin(a1,a2);b=IntvBoundMax(b1,a2)}
    
    let IntvWidening(I1,I2) =
        let {a=a1;b=b1} = I1
        let {a=a2; b=b2} = I2
        let _a = if IntvBoundGreaterEqualThan(a2,a1) then a1 else NegInf
        let _b = if IntvBoundLessEqualThan(b2,b1) then b1 else Inf  
        {a = _a; b = _b}
    


    let TopInterval = {a=Inf; b=NegInf}
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


