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


    
    type Intv = {mutable a: IntvBound;  mutable b:IntvBound}       
                static member (+) (left: Intv, right: Intv) =
                    let {a=a1;b=b1} = left
                    let {a=a2; b=b2} = right
                    {a=a1+a2; b=b1+b2}    
    
                static member (-) (left:Intv, right:Intv) =
                    let {a=a1;b=b1} = left
                    let {a=a2; b=b2} = right
                    {a=a1+a2; b=b1+b2}
        
                static member (*) (left:Intv, right:Intv) =
                    let {a=a1;b=b1} = left
                    let {a=a2; b=b2} = right
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

                member this.Assign(other:Intv) =
                  this.a <- FinitBound(1)
                  this.b <- FinitBound(2)

                member this.Join(other:Intv) =
                  let {a=a2; b=b2} = other
                  this.a <- IntvBoundMin(this.a,a2)
                  this.b <- IntvBoundMax(this.b,b2)
                 
                member this.Widen(other:Intv) =
                  let {a=a2; b=b2} = other
                  this.a <- if IntvBoundGreaterEqualThan(a2,this.a) then this.a else NegInf
                  this.b = if IntvBoundLessEqualThan(b2,this.b) then this.b else Inf 
    
   
    type Pentagon = {var: int; interval:Intv; relations: vector}       
    
    let TopInterval = {a=Inf; b=NegInf}
    let TopMatrix n = Matrix.zero n n
    let TopVector n = Vector.zero n
    let Bottom = "_"
    let Top = "T"

(************************************************************)
(* definitions of abstract states.                          *)
(************************************************************)
module AbstractSemantics = 
    open BJKCore
    open AbstractDomain
//    type PState = AbstractDomain.PntgLattice array   (* an F# array type *)
    type PState = int array   (* an F# array type *)
    type _PState = {mutable Intervals:Intv array; mutable RelationMatrix: matrix}
                    member this.setInterval(i:int, interval: Intv) =
                        this.Intervals.SetValue(interval,i-1)
                        

    let mutable dim: int = 0
    
    let  initLattice n = 
        dim <- n
        { Intervals= Array.create n AbstractDomain.TopInterval;
            RelationMatrix = AbstractDomain.TopMatrix n}

    let mutable currentState: _PState = initLattice 0

    let rec updatePState (c:BJKCore.Cmd) (pst:_PState) = 
        let rec analyzeAsgn (id: int) (e:BJKCore.Expr) (pst:_PState) =
            match e with
                Fix(i) -> pst.setInterval(id,{a=FinitBound(i); b=FinitBound(i)})
                          pst
                | _ -> pst

        match c with
            | BJKCore.Skip          -> pst
            | BJKCore.Asgn(id,e)    -> match e with
//                                            | BJKCore.Expr.Fix(i)       -> updateVarPState i (AbstractDomain.FinitBound i) (AbstractDomain.FinitBound i) pst
                                            | BJKCore.Expr.Fix(i)       -> analyzeAsgn (id) (e) (pst)
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
        let rec CountVarsInExprs (e : Expr) =
            match e with
                Fix _ -> 0
                | Var x -> x
                | Plus(e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | Minus(e1,e2) ->  Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | Times(e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))

        let rec CountVarsInBoolExprs (e : BoolExp) =
            match e with
                True -> 0
                | False -> 0
                | Equal (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | NotEqual (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | Great (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | GreatEqual (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | Less (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))
                | LessEqual (e1,e2) -> Util.max(CountVarsInExprs(e1), CountVarsInExprs(e2))

        let rec CountVarsInCmds (c: BJKCore.Cmd) =
            match c with
               Skip  -> 0
               | Choice (c1,c2) ->  Util.max(CountVarsInCmds(c1), CountVarsInCmds(c2))
               | Seq (c1,c2) -> Util.max(CountVarsInCmds(c1), CountVarsInCmds(c2))
               | Asgn (id, e2) -> Util.max(id,CountVarsInExprs(e2))
               | Assume(e) -> CountVarsInBoolExprs(e)
               | AssumeNot(e) -> CountVarsInBoolExprs(e)
               | While(e,c) -> Util.max(CountVarsInBoolExprs(e),CountVarsInCmds(c))
               | Loop(c) -> CountVarsInCmds(c)
                 
        let _PState = initLattice(CountVarsInCmds(c))
        let analysys = updatePState(c) (_PState)



        { info = dim.ToString() } : resultType
        //   {  info = BJKCore.pprti "" "\r\n" c } : resultType


