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

                        override this.ToString() =
                                    match this with
                                    | FinitBound(i) -> i.ToString()
                                    | Inf -> "Inf"
                                    | NegInf -> "-Inf"

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

    let printIntervals (intervals: Intv array) =
        let mutable s = ""
        for ind in 0 .. (intervals.Length-1) do
            s <- s + "X" + (ind+1).ToString() + " = [" + (string)intervals.[ind].b + "," + intervals.[ind].a.ToString() + "]" + System.Environment.NewLine
        s

    let printMatrix (si :matrix) =
        let mutable r = System.Environment.NewLine
        for i in 0 .. (si.NumRows - 1) do
            for j in 0 .. (si.NumCols - 1) do
                if (int(si.[i,j]) = 1) then
                    r <- "X" + (i+1).ToString() + " < " + "X" + (j+1).ToString() + System.Environment.NewLine
        r


(************************************************************)
(* definitions of abstract states.                          *)
(************************************************************)
module AbstractSemantics = 
    open BJKCore
    open AbstractDomain
//    type PState = AbstractDomain.PntgLattice array   (* an F# array type *)
    type PState = int array   (* an F# array type *)
    type _PState = {mutable Intervals:Intv array; mutable RelationMatrix: matrix; mutable widening: int}
                    member this.setInterval(i:int, interval: Intv) =
                        this.Intervals.SetValue(interval,i-1)
                    member this.setWidening(i:int) =
                        this.widening <- i
                        

    let mutable dim: int = 0
    
    let  initLattice n = 
        dim <- n
        { Intervals= Array.create n AbstractDomain.TopInterval;
            RelationMatrix = AbstractDomain.TopMatrix n;
            widening = -1}

    let mutable currentState: _PState = initLattice 0

    let AreIntervalsEqual(intvs1 : Intv array, intvs2 : Intv array) =
        let mutable res = true
        for ind in 0 .. (intvs1.Length - 1) do
            if (intvs1.[ind].a <> intvs2.[ind].a || intvs1.[ind].b <> intvs2.[ind].b) then res <- false
        res

    let AreStatesEqual(pst1: _PState,pst2: _PState) = AreIntervalsEqual(pst1.Intervals, pst2.Intervals)

    let join(pst1: _PState,pst2: _PState) = 
        for ind in 0 .. (pst1.Intervals.Length - 1) do
            pst1.Intervals.[ind].Join(pst2.Intervals.[ind])
        pst1

    let widen(pst1: _PState,pst2: _PState) = 
        for ind in 0 .. (pst1.Intervals.Length - 1) do
            pst1.Intervals.[ind].Widen(pst2.Intervals.[ind])
        pst1
   

    let rec updatePState (c:BJKCore.Cmd) (pst:_PState) = 
        let rec analyzeAsgn (id: int) (e:BJKCore.Expr) (pst:_PState) =
            match e with
                Fix(i) -> pst.setInterval(id,{a=FinitBound(i); b=FinitBound(i)})
                          pst
                | _ -> pst

        let joinLoop (c:BJKCore.Cmd , pst: _PState) = 
            let mutable fp = false
            let mutable count = 0
            let mutable pstNew = initLattice(pst.Intervals.Length)
            while (fp <> true) do
                pstNew <-  updatePState(c)(pst)
                if (pst.widening = count) then
                   pstNew <- widen(pst, pstNew)
                else pstNew <- join(pst, pstNew)
                fp <- AreStatesEqual(pst, pstNew)
            pstNew

        match c with
            | BJKCore.Skip          -> pst
            | BJKCore.Asgn(id,e)    -> match e with
//                                            | BJKCore.Expr.Fix(i)       -> updateVarPState i (AbstractDomain.FinitBound i) (AbstractDomain.FinitBound i) pst
                                            | BJKCore.Expr.Fix(i)       -> analyzeAsgn (id) (e) (pst)
                                            | BJKCore.Expr.Var(i)       -> pst
                                            | BJKCore.Expr.Plus(e1, e2) -> pst
                                            | BJKCore.Expr.Minus(e1,e2) -> pst
                                            | BJKCore.Expr.Times(e1,e2) -> pst
            | BJKCore.Choice(s1,s2) -> join(updatePState(s1)(pst),updatePState(s2)(pst))
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
            | BJKCore.Seq(s1,s2)    -> updatePState(s2)(updatePState(s1)(pst))
            | BJKCore.Loop(c)       -> joinLoop(c, pst)
            | BJKCore.While(b,s)    -> joinLoop(BJKCore.Seq(BJKCore.Assume(b),s) ,pst)
            | BJKCore.Widening(i)   -> pst.setWidening(i) 
                                       pst


    type resultType = 
        {
            info: string
        }
        member x.toString() = x.info.ToString()


    (*  main analysis function *)
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
               | Widening(i) -> 0
                 
        let state = initLattice(CountVarsInCmds(c))
        let analysys = updatePState(c) (state)

        { info = AbstractDomain.printIntervals(state.Intervals) + AbstractDomain.printMatrix(state.RelationMatrix) 
        + System.Environment.NewLine + "Widening: " + state.widening.ToString(); } : resultType
        //   {  info = BJKCore.pprti "" "\r\n" c } : resultType


