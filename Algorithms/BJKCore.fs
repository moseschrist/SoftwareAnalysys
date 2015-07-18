module BJKCore

    type Expr = | Plus    of Expr * Expr 
                | Minus   of Expr * Expr
                | Times   of Expr * Expr
                | Var     of int 
                | Fix     of int

    type BoolExp = | Equal      of Expr * Expr
                   | NotEqual   of Expr * Expr
                   | Great      of Expr * Expr
                   | GreatEqual of Expr * Expr
                   | Less       of Expr * Expr
                   | LessEqual  of Expr * Expr
                   | True
                   | False

    type Cmd = | Skip 
               | Asgn      of int * Expr 
               | Choice    of Cmd * Cmd 
               | Assume    of BoolExp
               | AssumeNot of BoolExp
               | Seq       of Cmd * Cmd  
               | While     of BoolExp * Cmd
               | Loop      of Cmd


    let rec highest_var_inE = function
        | Var(i)          -> i
        | Fix(i)          -> 1
        | Plus(e1, e2)    -> Util.max(highest_var_inE e1, highest_var_inE e2)
        | Minus(e1, e2)   -> Util.max(highest_var_inE e1, highest_var_inE e2)
        | Times(e1, e2)   -> Util.max(highest_var_inE e1, highest_var_inE e2)  
    and highest_var = function
        | Skip          -> 1
        | Asgn(j,e)     -> Util.max(j, highest_var_inE e)
        | Seq(C1,C2)    -> Util.max(highest_var C1, highest_var C2) 
        | Loop(C)       -> highest_var C
        | Choice(C1,C2) -> Util.max(highest_var C1, highest_var C2)
        | Assume(b)     -> match b with
                                | True               -> 1
                                | False              -> 1
                                | Equal(e1, e2)      -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | NotEqual(e1, e2)   -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | Great(e1, e2)      -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | GreatEqual(e1, e2) -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | Less(e1, e2)       -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | LessEqual(e1, e2)  -> Util.max(highest_var_inE e1, highest_var_inE e2)
        | AssumeNot(b)  -> match b with
                                | True               -> 1
                                | False              -> 1
                                | Equal(e1, e2)      -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | NotEqual(e1, e2)   -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | Great(e1, e2)      -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | GreatEqual(e1, e2) -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | Less(e1, e2)       -> Util.max(highest_var_inE e1, highest_var_inE e2)
                                | LessEqual(e1, e2)  -> Util.max(highest_var_inE e1, highest_var_inE e2)
        | While(b,c)    -> match b with
                                | True               -> 1
                                | False              -> 1
                                | Equal(e1, e2)      -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)
                                | NotEqual(e1, e2)   -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)
                                | Great(e1, e2)      -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)
                                | GreatEqual(e1, e2) -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)
                                | Less(e1, e2)       -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)
                                | LessEqual(e1, e2)  -> Util.max(Util.max(highest_var_inE e1, highest_var_inE e2), highest_var c)

    let rec prtExpr = function
        | Fix i             -> i.ToString()
        | Var n             -> "X" + n.ToString()
        | (Plus(e1, e2))    -> prtExpr(e1) + " + " + prtExpr(e2) 
        | (Minus(e1, e2))   -> prtExpr(e1) + " - " + prtExpr(e2) 
        | (Times(e1, e2))   -> prtInnerExpr(e1) + " * " + prtInnerExpr(e2)
    and prtInnerExpr = function
        | Fix i             -> i.ToString()  
        | Var n             -> "X" + n.ToString()
        | (Plus(e1, e2))    -> "( " + prtInnerExpr(e1) + " + " + prtInnerExpr(e2) + " )"
        | (Minus(e1, e2))   -> "( " + prtInnerExpr(e1) + " - " + prtInnerExpr(e2) + " )"
        | (Times(e1, e2))   -> "( " + prtInnerExpr(e1) + " * " + prtInnerExpr(e2) + " )"

    let rec prtBoolExpr = function
        | True                 -> "(TRUE)"
        | False                -> "(FALSE)"
        | (Equal(e1, e2))      -> "( " + prtExpr(e1) + " == " + prtExpr(e2) + " )"
        | (NotEqual(e1, e2))   -> "( " + prtExpr(e1) + " != " + prtExpr(e2) + " )"
        | (Great(e1, e2))      -> "( " + prtExpr(e1) + " > " + prtExpr(e2) + " )"
        | (GreatEqual(e1, e2)) -> "( " + prtExpr(e1) + " >= " + prtExpr(e2) + " )"
        | (Less(e1, e2))       -> "( " + prtExpr(e1) + " < " + prtExpr(e2) + " )"
        | (LessEqual(e1, e2))  -> "( " + prtExpr(e1) + " <= " + prtExpr(e2) + " )"

    (* print a command. In this version, some attention to pretty-printing is done, but no line breaking *)
    let rec pprt = function
        | Skip          -> "Skip"
        | Asgn(id,e)    -> (prtExpr (Var id)) + " := " + (prtExpr e)
        | Choice(s1,s2) -> "if ? then " + (pprt s1) + " else " + (pprt s2)
        | Assume(b)     -> "assume" + (prtBoolExpr b)
        | AssumeNot(b)  -> "assume" + (prtBoolExpr b)
        | Seq(s1,s2)    -> "{ " + (pprtNoBrace s1) + (pprtNoBrace s2) + " }" 
        | Loop(c)       -> "loop* { " + (pprtNoBrace c) + " }"
        | While(b,s)    -> "while" + (prtBoolExpr b) + " do { " + (pprtNoBrace s) + " }"
    and pprtNoBrace = function
        | Skip as c     -> pprt(c)
        | Asgn(id,e)    -> (prtExpr (Var id)) + " := " + (prtExpr e)
        | Choice(s1,s2) -> "if ? " + " then " + (pprt s1) + " else " + (pprt s2)
        | Assume(b)     -> "assume" + (prtBoolExpr b)
        | AssumeNot(b)  -> "assume" + (prtBoolExpr b)
        | Seq(s1,s2)    -> (pprtNoBrace s1) + (pprtNoBrace s2) 
        | Loop(c)       -> "loop* { " + (pprtNoBrace c) + " }"
        | While(b,s)    -> "while (" + (prtBoolExpr b) + ") do { " + (pprtNoBrace s) + " }"
       
    (* the tagged functions do line-breaking. Receive parameters for indentation and line-termination *)
    let rec pprti (indent:string) (finish:string) (c:Cmd) =
       match c with
            | Skip          -> indent + pprt(c) + finish
            | Asgn(id,e)    -> indent + (prtExpr (Var id)) + " := " + (prtExpr e) + finish
            | Choice(s1,s2) -> indent + "if ? then\n" + (pprti (indent + "\t") "\n" s1) + indent + "else\n" + (pprti (indent + "\t") finish s2)
            | Assume(b)     -> indent + "assume" + (prtBoolExpr b) + ";" + finish
            | AssumeNot(b)  -> indent + "assume" + (prtBoolExpr b) + ";" + finish
            | Seq(s1,s2)    -> (pprtNoBracei indent finish s1) + (pprtNoBracei indent finish s2)
            | Loop(c)       -> indent + "loop* {" + finish + (pprtNoBracei (indent + "   ") finish c) + indent + "}" + finish
            | While(b,s)    -> indent + "while " + (prtNotBoolExpri (indent + "  ") "\n" b) + ") do {\n" + (pprtNoBracei (indent + "  ") "\n" s) + indent + "}" + finish
    and pprtNoBracei (indent:string) (finish:string) (c:Cmd) =
        match c with
            | Skip          -> indent + pprt(c) + finish
            | Asgn(id,e)    -> indent + (prtExpr (Var id)) + " := " + (prtExpr e) + ";" + finish
            | Choice(s1,s2) -> indent + "choose {" + finish + (pprti (indent + "          ") finish s1) + finish + indent + "}" + finish + indent + "or {" + (pprti (indent + "          ") finish s2) + finish + indent + "}" + finish
            | Assume(b)     -> indent + "assume " + (prtBoolExpri "" "" b) + ";" + finish
            | AssumeNot(b)  -> indent + "assume " + (prtNotBoolExpri "" ""  b) + ";" + finish
            | Seq(s1,s2)    -> (pprtNoBracei indent finish s1) + (pprtNoBracei indent finish s2)
            | Loop(c)       -> indent + "loop* {" + finish + (pprtNoBracei (indent + "          ") finish c) + indent + "}" + finish
            | While(b,s)    -> indent + "while (" + (prtBoolExpr b) + ") do {\n" + (pprtNoBracei (indent + "  ") "\n" s) + indent + "}" + finish
    and prtBoolExpri (indent:string) (finish:string) (b:BoolExp) =
        match b with
            | True                 -> indent + "(TRUE)" + finish
            | False                -> indent + "(FALSE)" + finish
            | (Equal(e1, e2))      -> indent + "(" + prtExpr(e1) + " == " + prtExpr(e2) + ")" + finish
            | (NotEqual(e1, e2))   -> indent + "(" + prtExpr(e1) + " != " + prtExpr(e2) + ")" + finish
            | (Great(e1, e2))      -> indent + "(" + prtExpr(e1) + " > " + prtExpr(e2) + ")" + finish
            | (GreatEqual(e1, e2)) -> indent + "(" + prtExpr(e1) + " >= " + prtExpr(e2) + ")" + finish
            | (Less(e1, e2))       -> indent + "(" + prtExpr(e1) + " < " + prtExpr(e2) + ")" + finish
            | (LessEqual(e1, e2))  -> indent + "(" + prtExpr(e1) + " <= " + prtExpr(e2) + ")" + finish
    and prtNotBoolExpri (indent:string) (finish:string) (b:BoolExp) =
        match b with
            | True                 -> indent + "(FALSE)" + finish
            | False                -> indent + "(TRUE)" + finish
            | (Equal(e1, e2))      -> indent + "( " + prtExpr(e1) + " != " + prtExpr(e2) + " )" + finish
            | (NotEqual(e1, e2))   -> indent + "( " + prtExpr(e1) + " == " + prtExpr(e2) + " )" + finish
            | (Great(e1, e2))      -> indent + "( " + prtExpr(e1) + " <= " + prtExpr(e2) + " )" + finish
            | (GreatEqual(e1, e2)) -> indent + "( " + prtExpr(e1) + " < " + prtExpr(e2) + " )" + finish
            | (Less(e1, e2))       -> indent + "( " + prtExpr(e1) + " >= " + prtExpr(e2) + " )" + finish
            | (LessEqual(e1, e2))  -> indent + "( " + prtExpr(e1) + " > " + prtExpr(e2) + " )" + finish

     (* finally, the pretty-printer chooses whether to do line-breaking or not *)
    let prt(c:Cmd) : string =
        if (String.length (pprt c)) < 60 then
            (pprt c)+ "\n\n"
        else
            (pprti "" "\n" c)+ "\n"
    
