
datatype token = ADD | MUL | SUB | DIV | RATADD | RATSUB | RATMUL | RATDIV | MOD  | BOOLAND | BOOLOR | EQ | NEQ | LT | LEQ | GT | GEQ
datatype unary = NEG | PLUS | INV | BOOLNEG 

datatype exp = Rnum of Rat.rational | IRnum of BigInt.bigint | Inum of BigInt.bigint | Bnum of bool | 
    Var of string | UnaryOp of unary * exp | BinOp of exp * token * exp

datatype part = Block of dseq * part option | 
    CommandSeq of part * part option |
    AssignmentCmd of string * exp |
    CallCmd of string |
    ReadCmd of string |
    PrintCmd of exp |
    WhileCmd of exp * part option |
    IfCmd of exp * part option * part option

and dseq = DeclarationSeq of dseq * dseq option | 
    VarDecls of dseq option * dseq option * dseq option |
    RatVarDecls of string * dseq option |
    IntVarDecls of string * dseq option |
    BoolVarDecls of string * dseq option |
    IDlist of string * dseq option |
    ProcDecls of dseq * dseq option |
    ProcDef of string * part

datatype dtype = RATT | INTT | BOOLT
datatype dval = RatVal of Rat.rational | IntVal of BigInt.bigint | BoolVal of bool | Unassigned
datatype vtree = VNode of (string * dval * dtype) list ref * vtree option
datatype ptree = PNode of (string * part) list ref * ptree option * vtree

             

fun checkListUniqueness3 [] = true
    | checkListUniqueness3 ((id,_,_)::xs) = 
        if List.exists(fn (id',_,_) => id' = id) xs then
            raise Fail "Variable already declared"
        else
            checkListUniqueness3 xs

fun checkListUniqueness2 [] = true
    | checkListUniqueness2 ((id,_)::xs) = 
        if List.exists(fn (id',_) => id' = id) xs then
            raise Fail "Procedure already declared"
        else
            checkListUniqueness2 xs


exception Fail of string

fun solve(Rnum(x), vscope) = Rnum(x)
    | solve(IRnum(x), vscope) = IRnum(x)
    | solve(Inum(x), vscope) = Inum(x)
    | solve(Bnum(x), vscope) = Bnum(x)
    | solve(Var(x), VNode(vl, parscope)) = 
        let fun findVar ((id, vval, dtype)::xs) = 
            if id = x then
                vval
            else
                findVar xs
            | findVar [] = Unassigned
            val v = findVar(!vl)
        in 
            case v of 
                Unassigned => 
                    (case parscope of
                        SOME p => solve(Var(x), p)
                      | NONE => raise Fail "Variable not declared")
              | RatVal(x) => Rnum(x)
              | IntVal(x) => Inum(x)
              | BoolVal(x) => Bnum(x)

        end
    | solve(UnaryOp(NEG, exp), vscope) = 
        let val v = solve(exp, vscope)
        in
            case v of
                Rnum(x) => Rnum(Rat.neg x)
              | IRnum(x) => IRnum(BigInt.neg x)
              | Inum(x) => Inum(BigInt.neg x)
              | _ => raise Fail "TypeError in expression"
        end
    | solve(UnaryOp(PLUS, exp), vscope) =
        let val v = solve(exp, vscope)
        in
            case v of
                Rnum(x) => Rnum(x)
              | IRnum(x) => IRnum(x)
              | Inum(x) => Inum(x)
              | _ => raise Fail "TypeError in expression"
        end
    | solve(UnaryOp(INV, exp), vscope) =
        let val v = solve(exp, vscope)
        in
            case v of
                Rnum(x) => Rnum(case Rat.inverse(x) of SOME z => z | NONE => raise Fail "Cannot invert zero")
              | IRnum(x) => Rnum(case Rat.rat(x) of SOME z => z | NONE => raise Fail "Cannot invert zero")
              | _ => raise Fail "TypeError in expression"
        end
    | solve(UnaryOp(BOOLNEG, exp), vscope) =
        let val v = solve(exp, vscope)
        in
            case v of
              Bnum(x) => Bnum(not x)
            | _ => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, ADD, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (IRnum(x), IRnum(y)) => Inum(BigInt.add(x,y))
                | (IRnum(x), Inum(y)) => Inum(BigInt.add(x,y))
                | (Inum(x), IRnum(y)) => Inum(BigInt.add(x,y))
                | (Inum(x), Inum(y)) => Inum(BigInt.add(x,y))
                | (_, _) => raise Fail "TypeError in expression"
                
        end
    | solve(BinOp(exp1, SUB, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                 (IRnum(x), IRnum(y)) => Inum(BigInt.sub(x,y))
                | (IRnum(x), Inum(y)) => Inum(BigInt.sub(x,y))
                | (Inum(x), IRnum(y)) => Inum(BigInt.sub(x,y))
                | (Inum(x), Inum(y)) => Inum(BigInt.sub(x,y))
                | (_, _) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, MUL, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (IRnum(x), IRnum(y)) => Inum(BigInt.multiply(x,y))
                | (IRnum(x), Inum(y)) => Inum(BigInt.multiply(x,y))
                | (Inum(x), IRnum(y)) => Inum(BigInt.multiply(x,y))
                | (Inum(x), Inum(y)) => Inum(BigInt.multiply(x,y))
                | (_, _) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, DIV, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (IRnum(x), IRnum(y)) => Inum(BigInt.divide(x,y))
                | (IRnum(x), Inum(y)) => Inum(BigInt.divide(x,y))
                | (Inum(x), IRnum(y)) => Inum(BigInt.divide(x,y))
                | (Inum(x), Inum(y)) => Inum(BigInt.divide(x,y))
                | (_, _) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, MOD, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (IRnum(x), IRnum(y)) => Inum(BigInt.modulo(x,y))
                | (IRnum(x), Inum(y)) => Inum(BigInt.modulo(x,y))
                | (Inum(x), IRnum(y)) => Inum(BigInt.modulo(x,y))
                | (Inum(x), Inum(y)) => Inum(BigInt.modulo(x,y))
                | (_, _) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, RATADD, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Rnum(x), Rnum(y)) => Rnum(Rat.add(x,y))
                | (IRnum(x), Rnum(y)) => Rnum(Rat.add((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y))
                | (Rnum(x), IRnum(y)) => Rnum(Rat.add(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (IRnum(x), IRnum(y)) => Rnum(Rat.add((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, RATSUB, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Rnum(x), Rnum(y)) => Rnum(Rat.subtract(x,y))
                | (IRnum(x), Rnum(y)) => Rnum(Rat.subtract((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y))
                | (Rnum(x), IRnum(y)) => Rnum(Rat.subtract(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (IRnum(x), IRnum(y)) => Rnum(Rat.subtract((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, RATMUL, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Rnum(x), Rnum(y)) => Rnum(Rat.multiply(x,y))
                | (IRnum(x), Rnum(y)) => Rnum(Rat.multiply((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y))
                | (Rnum(x), IRnum(y)) => Rnum(Rat.multiply(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (IRnum(x), IRnum(y)) => Rnum(Rat.multiply((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, RATDIV, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Rnum(x), Rnum(y)) => Rnum((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.divide(x,y)))
                | (IRnum(x), Rnum(y)) => Rnum((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.divide((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y)))
                | (Rnum(x), IRnum(y)) => Rnum((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.divide(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y)))))
                | (IRnum(x), IRnum(y)) => Rnum((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.divide((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y)))))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, BOOLAND, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Bnum(x), Bnum(y)) => Bnum(x andalso y)
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, BOOLOR, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Bnum(x), Bnum(y)) => Bnum(x orelse y)
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, EQ, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Inum(x), Inum(y)) => Bnum(BigInt.equal(x,y))
                | (Rnum(x), Rnum(y)) => Bnum(Rat.equal(x,y))
                | (Rnum(x), IRnum(y)) => Bnum(Rat.equal(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (IRnum(x), IRnum(y)) => Bnum(BigInt.equal(x,y))
                | (IRnum(x), Rnum(y)) => Bnum(Rat.equal((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y))
                | (Inum(x), IRnum(y)) => Bnum(BigInt.equal(x,y))
                | (IRnum(x), Inum(y)) => Bnum(BigInt.equal(x,y))
                | (Bnum(x), Bnum(y)) => Bnum(x = y)
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, NEQ, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Inum(x), Inum(y)) => Bnum(not(BigInt.equal(x,y)))
                | (Rnum(x), Rnum(y)) => Bnum(not(Rat.equal(x,y)))
                | (Rnum(x), IRnum(y)) => Bnum(not(Rat.equal(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y)))))
                | (IRnum(x), IRnum(y)) => Bnum(not(BigInt.equal(x,y)))
                | (IRnum(x), Rnum(y)) => Bnum(not(Rat.equal((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y)))
                | (Inum(x), IRnum(y)) => Bnum(not(BigInt.equal(x,y)))
                | (IRnum(x), Inum(y)) => Bnum(not(BigInt.equal(x,y))) 
                | (Bnum(x), Bnum(y)) => Bnum(not(x = y))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, LT, exp2), vscope) =
        let val v1 = solve(exp1, vscope)
            val v2 = solve(exp2, vscope)
        in
            case (v1, v2) of
                (Inum(x), Inum(y)) => Bnum(BigInt.less(x,y))
                | (Rnum(x), Rnum(y)) => Bnum(Rat.less(x,y))
                | (Rnum(x), IRnum(y)) => Bnum(Rat.less(x,(fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(y))))
                | (IRnum(x), IRnum(y)) => Bnum(BigInt.less(x,y))
                | (IRnum(x), Rnum(y)) => Bnum(Rat.less((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x)),y))
                | (Inum(x), IRnum(y)) => Bnum(BigInt.less(x,y))
                | (IRnum(x), Inum(y)) => Bnum(BigInt.less(x,y))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, GT, exp2), vscope) =
        let val v3 = solve(BinOp(exp1, LT, exp2), vscope)
            val v4 = solve(BinOp(exp1, EQ, exp2), vscope)
        in  case (v3, v4) of
                (Bnum(x), Bnum(y)) => Bnum(not(x orelse y))
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, LEQ, exp2), vscope) =
        let 
            val v3 = solve(BinOp(exp1, LT, exp2), vscope)
            val v4 = solve(BinOp(exp1, EQ, exp2), vscope)
        in
            case (v3, v4) of
                (Bnum(x), Bnum(y)) => Bnum(x orelse y)
                | (_,_) => raise Fail "TypeError in expression"
        end
    | solve(BinOp(exp1, GEQ, exp2), vscope) =
        let
            val v3 = solve(BinOp(exp1, LT, exp2), vscope)
        in
            case v3 of
                Bnum(x) => Bnum(not(x))
                | _ => raise Fail "TypeError in expression"
        end





fun first(x,_) = x
fun second(_,y) = y

fun declare(DeclarationSeq(vdec, pdec)) =
    let val vl = first(declare(vdec))
        val pl = case pdec of
            NONE => []
          | SOME pdec' => second(declare(pdec'))
    in
        (vl, pl)
    end
    | declare(VarDecls(rvdec, ivdec, bvdec)) =
        let val rvl = case rvdec of
            NONE => []
          | SOME rvdec' => first(declare(rvdec'))
            val ivl = case ivdec of
            NONE => []
          | SOME ivdec' => first(declare(ivdec'))
            val bvl = case bvdec of
            NONE => []
          | SOME bvdec' => first(declare(bvdec'))
          val _ = checkListUniqueness3 (rvl@ivl@bvl)
        in
            (rvl@ivl@bvl,[])
        end
    | declare(RatVarDecls(rvdec, idl)) =
        let val rvl = case idl of SOME idl' => (rvdec,Unassigned,RATT)::first(declare(idl')) | NONE => [(rvdec,Unassigned,RATT)]
            fun specifyType (x,_,_) = (x, Unassigned, RATT)
        in
            (List.map specifyType rvl,[])
        end
    | declare(IntVarDecls(ivdec,idl)) =
        let val ivl = case idl of SOME idl' => (ivdec,Unassigned,RATT)::first(declare(idl')) | NONE => [(ivdec,Unassigned,RATT)]
            fun specifyType (x,_,_) = (x, Unassigned, INTT)
        in
            (List.map specifyType ivl,[])
        end
    | declare(BoolVarDecls(bvdec, idl)) =
        let val bvl = case idl of SOME idl' => (bvdec,Unassigned,RATT)::first(declare(idl')) | NONE => [(bvdec,Unassigned,RATT)]
            fun specifyType (x,_,_) = (x, Unassigned, BOOLT)
        in
            (List.map specifyType bvl,[])
        end
    | declare(IDlist(id, idlist)) =
        let val idl = case idlist of
            NONE => []
          | SOME idlist' => first(declare(idlist'))
        in
            ((id,Unassigned,RATT)::idl,[])
        end
    | declare(ProcDecls(pdec, pdeclist)) =
        let val pl = second(declare(pdec))
            val ppl = case pdeclist of
            NONE => []
          | SOME pdeclist' => second(declare(pdeclist'))
          val _ = checkListUniqueness2 (pl@ppl)
        in
            ([],pl@ppl)
        end
    | declare(ProcDef(id, part)) = ([],[(id, part)])


(*type of part, scope, dtype*)
fun eval (Block(dseq, cseq), vscope, pscope) = 
        let
            val (vl, pl) = declare(dseq)
            val nvscope = VNode(ref vl, SOME vscope)
            val npscope = PNode(ref pl, SOME pscope, nvscope)
        in 
            case cseq of
                NONE => ()
              | SOME cseq' => eval(cseq', nvscope, npscope)
        end
    | eval (CommandSeq(com, cseq), vscope, pscope) =
        let 
            val _ = eval(com, vscope, pscope)
        in
            case cseq of
                NONE => ()
              | SOME cseq' => eval(cseq', vscope, pscope)
        end
    | eval (AssignmentCmd(id, exp), vscope, pscope) =
        let fun findVar ((x, vval, dtype)::xs) = 
            if id = x then
                SOME (vval,dtype)
            else
                findVar xs
            | findVar [] = NONE
            fun findInScope(VNode(vl, parscope)) = 
                let val v = findVar(!vl)
                in
                    case v of
                        NONE => (case parscope of
                                    NONE => raise Fail "Variable not declared"
                                    | SOME parscope' => findInScope(parscope'))
                        | SOME (v',d') => (vl,d')
                end
            val (v,d) = findInScope(vscope)
            val e = solve(exp, vscope)
        in
            case (d,e) of
                (RATT, Rnum(x)) => v := (id, RatVal(x), RATT)::(!v)
                | (INTT, Inum(x)) => v := (id, IntVal(x), INTT)::(!v)
                | (BOOLT, Bnum(x)) => v := (id, BoolVal(x), BOOLT)::(!v)
                | (RATT, IRnum(x)) => v := (id, RatVal((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(x))), RATT)::(!v)
                | (INTT, IRnum(x)) => v := (id, IntVal(x), INTT)::(!v)
                | (_,_) => raise Fail "TypeError in assignment"
        end 
    | eval (CallCmd(id), vscope, pscope) =
        let fun findVar((x,block)::xs) = 
            if id = x then
                SOME block
            else
                findVar xs
            | findVar [] = NONE
            fun findInScope(PNode(pl, parscope, vscope)) = 
                let val p = findVar(!pl)
                in
                    case p of
                        NONE => (case parscope of
                            NONE => raise Fail "Procedure not declared"
                          | SOME parscope' => findInScope(parscope'))
                      | SOME p' => (p',vscope,PNode(pl, parscope, vscope))
                end
            val (blc, vscope', pscope') = findInScope(pscope)
        in
            eval(blc, vscope', pscope')
        end
    | eval (IfCmd(exp, cseq1, cseq2), vscope, pscope) =
        let val e = solve(exp, vscope)
        in
            case e of
                Bnum(true) => (case cseq1 of
                    NONE => ()
                  | SOME cseq1' => eval(cseq1', vscope, pscope))
                | Bnum(false) => (case cseq2 of
                    NONE => ()
                    | SOME cseq2' => eval(cseq2', vscope, pscope))
                | _ => raise Fail "TypeError in expression"
        end
    | eval (WhileCmd(exp, cseq), vscope, pscope) =
        let val e = solve(exp, vscope)
        in
            case e of
                Bnum(true) => ( case cseq of
                    NONE => ()
                    | SOME cseq' => (eval(cseq', vscope, pscope); eval(WhileCmd(exp, SOME cseq'), vscope, pscope)))
                | Bnum(false) => ()
                | _ => raise Fail "TypeError in expression"
        end
    | eval (ReadCmd(id), vscope, pscope) =
        let fun findVar ((x, vval, dtype)::xs) = 
            if id = x then
                SOME (vval,dtype)
            else
                findVar xs
            | findVar [] = NONE
            fun findInScope(VNode(vl, parscope)) = 
                let val v = findVar(!vl)
                in
                    case v of
                        NONE => (case parscope of
                            NONE => raise Fail "Variable not declared"
                          | SOME parscope' => findInScope(parscope'))
                      | SOME (v',d') => (vl,d')
                end
            val (v,d) = findInScope(vscope)
            val r1 = ((fn SOME x => x | NONE => "" ) (TextIO.inputLine(TextIO.stdIn)))
            val s = size r1
            val r = String.substring(r1, 0, s-1)
        in
            case d of 
                RATT => v := (id, RatVal(Rat.fromDecimal(r)), RATT)::(!v)
                | INTT => v := (id, IntVal(BigInt.fromString(r)), INTT)::(!v)
                | BOOLT => v := (id, BoolVal(r = "tt"), BOOLT)::(!v)
        end
    | eval (PrintCmd(exp), vscope, pscope) =
        let val e = solve(exp, vscope)
        in
            case e of
                Rnum(x) => TextIO.output(!outstream,Rat.showDecimal(x)^"\n")
                | Inum(x) => TextIO.output(!outstream,BigInt.toString(x)^"\n")
                | Bnum(x) => TextIO.output(!outstream,(if x then "tt" else "ff")^"\n")
                | IRnum(x) => TextIO.output(!outstream,BigInt.toString(x)^"\n")
                | _ => raise Fail "TypeError in printing expression"
        end



%%

%eop EOF
%pos int

%term IDENT of string | NEG | PLUS | RATADD | RATSUB | RATMUL | RATDIV | SUB | MUL | DIV | MOD | BOOLNEG 
    | BOOLAND | BOOLOR | EQ | NEQ | LT | LEQ | GT | GEQ | ASSIGN | LPAREN | RPAREN | LBRACE | RBRACE 
    | SEMICOLON | COMMA | RATIONAL | INTEGER | INT of BigInt.bigint | BOOLEAN | TRUE | FALSE 
    | PROCEDURE | CALL | IF | THEN | ELSE | FI | WHILE | DO | OD | READ | PRINT | MAKERAT | INTTORAT
    | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | INVERSE | EOF | DEC of Rat.rational | VAR

%nonterm PROGRAM | BLOCK of part | DECLARATIONSEQ of dseq | COMMANDSEQ of part | VARDECLS of dseq | RATVARDECLS of dseq 
    | INTVARDECLS of dseq | BOOLVARDECLS of dseq | PROCDECLS of dseq | PROCDEF of dseq 
    | ASSIGNMENTCMD of part | IFCMD of part | WHILECMD of part | CALLCMD of part | READCMD of part 
    | PRINTCMD of part | EXP of exp | NUM of exp | COMMAND of part | IDLIST of dseq

%name pl0

%left BOOLAND BOOLOR
%left EQ NEQ LT LEQ GT GEQ
%left RATADD RATSUB PLUS SUB
%left RATMUL RATDIV MUL DIV MOD 
%arg (fileName) : string
%noshift EOF
%nodefault
%verbose

%start PROGRAM

%%

PROGRAM : BLOCK (eval(BLOCK, VNode(ref [], NONE), PNode(ref [], NONE, VNode(ref [], NONE))))
BLOCK : DECLARATIONSEQ LBRACE COMMANDSEQ RBRACE (Block(DECLARATIONSEQ, SOME COMMANDSEQ))
    | DECLARATIONSEQ LBRACE RBRACE (Block(DECLARATIONSEQ, NONE))

DECLARATIONSEQ : VARDECLS PROCDECLS (DeclarationSeq(VARDECLS, SOME PROCDECLS))
    | VARDECLS (DeclarationSeq(VARDECLS, NONE))

VARDECLS : RATVARDECLS INTVARDECLS BOOLVARDECLS (VarDecls(SOME RATVARDECLS,SOME INTVARDECLS, SOME BOOLVARDECLS))
    | RATVARDECLS INTVARDECLS (VarDecls(SOME RATVARDECLS, SOME INTVARDECLS, NONE))
    | RATVARDECLS BOOLVARDECLS (VarDecls(SOME RATVARDECLS, NONE, SOME BOOLVARDECLS))
    | INTVARDECLS BOOLVARDECLS (VarDecls(NONE, SOME INTVARDECLS, SOME BOOLVARDECLS))
    | RATVARDECLS (VarDecls(SOME RATVARDECLS, NONE, NONE))
    | INTVARDECLS (VarDecls(NONE, SOME INTVARDECLS, NONE))
    | BOOLVARDECLS (VarDecls(NONE, NONE, SOME BOOLVARDECLS))
    | (VarDecls(NONE, NONE, NONE))

RATVARDECLS : RATIONAL IDENT IDLIST SEMICOLON (RatVarDecls(IDENT, SOME IDLIST))
    | RATIONAL IDENT SEMICOLON (RatVarDecls(IDENT, NONE))

INTVARDECLS : INTEGER IDENT IDLIST SEMICOLON (IntVarDecls(IDENT, SOME IDLIST))
    | INTEGER IDENT SEMICOLON (IntVarDecls(IDENT, NONE))

BOOLVARDECLS : BOOLEAN IDENT IDLIST SEMICOLON (BoolVarDecls(IDENT, SOME IDLIST))
    | BOOLEAN IDENT SEMICOLON (BoolVarDecls(IDENT, NONE))

IDLIST : COMMA IDENT IDLIST (IDlist(IDENT, SOME IDLIST))
    | COMMA IDENT (IDlist(IDENT, NONE))

PROCDECLS : PROCDEF SEMICOLON PROCDECLS (ProcDecls(PROCDEF, SOME PROCDECLS))
    | PROCDEF SEMICOLON (ProcDecls(PROCDEF, NONE))

PROCDEF : PROCEDURE IDENT BLOCK (ProcDef(IDENT, BLOCK))

COMMANDSEQ : COMMAND SEMICOLON COMMANDSEQ (CommandSeq(COMMAND, SOME COMMANDSEQ))
    | COMMAND SEMICOLON (CommandSeq(COMMAND, NONE))

COMMAND : IDENT ASSIGN EXP (AssignmentCmd(IDENT, EXP))
    | IF EXP THEN LBRACE COMMANDSEQ RBRACE ELSE LBRACE COMMANDSEQ RBRACE FI (IfCmd(EXP, SOME COMMANDSEQ1, SOME COMMANDSEQ2))
    | IF EXP THEN LBRACE COMMANDSEQ RBRACE ELSE LBRACE RBRACE FI (IfCmd(EXP, SOME COMMANDSEQ1, NONE))
    | IF EXP THEN LBRACE RBRACE ELSE LBRACE COMMANDSEQ RBRACE FI (IfCmd(EXP, NONE, SOME COMMANDSEQ1))
    | IF EXP THEN LBRACE RBRACE ELSE LBRACE RBRACE FI (IfCmd(EXP, NONE, NONE))
    | WHILE EXP DO LBRACE COMMANDSEQ RBRACE OD (WhileCmd(EXP, SOME COMMANDSEQ))
    | WHILE EXP DO LBRACE RBRACE OD (WhileCmd(EXP, NONE))
    | CALL IDENT (CallCmd(IDENT))
    | READ LPAREN IDENT RPAREN (ReadCmd(IDENT))
    | PRINT LPAREN EXP RPAREN (PrintCmd(EXP))

EXP : EXP PLUS EXP (BinOp(EXP1, ADD, EXP2))
    | EXP SUB EXP (BinOp(EXP1, SUB, EXP2))
    | EXP MUL EXP (BinOp(EXP1, MUL, EXP2))
    | EXP DIV EXP (BinOp(EXP1, DIV, EXP2))
    | EXP MOD EXP (BinOp(EXP1, MOD, EXP2))
    | EXP RATADD EXP (BinOp(EXP1, RATADD, EXP2))
    | EXP RATSUB EXP (BinOp(EXP1, RATSUB, EXP2))
    | EXP RATMUL EXP (BinOp(EXP1, RATMUL, EXP2))
    | EXP RATDIV EXP (BinOp(EXP1, RATDIV, EXP2))
    | EXP BOOLAND EXP (BinOp(EXP1, BOOLAND, EXP2))
    | EXP BOOLOR EXP (BinOp(EXP1, BOOLOR, EXP2))
    | EXP EQ EXP (BinOp(EXP1, EQ, EXP2))
    | EXP NEQ EXP (BinOp(EXP1, NEQ, EXP2))
    | EXP LT EXP (BinOp(EXP1, LT, EXP2))
    | EXP LEQ EXP (BinOp(EXP1, LEQ, EXP2))
    | EXP GT EXP (BinOp(EXP1, GT, EXP2))
    | EXP GEQ EXP (BinOp(EXP1, GEQ, EXP2))
    | NUM (NUM)


NUM : INT (IRnum(INT))
    | DEC (Rnum(DEC))
    | FROMDECIMAL LPAREN EXP RPAREN (BinOp(EXP,RATADD, Rnum(Rat.fromDecimal("0.(0)"))))
    | MAKERAT LPAREN EXP COMMA EXP RPAREN (BinOp(EXP1, RATDIV, EXP2))
    | INTTORAT LPAREN EXP RPAREN (BinOp(EXP,RATADD, Rnum(Rat.fromDecimal("0.(0)"))))
    | TRUE (Bnum(true))
    | FALSE (Bnum(false))
    | IDENT (Var(IDENT))
    | NEG NUM (UnaryOp(NEG, NUM))
    | PLUS NUM (UnaryOp(PLUS, NUM))
    | INVERSE NUM (UnaryOp(INV, NUM))
    | BOOLNEG NUM (UnaryOp(BOOLNEG, NUM))
    | LPAREN EXP RPAREN (EXP)

    




