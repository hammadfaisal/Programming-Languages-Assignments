functor pl0LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : pl0_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

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




end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\019\000\000\000\
\\001\000\001\000\020\000\000\000\
\\001\000\001\000\021\000\000\000\
\\001\000\001\000\024\000\000\000\
\\001\000\001\000\033\000\025\000\179\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\033\000\025\000\032\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\033\000\025\000\127\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\033\000\025\000\132\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\033\000\025\000\144\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\033\000\025\000\146\000\035\000\031\000\036\000\030\000\
\\040\000\029\000\043\000\028\000\044\000\027\000\000\000\
\\001\000\001\000\061\000\002\000\060\000\003\000\059\000\012\000\058\000\
\\022\000\057\000\030\000\056\000\032\000\055\000\033\000\054\000\
\\045\000\053\000\046\000\052\000\049\000\051\000\051\000\050\000\
\\053\000\049\000\000\000\
\\001\000\001\000\063\000\000\000\
\\001\000\001\000\066\000\000\000\
\\001\000\001\000\071\000\000\000\
\\001\000\003\000\190\000\004\000\190\000\005\000\190\000\006\000\086\000\
\\007\000\085\000\008\000\190\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\190\000\014\000\190\000\015\000\190\000\
\\016\000\190\000\017\000\190\000\018\000\190\000\019\000\190\000\
\\020\000\190\000\023\000\190\000\026\000\190\000\027\000\190\000\
\\037\000\190\000\041\000\190\000\000\000\
\\001\000\003\000\191\000\004\000\191\000\005\000\191\000\006\000\086\000\
\\007\000\085\000\008\000\191\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\191\000\014\000\191\000\015\000\191\000\
\\016\000\191\000\017\000\191\000\018\000\191\000\019\000\191\000\
\\020\000\191\000\023\000\191\000\026\000\191\000\027\000\191\000\
\\037\000\191\000\041\000\191\000\000\000\
\\001\000\003\000\192\000\004\000\192\000\005\000\192\000\006\000\192\000\
\\007\000\192\000\008\000\192\000\009\000\192\000\010\000\192\000\
\\011\000\192\000\013\000\192\000\014\000\192\000\015\000\192\000\
\\016\000\192\000\017\000\192\000\018\000\192\000\019\000\192\000\
\\020\000\192\000\023\000\192\000\026\000\192\000\027\000\192\000\
\\037\000\192\000\041\000\192\000\000\000\
\\001\000\003\000\193\000\004\000\193\000\005\000\193\000\006\000\193\000\
\\007\000\193\000\008\000\193\000\009\000\193\000\010\000\193\000\
\\011\000\193\000\013\000\193\000\014\000\193\000\015\000\193\000\
\\016\000\193\000\017\000\193\000\018\000\193\000\019\000\193\000\
\\020\000\193\000\023\000\193\000\026\000\193\000\027\000\193\000\
\\037\000\193\000\041\000\193\000\000\000\
\\001\000\003\000\194\000\004\000\194\000\005\000\194\000\006\000\194\000\
\\007\000\194\000\008\000\194\000\009\000\194\000\010\000\194\000\
\\011\000\194\000\013\000\194\000\014\000\194\000\015\000\194\000\
\\016\000\194\000\017\000\194\000\018\000\194\000\019\000\194\000\
\\020\000\194\000\023\000\194\000\026\000\194\000\027\000\194\000\
\\037\000\194\000\041\000\194\000\000\000\
\\001\000\003\000\195\000\004\000\195\000\005\000\195\000\006\000\086\000\
\\007\000\085\000\008\000\195\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\195\000\014\000\195\000\015\000\195\000\
\\016\000\195\000\017\000\195\000\018\000\195\000\019\000\195\000\
\\020\000\195\000\023\000\195\000\026\000\195\000\027\000\195\000\
\\037\000\195\000\041\000\195\000\000\000\
\\001\000\003\000\196\000\004\000\196\000\005\000\196\000\006\000\086\000\
\\007\000\085\000\008\000\196\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\196\000\014\000\196\000\015\000\196\000\
\\016\000\196\000\017\000\196\000\018\000\196\000\019\000\196\000\
\\020\000\196\000\023\000\196\000\026\000\196\000\027\000\196\000\
\\037\000\196\000\041\000\196\000\000\000\
\\001\000\003\000\197\000\004\000\197\000\005\000\197\000\006\000\197\000\
\\007\000\197\000\008\000\197\000\009\000\197\000\010\000\197\000\
\\011\000\197\000\013\000\197\000\014\000\197\000\015\000\197\000\
\\016\000\197\000\017\000\197\000\018\000\197\000\019\000\197\000\
\\020\000\197\000\023\000\197\000\026\000\197\000\027\000\197\000\
\\037\000\197\000\041\000\197\000\000\000\
\\001\000\003\000\198\000\004\000\198\000\005\000\198\000\006\000\198\000\
\\007\000\198\000\008\000\198\000\009\000\198\000\010\000\198\000\
\\011\000\198\000\013\000\198\000\014\000\198\000\015\000\198\000\
\\016\000\198\000\017\000\198\000\018\000\198\000\019\000\198\000\
\\020\000\198\000\023\000\198\000\026\000\198\000\027\000\198\000\
\\037\000\198\000\041\000\198\000\000\000\
\\001\000\003\000\207\000\004\000\207\000\005\000\207\000\006\000\207\000\
\\007\000\207\000\008\000\207\000\009\000\207\000\010\000\207\000\
\\011\000\207\000\013\000\207\000\014\000\207\000\015\000\207\000\
\\016\000\207\000\017\000\207\000\018\000\207\000\019\000\207\000\
\\020\000\207\000\023\000\207\000\026\000\207\000\027\000\207\000\
\\037\000\207\000\041\000\207\000\000\000\
\\001\000\003\000\208\000\004\000\208\000\005\000\208\000\006\000\208\000\
\\007\000\208\000\008\000\208\000\009\000\208\000\010\000\208\000\
\\011\000\208\000\013\000\208\000\014\000\208\000\015\000\208\000\
\\016\000\208\000\017\000\208\000\018\000\208\000\019\000\208\000\
\\020\000\208\000\023\000\208\000\026\000\208\000\027\000\208\000\
\\037\000\208\000\041\000\208\000\000\000\
\\001\000\003\000\209\000\004\000\209\000\005\000\209\000\006\000\209\000\
\\007\000\209\000\008\000\209\000\009\000\209\000\010\000\209\000\
\\011\000\209\000\013\000\209\000\014\000\209\000\015\000\209\000\
\\016\000\209\000\017\000\209\000\018\000\209\000\019\000\209\000\
\\020\000\209\000\023\000\209\000\026\000\209\000\027\000\209\000\
\\037\000\209\000\041\000\209\000\000\000\
\\001\000\003\000\210\000\004\000\210\000\005\000\210\000\006\000\210\000\
\\007\000\210\000\008\000\210\000\009\000\210\000\010\000\210\000\
\\011\000\210\000\013\000\210\000\014\000\210\000\015\000\210\000\
\\016\000\210\000\017\000\210\000\018\000\210\000\019\000\210\000\
\\020\000\210\000\023\000\210\000\026\000\210\000\027\000\210\000\
\\037\000\210\000\041\000\210\000\000\000\
\\001\000\003\000\211\000\004\000\211\000\005\000\211\000\006\000\211\000\
\\007\000\211\000\008\000\211\000\009\000\211\000\010\000\211\000\
\\011\000\211\000\013\000\211\000\014\000\211\000\015\000\211\000\
\\016\000\211\000\017\000\211\000\018\000\211\000\019\000\211\000\
\\020\000\211\000\023\000\211\000\026\000\211\000\027\000\211\000\
\\037\000\211\000\041\000\211\000\000\000\
\\001\000\003\000\212\000\004\000\212\000\005\000\212\000\006\000\212\000\
\\007\000\212\000\008\000\212\000\009\000\212\000\010\000\212\000\
\\011\000\212\000\013\000\212\000\014\000\212\000\015\000\212\000\
\\016\000\212\000\017\000\212\000\018\000\212\000\019\000\212\000\
\\020\000\212\000\023\000\212\000\026\000\212\000\027\000\212\000\
\\037\000\212\000\041\000\212\000\000\000\
\\001\000\003\000\213\000\004\000\213\000\005\000\213\000\006\000\213\000\
\\007\000\213\000\008\000\213\000\009\000\213\000\010\000\213\000\
\\011\000\213\000\013\000\213\000\014\000\213\000\015\000\213\000\
\\016\000\213\000\017\000\213\000\018\000\213\000\019\000\213\000\
\\020\000\213\000\023\000\213\000\026\000\213\000\027\000\213\000\
\\037\000\213\000\041\000\213\000\000\000\
\\001\000\003\000\214\000\004\000\214\000\005\000\214\000\006\000\214\000\
\\007\000\214\000\008\000\214\000\009\000\214\000\010\000\214\000\
\\011\000\214\000\013\000\214\000\014\000\214\000\015\000\214\000\
\\016\000\214\000\017\000\214\000\018\000\214\000\019\000\214\000\
\\020\000\214\000\023\000\214\000\026\000\214\000\027\000\214\000\
\\037\000\214\000\041\000\214\000\000\000\
\\001\000\003\000\215\000\004\000\215\000\005\000\215\000\006\000\215\000\
\\007\000\215\000\008\000\215\000\009\000\215\000\010\000\215\000\
\\011\000\215\000\013\000\215\000\014\000\215\000\015\000\215\000\
\\016\000\215\000\017\000\215\000\018\000\215\000\019\000\215\000\
\\020\000\215\000\023\000\215\000\026\000\215\000\027\000\215\000\
\\037\000\215\000\041\000\215\000\000\000\
\\001\000\003\000\216\000\004\000\216\000\005\000\216\000\006\000\216\000\
\\007\000\216\000\008\000\216\000\009\000\216\000\010\000\216\000\
\\011\000\216\000\013\000\216\000\014\000\216\000\015\000\216\000\
\\016\000\216\000\017\000\216\000\018\000\216\000\019\000\216\000\
\\020\000\216\000\023\000\216\000\026\000\216\000\027\000\216\000\
\\037\000\216\000\041\000\216\000\000\000\
\\001\000\003\000\217\000\004\000\217\000\005\000\217\000\006\000\217\000\
\\007\000\217\000\008\000\217\000\009\000\217\000\010\000\217\000\
\\011\000\217\000\013\000\217\000\014\000\217\000\015\000\217\000\
\\016\000\217\000\017\000\217\000\018\000\217\000\019\000\217\000\
\\020\000\217\000\023\000\217\000\026\000\217\000\027\000\217\000\
\\037\000\217\000\041\000\217\000\000\000\
\\001\000\003\000\218\000\004\000\218\000\005\000\218\000\006\000\218\000\
\\007\000\218\000\008\000\218\000\009\000\218\000\010\000\218\000\
\\011\000\218\000\013\000\218\000\014\000\218\000\015\000\218\000\
\\016\000\218\000\017\000\218\000\018\000\218\000\019\000\218\000\
\\020\000\218\000\023\000\218\000\026\000\218\000\027\000\218\000\
\\037\000\218\000\041\000\218\000\000\000\
\\001\000\003\000\219\000\004\000\219\000\005\000\219\000\006\000\219\000\
\\007\000\219\000\008\000\219\000\009\000\219\000\010\000\219\000\
\\011\000\219\000\013\000\219\000\014\000\219\000\015\000\219\000\
\\016\000\219\000\017\000\219\000\018\000\219\000\019\000\219\000\
\\020\000\219\000\023\000\219\000\026\000\219\000\027\000\219\000\
\\037\000\219\000\041\000\219\000\000\000\
\\001\000\003\000\220\000\004\000\220\000\005\000\220\000\006\000\220\000\
\\007\000\220\000\008\000\220\000\009\000\220\000\010\000\220\000\
\\011\000\220\000\013\000\220\000\014\000\220\000\015\000\220\000\
\\016\000\220\000\017\000\220\000\018\000\220\000\019\000\220\000\
\\020\000\220\000\023\000\220\000\026\000\220\000\027\000\220\000\
\\037\000\220\000\041\000\220\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\199\000\014\000\199\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\199\000\026\000\199\000\027\000\199\000\
\\037\000\199\000\041\000\199\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\200\000\014\000\200\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\200\000\026\000\200\000\027\000\200\000\
\\037\000\200\000\041\000\200\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\201\000\014\000\201\000\015\000\201\000\
\\016\000\201\000\017\000\201\000\018\000\201\000\019\000\201\000\
\\020\000\201\000\023\000\201\000\026\000\201\000\027\000\201\000\
\\037\000\201\000\041\000\201\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\202\000\014\000\202\000\015\000\202\000\
\\016\000\202\000\017\000\202\000\018\000\202\000\019\000\202\000\
\\020\000\202\000\023\000\202\000\026\000\202\000\027\000\202\000\
\\037\000\202\000\041\000\202\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\203\000\014\000\203\000\015\000\203\000\
\\016\000\203\000\017\000\203\000\018\000\203\000\019\000\203\000\
\\020\000\203\000\023\000\203\000\026\000\203\000\027\000\203\000\
\\037\000\203\000\041\000\203\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\204\000\014\000\204\000\015\000\204\000\
\\016\000\204\000\017\000\204\000\018\000\204\000\019\000\204\000\
\\020\000\204\000\023\000\204\000\026\000\204\000\027\000\204\000\
\\037\000\204\000\041\000\204\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\205\000\014\000\205\000\015\000\205\000\
\\016\000\205\000\017\000\205\000\018\000\205\000\019\000\205\000\
\\020\000\205\000\023\000\205\000\026\000\205\000\027\000\205\000\
\\037\000\205\000\041\000\205\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\206\000\014\000\206\000\015\000\206\000\
\\016\000\206\000\017\000\206\000\018\000\206\000\019\000\206\000\
\\020\000\206\000\023\000\206\000\026\000\206\000\027\000\206\000\
\\037\000\206\000\041\000\206\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\101\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\124\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\128\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\129\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\023\000\139\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\026\000\180\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\027\000\130\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\037\000\098\000\000\000\
\\001\000\003\000\089\000\004\000\088\000\005\000\087\000\006\000\086\000\
\\007\000\085\000\008\000\084\000\009\000\083\000\010\000\082\000\
\\011\000\081\000\013\000\080\000\014\000\079\000\015\000\078\000\
\\016\000\077\000\017\000\076\000\018\000\075\000\019\000\074\000\
\\020\000\073\000\041\000\072\000\000\000\
\\001\000\021\000\064\000\000\000\
\\001\000\022\000\045\000\000\000\
\\001\000\022\000\046\000\000\000\
\\001\000\022\000\091\000\000\000\
\\001\000\022\000\092\000\000\000\
\\001\000\022\000\093\000\000\000\
\\001\000\023\000\102\000\000\000\
\\001\000\024\000\157\000\000\000\
\\001\000\024\000\158\000\034\000\017\000\000\000\
\\001\000\024\000\159\000\034\000\159\000\000\000\
\\001\000\024\000\160\000\031\000\009\000\034\000\160\000\000\000\
\\001\000\024\000\161\000\034\000\161\000\000\000\
\\001\000\024\000\162\000\034\000\162\000\000\000\
\\001\000\024\000\163\000\029\000\010\000\031\000\009\000\034\000\163\000\000\000\
\\001\000\024\000\164\000\031\000\009\000\034\000\164\000\000\000\
\\001\000\024\000\165\000\034\000\165\000\000\000\
\\001\000\024\000\166\000\028\000\011\000\029\000\010\000\031\000\009\000\
\\034\000\166\000\000\000\
\\001\000\024\000\167\000\029\000\167\000\031\000\167\000\034\000\167\000\000\000\
\\001\000\024\000\168\000\029\000\168\000\031\000\168\000\034\000\168\000\000\000\
\\001\000\024\000\169\000\031\000\169\000\034\000\169\000\000\000\
\\001\000\024\000\170\000\031\000\170\000\034\000\170\000\000\000\
\\001\000\024\000\171\000\034\000\171\000\000\000\
\\001\000\024\000\172\000\034\000\172\000\000\000\
\\001\000\024\000\175\000\000\000\
\\001\000\024\000\176\000\034\000\017\000\000\000\
\\001\000\024\000\018\000\000\000\
\\001\000\024\000\103\000\000\000\
\\001\000\024\000\125\000\000\000\
\\001\000\024\000\141\000\000\000\
\\001\000\024\000\142\000\000\000\
\\001\000\025\000\178\000\000\000\
\\001\000\025\000\044\000\000\000\
\\001\000\025\000\133\000\000\000\
\\001\000\025\000\136\000\000\000\
\\001\000\025\000\147\000\000\000\
\\001\000\025\000\149\000\000\000\
\\001\000\026\000\155\000\052\000\155\000\000\000\
\\001\000\026\000\156\000\052\000\156\000\000\000\
\\001\000\026\000\173\000\000\000\
\\001\000\026\000\174\000\027\000\035\000\000\000\
\\001\000\026\000\177\000\000\000\
\\001\000\026\000\181\000\000\000\
\\001\000\026\000\182\000\000\000\
\\001\000\026\000\183\000\000\000\
\\001\000\026\000\184\000\000\000\
\\001\000\026\000\185\000\000\000\
\\001\000\026\000\186\000\000\000\
\\001\000\026\000\187\000\000\000\
\\001\000\026\000\188\000\000\000\
\\001\000\026\000\189\000\000\000\
\\001\000\026\000\023\000\000\000\
\\001\000\026\000\036\000\027\000\035\000\000\000\
\\001\000\026\000\038\000\027\000\035\000\000\000\
\\001\000\026\000\040\000\027\000\035\000\000\000\
\\001\000\026\000\043\000\000\000\
\\001\000\026\000\065\000\000\000\
\\001\000\026\000\067\000\000\000\
\\001\000\026\000\068\000\000\000\
\\001\000\038\000\137\000\000\000\
\\001\000\038\000\140\000\000\000\
\\001\000\039\000\148\000\000\000\
\\001\000\039\000\150\000\000\000\
\\001\000\039\000\151\000\000\000\
\\001\000\039\000\152\000\000\000\
\\001\000\042\000\134\000\000\000\
\\001\000\042\000\138\000\000\000\
\\001\000\052\000\000\000\000\000\
\\001\000\052\000\154\000\000\000\
\"
val actionRowNumbers =
"\070\000\069\000\068\000\067\000\
\\062\000\079\000\121\000\000\000\
\\001\000\002\000\066\000\065\000\
\\064\000\104\000\061\000\003\000\
\\005\000\105\000\106\000\107\000\
\\063\000\078\000\070\000\108\000\
\\085\000\055\000\056\000\010\000\
\\010\000\011\000\091\000\054\000\
\\109\000\012\000\076\000\110\000\
\\074\000\111\000\072\000\077\000\
\\094\000\004\000\090\000\010\000\
\\013\000\023\000\053\000\025\000\
\\010\000\057\000\058\000\059\000\
\\030\000\029\000\024\000\010\000\
\\010\000\010\000\010\000\031\000\
\\052\000\101\000\010\000\075\000\
\\093\000\073\000\071\000\084\000\
\\045\000\060\000\080\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\010\000\010\000\010\000\010\000\
\\034\000\010\000\010\000\010\000\
\\046\000\035\000\033\000\032\000\
\\081\000\050\000\092\000\103\000\
\\102\000\006\000\044\000\043\000\
\\042\000\041\000\040\000\039\000\
\\038\000\037\000\018\000\017\000\
\\016\000\015\000\022\000\021\000\
\\020\000\019\000\014\000\047\000\
\\048\000\051\000\036\000\007\000\
\\086\000\118\000\026\000\028\000\
\\010\000\087\000\112\000\119\000\
\\100\000\049\000\113\000\082\000\
\\099\000\027\000\083\000\008\000\
\\009\000\088\000\114\000\089\000\
\\115\000\116\000\098\000\117\000\
\\096\000\097\000\095\000\120\000"
val gotoT =
"\
\\001\000\151\000\002\000\006\000\003\000\005\000\005\000\004\000\
\\006\000\003\000\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\008\000\010\000\000\000\
\\007\000\012\000\008\000\011\000\000\000\
\\009\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\019\000\023\000\000\000\
\\020\000\032\000\000\000\
\\020\000\035\000\000\000\
\\020\000\037\000\000\000\
\\000\000\
\\009\000\039\000\010\000\013\000\000\000\
\\002\000\040\000\003\000\005\000\005\000\004\000\006\000\003\000\
\\007\000\002\000\008\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\046\000\018\000\045\000\000\000\
\\017\000\060\000\018\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\067\000\019\000\023\000\000\000\
\\000\000\
\\017\000\068\000\018\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\088\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\092\000\018\000\045\000\000\000\
\\018\000\093\000\000\000\
\\018\000\094\000\000\000\
\\018\000\095\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\097\000\018\000\045\000\000\000\
\\000\000\
\\020\000\098\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\102\000\018\000\045\000\000\000\
\\017\000\103\000\018\000\045\000\000\000\
\\017\000\104\000\018\000\045\000\000\000\
\\017\000\105\000\018\000\045\000\000\000\
\\017\000\106\000\018\000\045\000\000\000\
\\017\000\107\000\018\000\045\000\000\000\
\\017\000\108\000\018\000\045\000\000\000\
\\017\000\109\000\018\000\045\000\000\000\
\\017\000\110\000\018\000\045\000\000\000\
\\017\000\111\000\018\000\045\000\000\000\
\\017\000\112\000\018\000\045\000\000\000\
\\017\000\113\000\018\000\045\000\000\000\
\\017\000\114\000\018\000\045\000\000\000\
\\017\000\115\000\018\000\045\000\000\000\
\\017\000\116\000\018\000\045\000\000\000\
\\017\000\117\000\018\000\045\000\000\000\
\\017\000\118\000\018\000\045\000\000\000\
\\000\000\
\\017\000\119\000\018\000\045\000\000\000\
\\017\000\120\000\018\000\045\000\000\000\
\\017\000\121\000\018\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\124\000\019\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\129\000\019\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\133\000\018\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\141\000\019\000\023\000\000\000\
\\004\000\143\000\019\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 152
val numrules = 67
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | DEC of unit ->  (Rat.rational) | INT of unit ->  (BigInt.bigint)
 | IDENT of unit ->  (string) | IDLIST of unit ->  (dseq)
 | COMMAND of unit ->  (part) | NUM of unit ->  (exp)
 | EXP of unit ->  (exp) | PRINTCMD of unit ->  (part)
 | READCMD of unit ->  (part) | CALLCMD of unit ->  (part)
 | WHILECMD of unit ->  (part) | IFCMD of unit ->  (part)
 | ASSIGNMENTCMD of unit ->  (part) | PROCDEF of unit ->  (dseq)
 | PROCDECLS of unit ->  (dseq) | BOOLVARDECLS of unit ->  (dseq)
 | INTVARDECLS of unit ->  (dseq) | RATVARDECLS of unit ->  (dseq)
 | VARDECLS of unit ->  (dseq) | COMMANDSEQ of unit ->  (part)
 | DECLARATIONSEQ of unit ->  (dseq) | BLOCK of unit ->  (part)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 51) => true | _ => false
val showTerminal =
fn (T 0) => "IDENT"
  | (T 1) => "NEG"
  | (T 2) => "PLUS"
  | (T 3) => "RATADD"
  | (T 4) => "RATSUB"
  | (T 5) => "RATMUL"
  | (T 6) => "RATDIV"
  | (T 7) => "SUB"
  | (T 8) => "MUL"
  | (T 9) => "DIV"
  | (T 10) => "MOD"
  | (T 11) => "BOOLNEG"
  | (T 12) => "BOOLAND"
  | (T 13) => "BOOLOR"
  | (T 14) => "EQ"
  | (T 15) => "NEQ"
  | (T 16) => "LT"
  | (T 17) => "LEQ"
  | (T 18) => "GT"
  | (T 19) => "GEQ"
  | (T 20) => "ASSIGN"
  | (T 21) => "LPAREN"
  | (T 22) => "RPAREN"
  | (T 23) => "LBRACE"
  | (T 24) => "RBRACE"
  | (T 25) => "SEMICOLON"
  | (T 26) => "COMMA"
  | (T 27) => "RATIONAL"
  | (T 28) => "INTEGER"
  | (T 29) => "INT"
  | (T 30) => "BOOLEAN"
  | (T 31) => "TRUE"
  | (T 32) => "FALSE"
  | (T 33) => "PROCEDURE"
  | (T 34) => "CALL"
  | (T 35) => "IF"
  | (T 36) => "THEN"
  | (T 37) => "ELSE"
  | (T 38) => "FI"
  | (T 39) => "WHILE"
  | (T 40) => "DO"
  | (T 41) => "OD"
  | (T 42) => "READ"
  | (T 43) => "PRINT"
  | (T 44) => "MAKERAT"
  | (T 45) => "INTTORAT"
  | (T 46) => "SHOWRAT"
  | (T 47) => "SHOWDECIMAL"
  | (T 48) => "FROMDECIMAL"
  | (T 49) => "TODECIMAL"
  | (T 50) => "INVERSE"
  | (T 51) => "EOF"
  | (T 52) => "DEC"
  | (T 53) => "VAR"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 53) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46)
 $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39)
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.BLOCK BLOCK1, BLOCK1left, BLOCK1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
BLOCK as BLOCK1) = BLOCK1 ()
 in (
eval(BLOCK, VNode(ref [], NONE), PNode(ref [], NONE, VNode(ref [], NONE)))
)
end; ()))
 in ( LrTable.NT 0, ( result, BLOCK1left, BLOCK1right), rest671)
end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: ( _, ( MlyValue.DECLARATIONSEQ 
DECLARATIONSEQ1, DECLARATIONSEQ1left, _)) :: rest671)) => let val  
result = MlyValue.BLOCK (fn _ => let val  (DECLARATIONSEQ as 
DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (Block(DECLARATIONSEQ, SOME COMMANDSEQ))
end)
 in ( LrTable.NT 1, ( result, DECLARATIONSEQ1left, RBRACE1right), 
rest671)
end
|  ( 2, ( ( _, ( _, _, RBRACE1right)) :: _ :: ( _, ( 
MlyValue.DECLARATIONSEQ DECLARATIONSEQ1, DECLARATIONSEQ1left, _)) :: 
rest671)) => let val  result = MlyValue.BLOCK (fn _ => let val  (
DECLARATIONSEQ as DECLARATIONSEQ1) = DECLARATIONSEQ1 ()
 in (Block(DECLARATIONSEQ, NONE))
end)
 in ( LrTable.NT 1, ( result, DECLARATIONSEQ1left, RBRACE1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.PROCDECLS PROCDECLS1, _, PROCDECLS1right))
 :: ( _, ( MlyValue.VARDECLS VARDECLS1, VARDECLS1left, _)) :: rest671)
) => let val  result = MlyValue.DECLARATIONSEQ (fn _ => let val  (
VARDECLS as VARDECLS1) = VARDECLS1 ()
 val  (PROCDECLS as PROCDECLS1) = PROCDECLS1 ()
 in (DeclarationSeq(VARDECLS, SOME PROCDECLS))
end)
 in ( LrTable.NT 2, ( result, VARDECLS1left, PROCDECLS1right), rest671
)
end
|  ( 4, ( ( _, ( MlyValue.VARDECLS VARDECLS1, VARDECLS1left, 
VARDECLS1right)) :: rest671)) => let val  result = 
MlyValue.DECLARATIONSEQ (fn _ => let val  (VARDECLS as VARDECLS1) = 
VARDECLS1 ()
 in (DeclarationSeq(VARDECLS, NONE))
end)
 in ( LrTable.NT 2, ( result, VARDECLS1left, VARDECLS1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.BOOLVARDECLS BOOLVARDECLS1, _, 
BOOLVARDECLS1right)) :: ( _, ( MlyValue.INTVARDECLS INTVARDECLS1, _, _
)) :: ( _, ( MlyValue.RATVARDECLS RATVARDECLS1, RATVARDECLS1left, _))
 :: rest671)) => let val  result = MlyValue.VARDECLS (fn _ => let val 
 (RATVARDECLS as RATVARDECLS1) = RATVARDECLS1 ()
 val  (INTVARDECLS as INTVARDECLS1) = INTVARDECLS1 ()
 val  (BOOLVARDECLS as BOOLVARDECLS1) = BOOLVARDECLS1 ()
 in (VarDecls(SOME RATVARDECLS,SOME INTVARDECLS, SOME BOOLVARDECLS))

end)
 in ( LrTable.NT 4, ( result, RATVARDECLS1left, BOOLVARDECLS1right), 
rest671)
end
|  ( 6, ( ( _, ( MlyValue.INTVARDECLS INTVARDECLS1, _, 
INTVARDECLS1right)) :: ( _, ( MlyValue.RATVARDECLS RATVARDECLS1, 
RATVARDECLS1left, _)) :: rest671)) => let val  result = 
MlyValue.VARDECLS (fn _ => let val  (RATVARDECLS as RATVARDECLS1) = 
RATVARDECLS1 ()
 val  (INTVARDECLS as INTVARDECLS1) = INTVARDECLS1 ()
 in (VarDecls(SOME RATVARDECLS, SOME INTVARDECLS, NONE))
end)
 in ( LrTable.NT 4, ( result, RATVARDECLS1left, INTVARDECLS1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.BOOLVARDECLS BOOLVARDECLS1, _, 
BOOLVARDECLS1right)) :: ( _, ( MlyValue.RATVARDECLS RATVARDECLS1, 
RATVARDECLS1left, _)) :: rest671)) => let val  result = 
MlyValue.VARDECLS (fn _ => let val  (RATVARDECLS as RATVARDECLS1) = 
RATVARDECLS1 ()
 val  (BOOLVARDECLS as BOOLVARDECLS1) = BOOLVARDECLS1 ()
 in (VarDecls(SOME RATVARDECLS, NONE, SOME BOOLVARDECLS))
end)
 in ( LrTable.NT 4, ( result, RATVARDECLS1left, BOOLVARDECLS1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.BOOLVARDECLS BOOLVARDECLS1, _, 
BOOLVARDECLS1right)) :: ( _, ( MlyValue.INTVARDECLS INTVARDECLS1, 
INTVARDECLS1left, _)) :: rest671)) => let val  result = 
MlyValue.VARDECLS (fn _ => let val  (INTVARDECLS as INTVARDECLS1) = 
INTVARDECLS1 ()
 val  (BOOLVARDECLS as BOOLVARDECLS1) = BOOLVARDECLS1 ()
 in (VarDecls(NONE, SOME INTVARDECLS, SOME BOOLVARDECLS))
end)
 in ( LrTable.NT 4, ( result, INTVARDECLS1left, BOOLVARDECLS1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.RATVARDECLS RATVARDECLS1, RATVARDECLS1left, 
RATVARDECLS1right)) :: rest671)) => let val  result = 
MlyValue.VARDECLS (fn _ => let val  (RATVARDECLS as RATVARDECLS1) = 
RATVARDECLS1 ()
 in (VarDecls(SOME RATVARDECLS, NONE, NONE))
end)
 in ( LrTable.NT 4, ( result, RATVARDECLS1left, RATVARDECLS1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.INTVARDECLS INTVARDECLS1, INTVARDECLS1left,
 INTVARDECLS1right)) :: rest671)) => let val  result = 
MlyValue.VARDECLS (fn _ => let val  (INTVARDECLS as INTVARDECLS1) = 
INTVARDECLS1 ()
 in (VarDecls(NONE, SOME INTVARDECLS, NONE))
end)
 in ( LrTable.NT 4, ( result, INTVARDECLS1left, INTVARDECLS1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.BOOLVARDECLS BOOLVARDECLS1, 
BOOLVARDECLS1left, BOOLVARDECLS1right)) :: rest671)) => let val  
result = MlyValue.VARDECLS (fn _ => let val  (BOOLVARDECLS as 
BOOLVARDECLS1) = BOOLVARDECLS1 ()
 in (VarDecls(NONE, NONE, SOME BOOLVARDECLS))
end)
 in ( LrTable.NT 4, ( result, BOOLVARDECLS1left, BOOLVARDECLS1right), 
rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.VARDECLS (fn _ => (
VarDecls(NONE, NONE, NONE)))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDLIST 
IDLIST1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, 
RATIONAL1left, _)) :: rest671)) => let val  result = 
MlyValue.RATVARDECLS (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (IDLIST as IDLIST1) = IDLIST1 ()
 in (RatVarDecls(IDENT, SOME IDLIST))
end)
 in ( LrTable.NT 5, ( result, RATIONAL1left, SEMICOLON1right), rest671
)
end
|  ( 14, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDENT 
IDENT1, _, _)) :: ( _, ( _, RATIONAL1left, _)) :: rest671)) => let
 val  result = MlyValue.RATVARDECLS (fn _ => let val  (IDENT as IDENT1
) = IDENT1 ()
 in (RatVarDecls(IDENT, NONE))
end)
 in ( LrTable.NT 5, ( result, RATIONAL1left, SEMICOLON1right), rest671
)
end
|  ( 15, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDLIST 
IDLIST1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, 
INTEGER1left, _)) :: rest671)) => let val  result = 
MlyValue.INTVARDECLS (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (IDLIST as IDLIST1) = IDLIST1 ()
 in (IntVarDecls(IDENT, SOME IDLIST))
end)
 in ( LrTable.NT 6, ( result, INTEGER1left, SEMICOLON1right), rest671)

end
|  ( 16, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDENT 
IDENT1, _, _)) :: ( _, ( _, INTEGER1left, _)) :: rest671)) => let val 
 result = MlyValue.INTVARDECLS (fn _ => let val  (IDENT as IDENT1) = 
IDENT1 ()
 in (IntVarDecls(IDENT, NONE))
end)
 in ( LrTable.NT 6, ( result, INTEGER1left, SEMICOLON1right), rest671)

end
|  ( 17, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDLIST 
IDLIST1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, 
BOOLEAN1left, _)) :: rest671)) => let val  result = 
MlyValue.BOOLVARDECLS (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (IDLIST as IDLIST1) = IDLIST1 ()
 in (BoolVarDecls(IDENT, SOME IDLIST))
end)
 in ( LrTable.NT 7, ( result, BOOLEAN1left, SEMICOLON1right), rest671)

end
|  ( 18, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IDENT 
IDENT1, _, _)) :: ( _, ( _, BOOLEAN1left, _)) :: rest671)) => let val 
 result = MlyValue.BOOLVARDECLS (fn _ => let val  (IDENT as IDENT1) = 
IDENT1 ()
 in (BoolVarDecls(IDENT, NONE))
end)
 in ( LrTable.NT 7, ( result, BOOLEAN1left, SEMICOLON1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.IDLIST IDLIST1, _, IDLIST1right)) :: ( _, (
 MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671
)) => let val  result = MlyValue.IDLIST (fn _ => let val  (IDENT as 
IDENT1) = IDENT1 ()
 val  (IDLIST as IDLIST1) = IDLIST1 ()
 in (IDlist(IDENT, SOME IDLIST))
end)
 in ( LrTable.NT 19, ( result, COMMA1left, IDLIST1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: ( _, ( _,
 COMMA1left, _)) :: rest671)) => let val  result = MlyValue.IDLIST (fn
 _ => let val  (IDENT as IDENT1) = IDENT1 ()
 in (IDlist(IDENT, NONE))
end)
 in ( LrTable.NT 19, ( result, COMMA1left, IDENT1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.PROCDECLS PROCDECLS1, _, PROCDECLS1right))
 :: _ :: ( _, ( MlyValue.PROCDEF PROCDEF1, PROCDEF1left, _)) :: 
rest671)) => let val  result = MlyValue.PROCDECLS (fn _ => let val  (
PROCDEF as PROCDEF1) = PROCDEF1 ()
 val  (PROCDECLS as PROCDECLS1) = PROCDECLS1 ()
 in (ProcDecls(PROCDEF, SOME PROCDECLS))
end)
 in ( LrTable.NT 8, ( result, PROCDEF1left, PROCDECLS1right), rest671)

end
|  ( 22, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.PROCDEF 
PROCDEF1, PROCDEF1left, _)) :: rest671)) => let val  result = 
MlyValue.PROCDECLS (fn _ => let val  (PROCDEF as PROCDEF1) = PROCDEF1
 ()
 in (ProcDecls(PROCDEF, NONE))
end)
 in ( LrTable.NT 8, ( result, PROCDEF1left, SEMICOLON1right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.BLOCK BLOCK1, _, BLOCK1right)) :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, PROCEDURE1left, _)) :: 
rest671)) => let val  result = MlyValue.PROCDEF (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 val  (BLOCK as BLOCK1) = BLOCK1 ()
 in (ProcDef(IDENT, BLOCK))
end)
 in ( LrTable.NT 9, ( result, PROCEDURE1left, BLOCK1right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.COMMANDSEQ COMMANDSEQ1, _, COMMANDSEQ1right
)) :: _ :: ( _, ( MlyValue.COMMAND COMMAND1, COMMAND1left, _)) :: 
rest671)) => let val  result = MlyValue.COMMANDSEQ (fn _ => let val  (
COMMAND as COMMAND1) = COMMAND1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (CommandSeq(COMMAND, SOME COMMANDSEQ))
end)
 in ( LrTable.NT 3, ( result, COMMAND1left, COMMANDSEQ1right), rest671
)
end
|  ( 25, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.COMMAND 
COMMAND1, COMMAND1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMANDSEQ (fn _ => let val  (COMMAND as COMMAND1) = COMMAND1
 ()
 in (CommandSeq(COMMAND, NONE))
end)
 in ( LrTable.NT 3, ( result, COMMAND1left, SEMICOLON1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( 
MlyValue.IDENT IDENT1, IDENT1left, _)) :: rest671)) => let val  result
 = MlyValue.COMMAND (fn _ => let val  (IDENT as IDENT1) = IDENT1 ()
 val  (EXP as EXP1) = EXP1 ()
 in (AssignmentCmd(IDENT, EXP))
end)
 in ( LrTable.NT 18, ( result, IDENT1left, EXP1right), rest671)
end
|  ( 27, ( ( _, ( _, _, FI1right)) :: _ :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: (
 _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 val  COMMANDSEQ2 = COMMANDSEQ2 ()
 in (IfCmd(EXP, SOME COMMANDSEQ1, SOME COMMANDSEQ2))
end)
 in ( LrTable.NT 18, ( result, IF1left, FI1right), rest671)
end
|  ( 28, ( ( _, ( _, _, FI1right)) :: _ :: _ :: _ :: _ :: ( _, ( 
MlyValue.COMMANDSEQ COMMANDSEQ1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 in (IfCmd(EXP, SOME COMMANDSEQ1, NONE))
end)
 in ( LrTable.NT 18, ( result, IF1left, FI1right), rest671)
end
|  ( 29, ( ( _, ( _, _, FI1right)) :: _ :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: _ :: _ :: _ :: _ :: ( _, ( MlyValue.EXP 
EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result
 = MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  COMMANDSEQ1 = COMMANDSEQ1 ()
 in (IfCmd(EXP, NONE, SOME COMMANDSEQ1))
end)
 in ( LrTable.NT 18, ( result, IF1left, FI1right), rest671)
end
|  ( 30, ( ( _, ( _, _, FI1right)) :: _ :: _ :: _ :: _ :: _ :: _ :: (
 _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671))
 => let val  result = MlyValue.COMMAND (fn _ => let val  (EXP as EXP1)
 = EXP1 ()
 in (IfCmd(EXP, NONE, NONE))
end)
 in ( LrTable.NT 18, ( result, IF1left, FI1right), rest671)
end
|  ( 31, ( ( _, ( _, _, OD1right)) :: _ :: ( _, ( MlyValue.COMMANDSEQ 
COMMANDSEQ1, _, _)) :: _ :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: (
 _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (COMMANDSEQ as COMMANDSEQ1) = COMMANDSEQ1 ()
 in (WhileCmd(EXP, SOME COMMANDSEQ))
end)
 in ( LrTable.NT 18, ( result, WHILE1left, OD1right), rest671)
end
|  ( 32, ( ( _, ( _, _, OD1right)) :: _ :: _ :: _ :: ( _, ( 
MlyValue.EXP EXP1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = 
EXP1 ()
 in (WhileCmd(EXP, NONE))
end)
 in ( LrTable.NT 18, ( result, WHILE1left, OD1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.IDENT IDENT1, _, IDENT1right)) :: ( _, ( _,
 CALL1left, _)) :: rest671)) => let val  result = MlyValue.COMMAND (fn
 _ => let val  (IDENT as IDENT1) = IDENT1 ()
 in (CallCmd(IDENT))
end)
 in ( LrTable.NT 18, ( result, CALL1left, IDENT1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IDENT IDENT1
, _, _)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) => let val  
result = MlyValue.COMMAND (fn _ => let val  (IDENT as IDENT1) = IDENT1
 ()
 in (ReadCmd(IDENT))
end)
 in ( LrTable.NT 18, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result
 = MlyValue.COMMAND (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (PrintCmd(EXP))
end)
 in ( LrTable.NT 18, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, ADD, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, SUB, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, MUL, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, DIV, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, MOD, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, RATADD, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, RATSUB, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, RATMUL, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, RATDIV, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, BOOLAND, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, BOOLOR, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, EQ, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, NEQ, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, LT, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, LEQ, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, GT, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, GEQ, EXP2))
end)
 in ( LrTable.NT 16, ( result, EXP1left, EXP2right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 16, ( result, NUM1left, NUM1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.NUM (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (IRnum(INT))
end)
 in ( LrTable.NT 17, ( result, INT1left, INT1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.DEC DEC1, DEC1left, DEC1right)) :: rest671)
) => let val  result = MlyValue.NUM (fn _ => let val  (DEC as DEC1) = 
DEC1 ()
 in (Rnum(DEC))
end)
 in ( LrTable.NT 17, ( result, DEC1left, DEC1right), rest671)
end
|  ( 56, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: _ :: ( _, ( _, FROMDECIMAL1left, _)) :: rest671)) => let val  
result = MlyValue.NUM (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (BinOp(EXP,RATADD, Rnum(Rat.fromDecimal("0.(0)"))))
end)
 in ( LrTable.NT 17, ( result, FROMDECIMAL1left, RPAREN1right), 
rest671)
end
|  ( 57, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _,
 _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( _, 
MAKERAT1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn _
 => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (BinOp(EXP1, RATDIV, EXP2))
end)
 in ( LrTable.NT 17, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 58, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: _ :: ( _, ( _, INTTORAT1left, _)) :: rest671)) => let val  
result = MlyValue.NUM (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (BinOp(EXP,RATADD, Rnum(Rat.fromDecimal("0.(0)"))))
end)
 in ( LrTable.NT 17, ( result, INTTORAT1left, RPAREN1right), rest671)

end
|  ( 59, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.NUM (fn _ => (Bnum(true)))
 in ( LrTable.NT 17, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 60, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.NUM (fn _ => (Bnum(false)))
 in ( LrTable.NT 17, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.IDENT IDENT1, IDENT1left, IDENT1right)) :: 
rest671)) => let val  result = MlyValue.NUM (fn _ => let val  (IDENT
 as IDENT1) = IDENT1 ()
 in (Var(IDENT))
end)
 in ( LrTable.NT 17, ( result, IDENT1left, IDENT1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn _ =>
 let val  (NUM as NUM1) = NUM1 ()
 in (UnaryOp(NEG, NUM))
end)
 in ( LrTable.NT 17, ( result, NEG1left, NUM1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUS1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn _ =>
 let val  (NUM as NUM1) = NUM1 ()
 in (UnaryOp(PLUS, NUM))
end)
 in ( LrTable.NT 17, ( result, PLUS1left, NUM1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
INVERSE1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn _
 => let val  (NUM as NUM1) = NUM1 ()
 in (UnaryOp(INV, NUM))
end)
 in ( LrTable.NT 17, ( result, INVERSE1left, NUM1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
BOOLNEG1left, _)) :: rest671)) => let val  result = MlyValue.NUM (fn _
 => let val  (NUM as NUM1) = NUM1 ()
 in (UnaryOp(BOOLNEG, NUM))
end)
 in ( LrTable.NT 17, ( result, BOOLNEG1left, NUM1right), rest671)
end
|  ( 66, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.NUM (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 17, ( result, LPAREN1left, RPAREN1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : pl0_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RATADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RATMUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLNEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLAND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun INTTORAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWRAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun FROMDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun TODECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun DEC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.DEC (fn () => i),p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
end
end
