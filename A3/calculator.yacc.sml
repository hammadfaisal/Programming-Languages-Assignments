functor CalculatorLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Calculator_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

val sh = ref 0


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\002\000\012\000\003\000\011\000\005\000\047\000\
\\006\000\010\000\010\000\009\000\011\000\008\000\012\000\007\000\
\\013\000\006\000\014\000\005\000\015\000\047\000\000\000\
\\001\000\002\000\012\000\003\000\011\000\006\000\010\000\010\000\009\000\
\\011\000\008\000\000\000\
\\001\000\002\000\012\000\011\000\008\000\000\000\
\\001\000\002\000\019\000\006\000\018\000\000\000\
\\001\000\002\000\028\000\000\000\
\\001\000\002\000\032\000\006\000\031\000\000\000\
\\001\000\002\000\034\000\000\000\
\\001\000\002\000\035\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\003\000\048\000\004\000\016\000\005\000\048\000\007\000\048\000\
\\008\000\048\000\009\000\014\000\015\000\048\000\000\000\
\\001\000\003\000\049\000\004\000\016\000\005\000\049\000\007\000\049\000\
\\008\000\049\000\009\000\014\000\015\000\049\000\000\000\
\\001\000\003\000\050\000\004\000\050\000\005\000\050\000\007\000\050\000\
\\008\000\050\000\009\000\050\000\015\000\050\000\000\000\
\\001\000\003\000\051\000\004\000\051\000\005\000\051\000\007\000\051\000\
\\008\000\051\000\009\000\051\000\015\000\051\000\000\000\
\\001\000\003\000\052\000\004\000\052\000\005\000\052\000\007\000\052\000\
\\008\000\052\000\009\000\052\000\015\000\052\000\000\000\
\\001\000\003\000\053\000\004\000\053\000\005\000\053\000\007\000\053\000\
\\008\000\053\000\009\000\053\000\015\000\053\000\000\000\
\\001\000\003\000\054\000\004\000\054\000\005\000\054\000\007\000\054\000\
\\008\000\054\000\009\000\054\000\015\000\054\000\000\000\
\\001\000\003\000\055\000\004\000\055\000\005\000\055\000\007\000\055\000\
\\008\000\055\000\009\000\055\000\015\000\055\000\000\000\
\\001\000\003\000\056\000\004\000\056\000\005\000\056\000\007\000\056\000\
\\008\000\056\000\009\000\056\000\011\000\023\000\015\000\056\000\000\000\
\\001\000\003\000\057\000\004\000\057\000\005\000\057\000\007\000\057\000\
\\008\000\057\000\009\000\057\000\015\000\057\000\000\000\
\\001\000\003\000\058\000\004\000\058\000\005\000\058\000\007\000\058\000\
\\008\000\058\000\009\000\058\000\015\000\058\000\000\000\
\\001\000\003\000\059\000\004\000\059\000\005\000\059\000\007\000\059\000\
\\008\000\059\000\009\000\059\000\015\000\059\000\000\000\
\\001\000\003\000\060\000\004\000\060\000\005\000\060\000\007\000\060\000\
\\008\000\060\000\009\000\060\000\015\000\060\000\000\000\
\\001\000\003\000\017\000\004\000\016\000\005\000\042\000\008\000\015\000\
\\009\000\014\000\015\000\042\000\000\000\
\\001\000\003\000\017\000\004\000\016\000\007\000\030\000\008\000\015\000\
\\009\000\014\000\000\000\
\\001\000\005\000\000\000\015\000\000\000\000\000\
\\001\000\005\000\043\000\015\000\043\000\000\000\
\\001\000\005\000\044\000\015\000\044\000\000\000\
\\001\000\005\000\045\000\015\000\045\000\000\000\
\\001\000\005\000\046\000\015\000\046\000\000\000\
\\001\000\006\000\029\000\000\000\
\\001\000\006\000\036\000\000\000\
\\001\000\007\000\033\000\000\000\
\\001\000\007\000\037\000\000\000\
\\001\000\007\000\038\000\000\000\
\\001\000\007\000\040\000\000\000\
\"
val actionRowNumbers =
"\000\000\016\000\022\000\027\000\
\\026\000\025\000\003\000\002\000\
\\001\000\002\000\017\000\028\000\
\\001\000\001\000\001\000\001\000\
\\004\000\029\000\015\000\023\000\
\\014\000\005\000\012\000\010\000\
\\011\000\009\000\031\000\006\000\
\\013\000\007\000\030\000\021\000\
\\032\000\033\000\008\000\020\000\
\\019\000\034\000\018\000\024\000"
val gotoT =
"\
\\001\000\002\000\002\000\039\000\003\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\018\000\000\000\
\\001\000\019\000\003\000\001\000\000\000\
\\003\000\020\000\000\000\
\\000\000\
\\000\000\
\\001\000\022\000\003\000\001\000\000\000\
\\001\000\023\000\003\000\001\000\000\000\
\\001\000\024\000\003\000\001\000\000\000\
\\001\000\025\000\003\000\001\000\000\000\
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
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 40
val numrules = 19
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
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INT of unit ->  (BigInt.bigint) | ID of unit ->  (string)
 | NUM of unit ->  (Rat.rational) | EXP of unit ->  (Rat.rational)
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
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "INT"
  | (T 2) => "PLUS"
  | (T 3) => "MUL"
  | (T 4) => "SEMI"
  | (T 5) => "LPAR"
  | (T 6) => "RPAR"
  | (T 7) => "SUB"
  | (T 8) => "DIV"
  | (T 9) => "NEG"
  | (T 10) => "DOT"
  | (T 11) => "SHFR"
  | (T 12) => "SHDEC"
  | (T 13) => "EXIT"
  | (T 14) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)
) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (EXP as 
EXP1) = EXP1 ()
 in (
if (!sh) = 0 then print(Rat.showRat(EXP)^"\n") else print(Rat.showDecimal(EXP)^"\n")
)
end; ()))
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( _, SHFR1left, SHFR1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (sh := 0))
 in ( LrTable.NT 1, ( result, SHFR1left, SHFR1right), rest671)
end
|  ( 2, ( ( _, ( _, SHDEC1left, SHDEC1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => (sh := 1))
 in ( LrTable.NT 1, ( result, SHDEC1left, SHDEC1right), rest671)
end
|  ( 3, ( ( _, ( _, EXIT1left, EXIT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => (OS.Process.exit(OS.Process.success)
))
 in ( LrTable.NT 1, ( result, EXIT1left, EXIT1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in (raise Rat.rat_error)
end; ()))
 in ( LrTable.NT 1, ( result, ID1left, ID1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rat.add(EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rat.subtract(EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (Rat.multiply(EXP1, EXP2))
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( 
MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in (
(fn SOME z => z | NONE => raise Rat.rat_error ) (Rat.divide(EXP1, EXP2))
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.EXP EXP1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in (EXP)
end)
 in ( LrTable.NT 0, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUS1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (NUM as NUM1) = NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 0, ( result, PLUS1left, NUM1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ =>
 let val  (NUM as NUM1) = NUM1 ()
 in (Rat.neg(NUM))
end)
 in ( LrTable.NT 0, ( result, NEG1left, NUM1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = 
NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.NUM (fn _ => let val  (INT as INT1) = 
INT1 ()
 in ((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(INT)))

end)
 in ( LrTable.NT 2, ( result, INT1left, INT1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT3, _, _
)) :: _ :: ( _, ( MlyValue.INT INT2, _, _)) :: _ :: ( _, ( 
MlyValue.INT INT1, INT1left, _)) :: rest671)) => let val  result = 
MlyValue.NUM (fn _ => let val  INT1 = INT1 ()
 val  INT2 = INT2 ()
 val  INT3 = INT3 ()
 in (
Rat.fromDecimal(BigInt.toString(INT1) ^ "." ^ BigInt.toString(INT2) ^ "(" ^ BigInt.toString(INT3) ^ ")")
)
end)
 in ( LrTable.NT 2, ( result, INT1left, RPAR1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT2, _, _
)) :: _ :: _ :: ( _, ( MlyValue.INT INT1, INT1left, _)) :: rest671))
 => let val  result = MlyValue.NUM (fn _ => let val  INT1 = INT1 ()
 val  INT2 = INT2 ()
 in (
Rat.fromDecimal(BigInt.toString(INT1) ^ "." ^ "(" ^ BigInt.toString(INT2) ^ ")")
)
end)
 in ( LrTable.NT 2, ( result, INT1left, RPAR1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT2, _, _
)) :: _ :: ( _, ( MlyValue.INT INT1, _, _)) :: ( _, ( _, DOT1left, _))
 :: rest671)) => let val  result = MlyValue.NUM (fn _ => let val  INT1
 = INT1 ()
 val  INT2 = INT2 ()
 in (
Rat.fromDecimal("." ^ BigInt.toString(INT1) ^ "(" ^ BigInt.toString(INT2) ^ ")")
)
end)
 in ( LrTable.NT 2, ( result, DOT1left, RPAR1right), rest671)
end
|  ( 18, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT1, _, _
)) :: _ :: ( _, ( _, DOT1left, _)) :: rest671)) => let val  result = 
MlyValue.NUM (fn _ => let val  (INT as INT1) = INT1 ()
 in (Rat.fromDecimal("." ^ "(" ^ BigInt.toString(INT) ^ ")"))
end)
 in ( LrTable.NT 2, ( result, DOT1left, RPAR1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calculator_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun SHFR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun SHDEC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EXIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
end
end
