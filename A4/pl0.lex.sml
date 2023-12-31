functor pl0LexFun(structure Tokens: pl0_TOKENS)=
   struct
    structure UserDeclarations =
      struct
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
type lexarg = string
type arg = lexarg
val pos = ref 0
val eof = fn fileName => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              print("line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\049\051\003\003\049\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\049\048\003\003\003\047\045\003\041\040\039\038\037\036\027\026\
\\020\020\020\020\020\020\020\020\020\020\018\017\014\013\011\003\
\\003\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\003\003\003\003\009\
\\003\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\008\006\005\004\003\
\\003"
),
 (6, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\007\000\000\000\
\\000"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\016\015\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\019\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\\021\021\021\021\021\021\021\021\021\021\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\023\000\000\000\000\000\000\000\
\\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\024\024\024\024\024\024\024\024\024\024\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\000\
\\024\024\024\024\024\024\024\024\024\024\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (27, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\023\000\034\032\000\030\000\028\
\\022\022\022\022\022\022\022\022\022\022\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\033\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (34, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (42, 
"\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\043\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042"
),
 (43, 
"\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\044\043\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\042\
\\042"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (49, 
"\000\000\000\000\000\000\000\000\000\050\000\000\000\050\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\050\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 94)], trans = 0},
{fin = [(N 14),(N 94)], trans = 0},
{fin = [(N 74),(N 94)], trans = 0},
{fin = [(N 94)], trans = 6},
{fin = [(N 48)], trans = 0},
{fin = [(N 72),(N 94)], trans = 0},
{fin = [(N 92),(N 94)], trans = 9},
{fin = [(N 92)], trans = 9},
{fin = [(N 60),(N 94)], trans = 11},
{fin = [(N 63)], trans = 0},
{fin = [(N 50),(N 94)], trans = 0},
{fin = [(N 55),(N 94)], trans = 14},
{fin = [(N 53)], trans = 0},
{fin = [(N 58)], trans = 0},
{fin = [(N 76),(N 94)], trans = 0},
{fin = [(N 94)], trans = 18},
{fin = [(N 66)], trans = 0},
{fin = [(N 89),(N 94)], trans = 20},
{fin = [(N 89)], trans = 20},
{fin = [], trans = 22},
{fin = [], trans = 23},
{fin = [], trans = 24},
{fin = [(N 86)], trans = 0},
{fin = [(N 38),(N 94)], trans = 0},
{fin = [(N 94)], trans = 27},
{fin = [], trans = 28},
{fin = [(N 32)], trans = 0},
{fin = [], trans = 30},
{fin = [(N 24)], trans = 0},
{fin = [], trans = 32},
{fin = [(N 20)], trans = 0},
{fin = [], trans = 34},
{fin = [(N 28)], trans = 0},
{fin = [(N 34),(N 94)], trans = 0},
{fin = [(N 78),(N 94)], trans = 0},
{fin = [(N 16),(N 94)], trans = 0},
{fin = [(N 36),(N 94)], trans = 0},
{fin = [(N 70),(N 94)], trans = 0},
{fin = [(N 68),(N 94)], trans = 41},
{fin = [], trans = 42},
{fin = [], trans = 43},
{fin = [(N 7)], trans = 42},
{fin = [(N 94)], trans = 45},
{fin = [(N 45)], trans = 0},
{fin = [(N 40),(N 94)], trans = 0},
{fin = [(N 42),(N 94)], trans = 0},
{fin = [(N 10),(N 94)], trans = 49},
{fin = [(N 10)], trans = 49},
{fin = [(N 12)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex (yyarg as (fileName : string)) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  10 => (continue())
| 12 => (pos := (!pos) + 1; continue())
| 14 => (Tokens.NEG(!pos,!pos))
| 16 => (Tokens.PLUS(!pos,!pos))
| 20 => (Tokens.RATADD(!pos,!pos))
| 24 => (Tokens.RATSUB(!pos,!pos))
| 28 => (Tokens.RATMUL(!pos,!pos))
| 32 => (Tokens.RATDIV(!pos,!pos))
| 34 => (Tokens.SUB(!pos,!pos))
| 36 => (Tokens.MUL(!pos,!pos))
| 38 => (Tokens.DIV(!pos,!pos))
| 40 => (Tokens.MOD(!pos,!pos))
| 42 => (Tokens.BOOLNEG(!pos,!pos))
| 45 => (Tokens.BOOLAND(!pos,!pos))
| 48 => (Tokens.BOOLOR(!pos,!pos))
| 50 => (Tokens.EQ(!pos,!pos))
| 53 => (Tokens.NEQ(!pos,!pos))
| 55 => (Tokens.LT(!pos,!pos))
| 58 => (Tokens.LEQ(!pos,!pos))
| 60 => (Tokens.GT(!pos,!pos))
| 63 => (Tokens.GEQ(!pos,!pos))
| 66 => (Tokens.ASSIGN(!pos,!pos))
| 68 => (Tokens.LPAREN(!pos,!pos))
| 7 => (continue())
| 70 => (Tokens.RPAREN(!pos,!pos))
| 72 => (Tokens.LBRACE(!pos,!pos))
| 74 => (Tokens.RBRACE(!pos,!pos))
| 76 => (Tokens.SEMICOLON(!pos,!pos))
| 78 => (Tokens.COMMA(!pos,!pos))
| 86 => let val yytext=yymktext() in Tokens.DEC(Rat.fromDecimal yytext,!pos,!pos) end
| 89 => let val yytext=yymktext() in Tokens.INT(BigInt.fromString yytext,!pos,!pos) end
| 92 => let val yytext=yymktext() in if yytext = "rational" then Tokens.RATIONAL(!pos,!pos)
                        else if yytext = "integer" then Tokens.INTEGER(!pos,!pos)
                        else if yytext = "boolean" then Tokens.BOOLEAN(!pos,!pos)
                        else if yytext = "tt" then Tokens.TRUE(!pos,!pos)
                        else if yytext = "ff" then Tokens.FALSE(!pos,!pos)
                        else if yytext = "var" then Tokens.VAR(!pos,!pos)
                        else if yytext = "procedure" then Tokens.PROCEDURE(!pos,!pos)
                        else if yytext = "call" then Tokens.CALL(!pos,!pos)
                        else if yytext = "if" then Tokens.IF(!pos,!pos)
                        else if yytext = "then" then Tokens.THEN(!pos,!pos)
                        else if yytext = "else" then Tokens.ELSE(!pos,!pos)
                        else if yytext = "fi" then Tokens.FI(!pos,!pos)
                        else if yytext = "while" then Tokens.WHILE(!pos,!pos)
                        else if yytext = "do" then Tokens.DO(!pos,!pos)
                        else if yytext = "od" then Tokens.OD(!pos,!pos)
                        else if yytext = "read" then Tokens.READ(!pos,!pos)
                        else if yytext = "print" then Tokens.PRINT(!pos,!pos)
                        else if yytext = "make_rat" then Tokens.MAKERAT(!pos,!pos)
                        else if yytext = "rat" then Tokens.INTTORAT(!pos,!pos)
                        else if yytext = "showRat" then Tokens.SHOWRAT(!pos,!pos)
                        else if yytext = "showDecimal" then Tokens.SHOWDECIMAL(!pos,!pos)
                        else if yytext = "fromDecimal" then Tokens.FROMDECIMAL(!pos,!pos)
                        else if yytext = "toDecimal" then Tokens.TODECIMAL(!pos,!pos)
                        else if yytext = "inverse" then Tokens.INVERSE(!pos,!pos)
                        else Tokens.IDENT(yytext,!pos,!pos) end
| 94 => let val yytext=yymktext() in error ("ignoring bad character "^yytext^" with ASCII value:"^Int.toString(ord(valOf(Char.fromString(yytext)))),!pos,!pos); continue() end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof yyarg
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
