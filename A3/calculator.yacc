
val sh = ref 0

%%

%eop EOF SEMI
%pos int
%left SUB PLUS
%left MUL DIV

%term ID of string | INT of BigInt.bigint | PLUS | MUL | SEMI |
        LPAR | RPAR | SUB | DIV | NEG | DOT | SHFR | SHDEC | EXIT | EOF

%nonterm EXP of Rat.rational | START | NUM of Rat.rational

%name Calculator

%noshift EOF
%nodefault
%verbose

%start START

%%

START : EXP (if (!sh) = 0 then print(Rat.showRat(EXP)^"\n") else print(Rat.showDecimal(EXP)^"\n"))
      | SHFR (sh := 0)
      | SHDEC (sh := 1)
      | EXIT (OS.Process.exit(OS.Process.success)) 
      | ID (raise Rat.rat_error)
      | ()

EXP : EXP PLUS EXP (Rat.add(EXP1, EXP2))
    | EXP SUB EXP (Rat.subtract(EXP1, EXP2))
    | EXP MUL EXP (Rat.multiply(EXP1, EXP2))
    | EXP DIV EXP ((fn SOME z => z | NONE => raise Rat.rat_error ) (Rat.divide(EXP1, EXP2)))
    | LPAR EXP RPAR (EXP)
    | PLUS NUM (NUM)
    | NEG NUM (Rat.neg(NUM))
    | NUM (NUM)

NUM : INT ((fn SOME z => z | NONE => raise Rat.rat_error )(Rat.rat(INT)))
    | INT DOT INT LPAR INT RPAR (Rat.fromDecimal(BigInt.toString(INT1) ^ "." ^ BigInt.toString(INT2) ^ "(" ^ BigInt.toString(INT3) ^ ")"))
    | INT DOT LPAR INT RPAR (Rat.fromDecimal(BigInt.toString(INT1) ^ "." ^ "(" ^ BigInt.toString(INT2) ^ ")"))
    | DOT INT LPAR INT RPAR (Rat.fromDecimal("." ^ BigInt.toString(INT1) ^ "(" ^ BigInt.toString(INT2) ^ ")"))
    | DOT LPAR INT RPAR (Rat.fromDecimal("." ^ "(" ^ BigInt.toString(INT) ^ ")"))

