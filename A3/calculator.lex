structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              print("line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")

%%
%header (functor CalculatorLexFun(structure Tokens: Calculator_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.INT(BigInt.fromString yytext,!pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.MUL(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
{alpha}+ => (if yytext="showfraction"
                 then Tokens.SHFR(!pos,!pos)
                else if yytext="showdecimal"
                 then Tokens.SHDEC(!pos,!pos)
                else if yytext="exit"
                 then Tokens.EXIT(!pos,!pos)
                else Tokens.ID(yytext,!pos,!pos)
            );
"("      => (Tokens.LPAR(!pos,!pos));
")"      => (Tokens.RPAR(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"~"      => (Tokens.NEG(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"."      => (Tokens.DOT(!pos,!pos));
.      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());
