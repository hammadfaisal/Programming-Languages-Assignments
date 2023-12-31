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

%%
%header (functor pl0LexFun(structure Tokens: pl0_TOKENS));
%arg (fileName : string);

alpha=[A-Za-z];
digit=[0-9];

space=[\ \t\r]+;
identifier=[_a-zA-Z][_a-zA-Z0-9]*;
ws = [\ \t];
%%          

"(*"(.*\n)*.*"*)"   => (continue());
{space}              => (continue());
\n                  => (pos := (!pos) + 1; continue());
"~"                 => (Tokens.NEG(!pos,!pos));
"+"                 => (Tokens.PLUS(!pos,!pos));
".+."               => (Tokens.RATADD(!pos,!pos));
".-."               => (Tokens.RATSUB(!pos,!pos));
".*."               => (Tokens.RATMUL(!pos,!pos));
"./."               => (Tokens.RATDIV(!pos,!pos));
"-"                 => (Tokens.SUB(!pos,!pos));
"*"                 => (Tokens.MUL(!pos,!pos));
"/"                 => (Tokens.DIV(!pos,!pos));
"%"                 => (Tokens.MOD(!pos,!pos));
"!"                 => (Tokens.BOOLNEG(!pos,!pos));
"&&"                => (Tokens.BOOLAND(!pos,!pos));
"||"                => (Tokens.BOOLOR(!pos,!pos));
"="                 => (Tokens.EQ(!pos,!pos));
"<>"                => (Tokens.NEQ(!pos,!pos));
"<"                 => (Tokens.LT(!pos,!pos));
"<="                => (Tokens.LEQ(!pos,!pos));
">"                 => (Tokens.GT(!pos,!pos));
">="                => (Tokens.GEQ(!pos,!pos));
":="                => (Tokens.ASSIGN(!pos,!pos));
"("                 => (Tokens.LPAREN(!pos,!pos));
")"                 => (Tokens.RPAREN(!pos,!pos));
"{"                 => (Tokens.LBRACE(!pos,!pos));
"}"                 => (Tokens.RBRACE(!pos,!pos));
";"                 => (Tokens.SEMICOLON(!pos,!pos));
","                 => (Tokens.COMMA(!pos,!pos));
{digit}* "." {digit}* "(" {digit}+ ")" => (Tokens.DEC(Rat.fromDecimal yytext,!pos,!pos));
{digit}+            => (Tokens.INT(BigInt.fromString yytext,!pos,!pos));
{identifier} => (if yytext = "rational" then Tokens.RATIONAL(!pos,!pos)
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
                        else Tokens.IDENT(yytext,!pos,!pos));
.   => (error ("ignoring bad character "^yytext^" with ASCII value:"^Int.toString(ord(valOf(Char.fromString(yytext)))),!pos,!pos); continue());
