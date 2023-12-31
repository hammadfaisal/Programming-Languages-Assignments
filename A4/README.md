# PL0 Compiler
Implementation of a interpreter for a basic programming language PL0 using lex and yacc in SML. 

# Grammar
# =======
Non Terminals = {Program, Block, DeclarationSeq, CommandSeq, VarDecls, ProcDecls, ProcDef, RatVarDecls, IntVarDecls, BoolVarDecls, Command, Exp, Term, Factor, ID, IDlist, COmmands}

Terminals = {0,1,2,3,4,5,6,7,8,9,"+","~","/",".","(",")","-","*","a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","_", ";", ",", "=", "<", ">", "<=", ">=", "<>", "(", ")", "if", "then", "else", "while", "do",  "call", "read", "print", "tt", "ff", "||" , "&&", "!", "integer, "boolean", "rational", "procedure", "fi", "od", ".+.", "./.", ".*.", ".-.", "%", }

Start = Program

Productions :-

Program -> Block

Block -> DeclarationSeq CommandSeq

DeclarationSeq -> VarDecls ProcDecls

VarDecls -> RatVarDecls IntVarDecls BoolVarDecls | RatVarDecls IntVarDecls | RatVarDecls BoolVarDecls | IntVarDecls BoolVarDecls | RatVarDecls | IntVarDecls | BoolVarDecls | ε

ProcDecls -> ProcDef ProcDecls | ε

ProcDef -> procedure ID Block ";"

RatVarDecls -> rational ID IDlist ";"

IntVarDecls -> integer ID IDlist ";"

BoolVarDecls -> boolean ID IDlist ";"

IDlist -> "," ID IDlist | ε

CommandSeq -> "{" Commands "}"

Commands -> Command ";" Commands | ε

Command -> ID ":=" Exp | if Exp then Command else Command fi | while Exp do Command od | call ID | read ID | print Exp

Exp -> Term | Exp "+" Term | Exp "-" Term | Exp "./." Term | Exp ".+." Term | Exp ".-." Term | Exp "&&" Term | Exp "||" Term |  Exp "<" Term | Exp ">" Term | Exp "<=" Term | Exp ">=" Term | Exp "<>" Term | Exp "=" Term

Term -> Factor | Term "*" Factor | Term "/" Factor | Term ".*." Factor | Term "./." Factor | Term "%" Factor

Factor -> ID | "(" Exp ")" | "~" Factor | "-" Factor | "!" Factor | "tt" | "ff" | "make_rat (" Exp "," Exp ")" | "fromDecimal (" Exp ")" | "inverse" Factor | Num

Num -> dPlus | dStar "." dStar "(" dPlus ")" | Var

Var -> Alphanum AlphanumStar dStar AlphanumStar

AlphanumStar -> Alphanum AlphanumStar | ε 

Alphanum -> "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "_"

dPlus -> Digit dStar

dStar -> Digit dStar | ε

Digit -> 0 | Pos

Pos -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9  


