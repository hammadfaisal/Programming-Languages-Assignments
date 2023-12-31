# Rational Calculator
# ==================
This is a calculator that can perform arithmetic operations on rational numbers and rational expressions. The calculator can be toggled between giving outputs as fractions and decimals. The calculator can also be used to evaluate rational expressions.

# Grammar for rational numbers
# ===========================
Non Terminals = { dPlus, dStar, Digit, Pos, Sign, Frac, Rat, Num}
Terminals = {0,1,2,3,4,5,6,7,8,9,"+","~","/",".","(",")"}
Start Symbol = Rat

Productions :-
Rat -> Sign Frac | Sign Num
Sign -> "+" | "~" | ε
Frac -> dPlus "/" dStar Pos dStar
Num -> dPlus | dStar "." dStar "(" dPlus ")"
dPlus -> Digit dPlus
dStar -> Digit dStar | ε
Digit -> 0 | Pos
Pos -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

# Grammar for rational expressions
# =================================

Non Terminals = { dPlus, dStar, Digit, Pos, Sign, Num, Exp, T , F, Var, Alphanum , AlphanumStar}
Terminals = {0,1,2,3,4,5,6,7,8,9,"+","~","/",".","(",")","-","*",
             "a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","_"}
Start Symbol = Exp

Productions :-
Sign -> "+" | "~" | ε
Num -> dPlus | dStar "." dStar "(" dPlus ")" | Var
Var -> Alphanum AlphanumStar dStar AlphanumStar
AlphanumStar -> Alphanum AlphanumStar | ε 
Alphanum -> "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "_"
dPlus -> Digit dPlus
dStar -> Digit dStar | ε
Digit -> 0 | Pos
Pos -> 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
Exp -> Exp "+" T | Exp "-" T | T
T -> T "*" F | T "/" F | F
F -> "(" Exp ")" | Sign Num

# Design Decisions
# ================
1. The variable names in the above grammar can have digits, alphabets and underscores but cannot start with a digit.
2. The calculator starts with the command ```sml rational.sml```.
3. The calculator can be toggled between giving outputs as fractions and decimals using ```showfraction``` and ```showdecimal``` commands respectively.
4. Use "exit" command to exit the calculator.

# Acknowledgements
# ================
1. http://smlnj.org/doc/
