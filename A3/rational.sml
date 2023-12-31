use "definition.sml";

structure Rat = Rational(BigInt);

CM.make("$/ml-yacc-lib.cm");

use "calculator.yacc.sig";

use "calculator.yacc.sml";

use "calculator.lex.sml";

structure CalculatorLrVals =
  CalculatorLrValsFun(structure Token = LrParser.Token)

structure CalculatorLex =
  CalculatorLexFun(structure Tokens = CalculatorLrVals.Tokens);

structure CalculatorParser =
  Join(structure LrParser = LrParser
       structure ParserData = CalculatorLrVals.ParserData
       structure Lex = CalculatorLex)

fun invoke lexstream =
    let fun print_error (s,i:int,_) =
            TextIO.output(TextIO.stdOut,
                          "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in CalculatorParser.parse(0,lexstream,print_error,())
    end

fun parse () = 
    let val lexer = CalculatorParser.makeLexer
                      (fn _ => (fn SOME x => x | NONE => "" ) (TextIO.inputLine TextIO.stdIn))
        val dummyEOF = CalculatorLrVals.Tokens.EOF(0,0)
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = CalculatorParser.Stream.get lexer
             in 
                if CalculatorParser.sameToken(nextToken,dummyEOF) then ()
                else loop lexer
            end
     in loop lexer
    end

val _ = parse();


