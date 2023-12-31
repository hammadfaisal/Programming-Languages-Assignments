use "definition.sml";

val outstream = ref (TextIO.stdOut);

structure Rat = Rational(BigInt);

CM.make("$/ml-yacc-lib.cm");

use "pl0.yacc.sig";

use "pl0.yacc.sml";

use "pl0.lex.sml";

structure pl0LrVals =
  pl0LrValsFun(structure Token = LrParser.Token)

structure pl0Lex =
  pl0LexFun(structure Tokens = pl0LrVals.Tokens);

structure pl0Parser =
  JoinWithArg(structure LrParser = LrParser
       structure ParserData = pl0LrVals.ParserData
       structure Lex = pl0Lex)


fun parse (fileName) = 
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                      ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = pl0Parser.parse
                    (15,
                    (pl0Parser.makeLexer grab fileName),
                    printError,
                    fileName)
     in
        TextIO.closeIn inStream 
    end

fun interpret(inputFile, outputFile) =
    let
        val _ = outstream := TextIO.openOut outputFile;
        val _ = parse inputFile;
     in
        TextIO.flushOut (!outstream)
    end




