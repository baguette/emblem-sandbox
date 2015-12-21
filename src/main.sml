use "util.sml";
use "token.sml";
use "filebuf.sml";
use "lexer.sml";

fun display_tokens fb =
  let val tok = ref (Lexer.lex fb) in
    while not (Token.iseof (!tok)) do
      (PolyML.print (!tok);
       tok := Lexer.lex fb)
  end

fun lex_file filename =
  let val fb = FileBuf.new filename in
    display_tokens fb;
    FileBuf.close fb
  end

fun main () =
  case CommandLine.arguments() of
    [] => raise Fail("Please supply a file name to process")
  | filename::_ => lex_file filename

