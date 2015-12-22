use "token.sml";
use "filebuf.sml";
use "lexer.sml";

fun display_tokens fb =
  let val tok = ref (Lexer.lex fb) in
    while not (Token.iseof (!tok)) do
      (PolyML.print (!tok);
       tok := Lexer.lex fb)
  end

fun report_lexical_error fb msg =
  let val FileBuf.Buf {filename, line_num, ...} = fb;
      val line = Int.toString (!line_num)
  in
    print (filename ^ "(" ^ line ^ "): Lexical error: " ^ msg ^ "\n")
  end

fun lex_file filename =
  let val fb = FileBuf.new filename;
      val report = report_lexical_error fb
  in
    display_tokens fb
      handle Lexer.SignedWordLiteral(s) =>
              report ("Word literal cannot be signed: " ^ s)
           | Lexer.UnclosedStringLiteral(s) => 
              report ("Unclosed string literal")
           | Lexer.InvalidControlCharacter(c) =>
              report ("Invalid control character: \\^" ^ str(c))
           | Lexer.InvalidHexEscape(s) =>
              report ("Invalid hexadecimal escape: \\u" ^ s)
           | Lexer.InvalidDecEscape(s) =>
              report ("Invalid decimal escape: \\" ^ s)
           | Lexer.InvalidEscapeSequence(c) =>
              report ("Invalid escape sequence: \\" ^ str(c))
           | Lexer.UnclosedComment =>
              report ("Unclosed block comment")
           | Lexer.InvalidCharacterLiteral(s) =>
              report ("Invalid character literal: #\"" ^ s ^ "\"")
           | Lexer.LexicalError(c) =>
              report ("Unrecognized character: " ^ str(c));
    FileBuf.close fb
  end

fun main () =
  case CommandLine.arguments() of
    [] => raise Fail("Please supply a file name to process")
  | filename::_ => lex_file filename

