use "token.sml";
use "filebuf.sml";
use "lexer.sml";

fun display_tokens lb =
  let val tok = ref (Lexer.lex lb) in
    while not (Token.iseof (!tok)) do
      let val LexBuf.Buf {line, start_col, end_col, ...} = lb in
        (print ((Int.toString (!line))      ^ "," ^
                (Int.toString (!start_col)) ^ "-" ^
                (Int.toString (!end_col))   ^ ": ");
         PolyML.print (!tok);
         tok := Lexer.lex lb)
      end
  end

fun report_lexical_error lb msg =
  let val LexBuf.Buf {filebuf, line, start_col, end_col, ...} = lb
      val FileBuf.Buf {filename, ...} = filebuf
      val line = Int.toString (!line)
      val start_col = Int.toString (!start_col)
      val end_col = Int.toString(!end_col)
  in
    print (filename ^ "(" ^ line ^ ":" ^ start_col ^ "-" ^ end_col ^
           "): Lexical error: " ^ msg ^ "\n")
  end

fun lex_file filename =
  let val fb = FileBuf.new filename;
      val lb = LexBuf.new fb;
      val report = report_lexical_error lb
  in
    display_tokens lb
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

