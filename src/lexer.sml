
structure Lexer = struct
  exception SignedWordLiteral of string
  exception UnclosedStringLiteral of string
  exception InvalidControlCharacter of char
  exception InvalidHexEscape of string
  exception InvalidDecEscape of string
  exception InvalidEscapeSequence of char
  exception UnclosedComment
  exception InvalidCharacterLiteral of string
  exception LexicalError of char

  fun isalpha x =
    ((ord x) >= (ord #"a") andalso (ord x) <= (ord #"z"))
    orelse
    ((ord x) >= (ord #"A") andalso (ord x) <= (ord #"Z"))
    orelse x = #"_" orelse x = #"'"

  fun isdigit x =
    ((ord x) >= (ord #"0") andalso (ord x) <= (ord #"9"))

  fun isoneof str x =
    Char.contains str x

  fun isalphanumeric x =
    (isalpha x) orelse (isdigit x) orelse x = #"_" orelse x = #"'"

  fun issymbol x =
    isoneof ".!%&$#+-/:<=>?@\\~'^|*" x

  fun iswhitespace x =
    isoneof " \t\n\r\f" x

  fun ishex x =
    isoneof "0123456789ABCDEFabcdef" x

  fun isnewline x = x = #"\n"

  fun lex_while pred fb =
    let
      fun lex_while_rev xs =
        case FileBuf.getch fb of
          NONE => xs
        | SOME(c) => if pred c then
                       (FileBuf.bump fb; lex_while_rev (c::xs))
                     else xs
    in
      implode (rev (lex_while_rev []))
    end

  fun lex_until pred fb =
    let fun p x = not (pred x) in
      lex_while p fb
    end

  fun lex_hex fb =
    "0x" ^ lex_while ishex fb

  fun lex_number sign fb =
    let val first = ref (FileBuf.getch fb);
        val leader = ref (case !first of
                       NONE => NONE
                     | SOME(#"0") => (FileBuf.bump fb; FileBuf.getch fb)
                     | c => c);
        val word = case !leader of
                     SOME(#"w") => (FileBuf.bump fb;
                                    leader := FileBuf.getch fb;
                                    true)
                   | _ => false;
        val hex = case !leader of
                    SOME(#"x") => (FileBuf.bump fb; true)
                  | _ => false
    in
      if word then
        if sign = "" then
          if hex then
            Token.Word(lex_hex fb)
          else
            Token.Word(lex_while isdigit fb)
        else
          if hex then
            raise SignedWordLiteral(lex_hex fb)
          else
            raise SignedWordLiteral(lex_while isdigit fb)
      else if hex then
        Token.Int(sign ^ lex_hex fb)
      else
        let val significand = lex_while isdigit fb;
            val significand = if significand = "" then "0" else significand
            val fraction = case FileBuf.getch fb of
                             SOME(#".") => (FileBuf.bump fb;
                                            "." ^ lex_while isdigit fb)
                           | _ => "";
            val exponent = case FileBuf.getch fb of
                           (* TODO: Handle negative exponents *)
                             SOME(#"e") => (FileBuf.bump fb;
                                            "e" ^ lex_while isdigit fb)
                           | SOME(#"E") => (FileBuf.bump fb;
                                            "e" ^ lex_while isdigit fb)
                           | _ => ""
        in
          if fraction <> "" orelse exponent <> "" then
            Token.Real(sign ^ significand ^ fraction ^ exponent)
          else
            Token.Int(sign ^ significand)
        end
    end

  fun lex_string fb =
    let
      fun lex_string_rev xs =
       (FileBuf.bump fb;
        case FileBuf.getch fb of
          NONE => raise UnclosedStringLiteral(implode (rev xs))
        | SOME(#"\"") => (FileBuf.bump fb; xs)
        | SOME(#"\\") => (FileBuf.bump fb;
                          case FileBuf.getch fb of
                            NONE =>
                              raise UnclosedStringLiteral(implode (rev xs))
                          | SOME(#"a") => lex_string_rev (#"\a"::xs)
                          | SOME(#"b") => lex_string_rev (#"\b"::xs)
                          | SOME(#"t") => lex_string_rev (#"\t"::xs)
                          | SOME(#"n") => lex_string_rev (#"\n"::xs)
                          | SOME(#"v") => lex_string_rev (#"\v"::xs)
                          | SOME(#"f") => lex_string_rev (#"\f"::xs)
                          | SOME(#"r") => lex_string_rev (#"\r"::xs)
                          | SOME(#"^") => 
                            (FileBuf.bump fb;
                             case FileBuf.getch fb of
                               NONE =>
                                raise UnclosedStringLiteral(implode (rev xs))
                             | SOME(c) =>
                                let val n = ord(c) in
                                  if 64 <= n andalso 95 >= n then
                                    let val ch = chr(n - 64) in
                                      lex_string_rev (ch::xs)
                                    end
                                  else
                                    raise InvalidControlCharacter(c)
                                end)
                          | SOME(#"u") =>
                            (FileBuf.bump fb;
                             let val d = lex_while ishex fb in
                               if size d = 4 then
                                 let val n = case Word.fromString d of
                                               SOME(n) => Word.toInt n
                                             | NONE =>
                                               raise InvalidHexEscape(d);
                                     val c = chr(n)
                                 in
                                   lex_string_rev (c::xs)
                                 end
                               else
                                 raise InvalidHexEscape(d)
                             end)
                           | SOME(#"\"") => lex_string_rev (#"\""::xs)
                           | SOME(#"\\") => lex_string_rev (#"\\"::xs)
                           | SOME(c) =>
                             if isdigit c then
                               let val d = lex_while isdigit fb in
                                 if size d = 3 then
                                   let val n = case Int.fromString d of
                                                 SOME(n) => n
                                               | NONE =>
                                                  raise InvalidDecEscape(d);
                                       val c = chr(n)
                                   in
                                     lex_string_rev (c::xs)
                                   end
                                 else
                                   raise InvalidDecEscape(d)
                               end
                             else if iswhitespace c then
                               let val _ = lex_while iswhitespace fb in
                                 case FileBuf.getch fb of
                                   NONE =>
                                    raise UnclosedStringLiteral(implode
                                                                  (rev xs))
                                 | SOME(#"\\") => lex_string_rev xs
                                 | SOME(c) => raise InvalidEscapeSequence(c)
                               end
                             else
                               raise InvalidEscapeSequence(c))
        | SOME(c) => lex_string_rev (c::xs))
    in
      implode (rev (lex_string_rev []))
    end

  fun lex_block_comment fb =
    let val () = FileBuf.bump fb
        val last = ref (FileBuf.getch fb)
        val loop = ref true
    in
      while !loop do
        (FileBuf.bump fb;
         case !last of
           SOME(#"(") =>
             (case FileBuf.getch fb of
                SOME(#"*") =>
                  (lex_block_comment fb;
                   last := FileBuf.getch fb)
              | SOME(c) =>
                  last := FileBuf.getch fb
              | NONE =>
                  raise UnclosedComment)
         | SOME(#"*") =>
             (case FileBuf.getch fb of
                SOME(#")") =>
                  loop := false
              | SOME(c) =>
                  last := FileBuf.getch fb
              | NONE =>
                  raise UnclosedComment)
         | SOME(c) =>
             last := FileBuf.getch fb
         | NONE =>
             raise UnclosedComment);
       FileBuf.bump fb
    end

  fun lex_line_comment fb =
    lex_until isnewline fb


  fun lex fb =
    let fun return t = (FileBuf.bump fb; t) in
      case FileBuf.getch fb of
        NONE => Token.EOF
      | SOME(c) =>

        if iswhitespace c then
          (lex_while iswhitespace fb; lex fb)

        else if c = #"(" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"*") => (lex_block_comment fb; lex fb)
           | _ => Token.LParen)

        else if c = #"-" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"-") => (lex_line_comment fb; lex fb)
           | _ => Token.reserved ("-" ^ (lex_while issymbol fb)))

        else if c = #"~" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(ch) => if isdigit ch then
                           lex_number "~" fb
                         else
                           Token.reserved ("~" ^ lex_while issymbol fb)
           | NONE => Token.reserved "~")

        else if c = #"#" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"\"") => let val s = lex_string fb in
                              if size s = 1 then
                                Token.Char(String.sub(s, 0))
                              else
                                raise InvalidCharacterLiteral(s)
                            end
           | SOME(c) => Token.reserved ("#" ^ lex_while issymbol fb)
           | NONE => Token.reserved "#")

        else if isalpha c then
          Token.reserved (lex_while isalphanumeric fb)

        else if issymbol c then
          Token.reserved (lex_while issymbol fb)

        else if isdigit c then
          lex_number "" fb

        else
          case c of
            #")" => return Token.RParen
          | #"[" => return Token.LSquare
          | #"]" => return Token.RSquare
          | #"{" => return Token.LCurly
          | #"}" => return Token.RCurly
          | #"," => return Token.Comma
          | #";" => return Token.Semicolon
          | #"\"" => Token.String(lex_string fb)
          | _ => raise LexicalError(c)
    end
end

