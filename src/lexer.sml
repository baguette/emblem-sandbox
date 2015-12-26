structure LexBuf = struct
  datatype t = Buf of {
    line: int ref,
    start_col: int ref,
    end_col: int ref,
    filebuf: FileBuf.t
  }
  
  fun new fb =
    Buf {
      line = ref 1,
      start_col = ref 1,
      end_col = ref 1,
      filebuf = fb
    }

  fun get_filebuf lb =
    let val Buf {filebuf, ...} = lb in
      filebuf
    end

  fun set_line lb =
    let val Buf {line, filebuf, ...} = lb
        val FileBuf.Buf {line_num, ...} = filebuf
    in
      line := !line_num
    end
    

  fun set_start_col lb =
    let val Buf {start_col, filebuf, ...} = lb
        val FileBuf.Buf {col_num, ...} = filebuf
    in
      start_col := !col_num
    end

  fun set_end_col lb =
    let val Buf {end_col, filebuf, ...} = lb
        val FileBuf.Buf {col_num, ...} = filebuf
    in
      end_col := !col_num - 1
    end
end

    

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

  fun lex_optional pred fb =
    case FileBuf.getch fb of
      NONE => ""
    | SOME(c) => if pred c then
                   (FileBuf.bump fb;
                    str(c))
                 else
                   ""

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
                             SOME(#"e") => (FileBuf.bump fb;
                                            "e" ^ lex_optional (isoneof "~") fb
                                                ^ lex_while isdigit fb)
                           | SOME(#"E") => (FileBuf.bump fb;
                                            "e" ^ lex_optional (isoneof "~") fb
                                                ^ lex_while isdigit fb)
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


  fun lex lb =
    let val fb = LexBuf.get_filebuf lb
        fun return t = (LexBuf.set_end_col lb; t)
        fun bumpreturn t = (FileBuf.bump fb; LexBuf.set_end_col lb; t)
    in
     LexBuf.set_line lb;
     LexBuf.set_start_col lb;
     case FileBuf.getch fb of
       NONE => return Token.EOF
     | SOME(c) =>
        if iswhitespace c then
          (lex_while iswhitespace fb; lex lb)

        else if c = #"(" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"*") => (lex_block_comment fb; lex lb)
           | _ => return Token.LParen)

        else if c = #"-" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"-") => (lex_line_comment fb; lex lb)
           | _ => return (Token.reserved ("-" ^ (lex_while issymbol fb))))

        else if c = #"~" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(ch) => if isdigit ch then
                           return (lex_number "~" fb)
                         else
                           return (Token.reserved ("~" ^ lex_while issymbol fb))
           | NONE => return (Token.reserved "~"))

        else if c = #"#" then
          (FileBuf.bump fb;
           case FileBuf.getch fb of
             SOME(#"\"") => let val s = lex_string fb in
                              if size s = 1 then
                                return (Token.Char(String.sub(s, 0)))
                              else
                                raise InvalidCharacterLiteral(s)
                            end
           | SOME(c) => return (Token.reserved ("#" ^ lex_while issymbol fb))
           | NONE => return (Token.reserved "#"))

        else if isalpha c then
          return (Token.reserved (lex_while isalphanumeric fb))

        else if issymbol c then
          return (Token.reserved (lex_while issymbol fb))

        else if isdigit c then
          return (lex_number "" fb)

        else
          case c of
            #")" => bumpreturn Token.RParen
          | #"[" => bumpreturn Token.LSquare
          | #"]" => bumpreturn Token.RSquare
          | #"{" => bumpreturn Token.LCurly
          | #"}" => bumpreturn Token.RCurly
          | #"," => bumpreturn Token.Comma
          | #";" => bumpreturn Token.Semicolon
          | #"\"" => return (Token.String(lex_string fb))
          | _ => raise LexicalError(c)
    end
end

