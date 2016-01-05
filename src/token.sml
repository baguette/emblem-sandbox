
structure Token = struct
  datatype typ = 
    EOF
  | And
  | Andalso
  | As
  | Case
  | Datatype
  | Do
  | Else
  | End
  | Exception
  | For
  | Fn
  | Fun
  | Handle
  | If
  | In
  | Infix
  | Infixr
  | Let
  | Local
  | Nonfix
  | Of
  | Op
  | Open
  | Orelse
  | Raise
  | Rec
  | Then
  | Try
  | Type
  | Val
  | With
  | Withtype
  | While
  | Colon
  | Underscore
  | Pipe
  | FatArrow
  | Arrow
  | Pound
  | Identifier of string
  | Tyvar of string
  | Int of string
  | Word of string
  | Real of string
  | String of string
  | Char of char
  | LParen
  | RParen
  | LCurly
  | RCurly
  | LSquare
  | RSquare
  | Comma
  | Dot
  | Semicolon
  | Dotdot
  | Dotdotdot
  | Derive
  | Functor
  | Impl
  | Include
  | Sharing
  | Sig
  | Signature
  | Struct
  | Structure
  | Trait
  | Where

  datatype t = Token of {
    typ: typ,
    line: int,
    start_col: int,
    end_col: int
  }

  fun reserved "and"       = And
    | reserved "&&"        = Andalso
    | reserved "as"        = As
    | reserved "case"      = Case
    | reserved "datatype"  = Datatype
    | reserved "do"        = Do
    | reserved "else"      = Else
    | reserved "end"       = End
    | reserved "exception" = Exception
    | reserved "for"       = For
    | reserved "fn"        = Fn
    | reserved "fun"       = Fun
    | reserved "handle"    = Handle
    | reserved "if"        = If
    | reserved "in"        = In
    | reserved "infix"     = Infix
    | reserved "infixr"    = Infixr
    | reserved "let"       = Let
    | reserved "local"     = Local
    | reserved "nonfix"    = Nonfix
    | reserved "of"        = Of
    | reserved "op"        = Op
    | reserved "open"      = Open
    | reserved "||"        = Orelse
    | reserved "raise"     = Raise
    | reserved "rec"       = Rec
    | reserved "then"      = Then
    | reserved "try"       = Try
    | reserved "type"      = Type
    | reserved "val"       = Val
    | reserved "with"      = With
    | reserved "withtype"  = Withtype
    | reserved "while"     = While
    | reserved "."         = Dot
    | reserved ":"         = Colon
    | reserved "_"         = Underscore
    | reserved "|"         = Pipe
    | reserved "=>"        = FatArrow
    | reserved "->"        = Arrow
    | reserved "#"         = Pound
    | reserved "..."       = Dotdotdot
    | reserved ".."        = Dotdot
    | reserved "derive"    = Derive
    | reserved "functor"   = Functor
    | reserved "impl"      = Impl
    | reserved "include"   = Include
    | reserved "sharing"   = Sharing
    | reserved "sig"       = Sig
    | reserved "signature" = Signature
    | reserved "struct"    = Struct
    | reserved "structure" = Structure
    | reserved "trait"     = Trait
    | reserved "where"     = Where
    | reserved id          =
        if String.isPrefix "'" id then
          Tyvar(id)
        else
          Identifier(id)

  fun iseof (Token {typ, ...}) =
    case typ of
      EOF => true
    | _ => false

  fun toString (Token {typ, ...}) =
    case typ of
      EOF => "EOF"
    | And => "and"
    | Andalso => "&&"
    | As => "as"
    | Case => "case"
    | Datatype => "datatype"
    | Do => "do"
    | Else => "else"
    | End => "end"
    | Exception => "exception"
    | For => "for"
    | Fn => "fn"
    | Fun => "fun"
    | Handle => "handle"
    | If => "if"
    | In => "in"
    | Infix => "infix"
    | Infixr => "infixr"
    | Let => "let"
    | Local => "local"
    | Nonfix => "nonfix"
    | Of => "of"
    | Op => "op"
    | Open => "open"
    | Orelse => "||"
    | Raise => "raise"
    | Rec => "rec"
    | Then => "then"
    | Try => "try"
    | Type => "type"
    | Val => "val"
    | With => "with"
    | Withtype => "withtype"
    | While => "while"
    | Colon => ":"
    | Underscore => "_"
    | Pipe => "|"
    | FatArrow => "=>"
    | Arrow => "->"
    | Pound => "#"
    | Identifier(s) => s
    | Tyvar(s) => s
    | Int(s) => s
    | Word(s) => s
    | Real(s) => s
    | String(s) => s
    | Char(c) => str(c)
    | LParen => "("
    | RParen => ")"
    | LCurly => "{"
    | RCurly => "}"
    | LSquare => "["
    | RSquare => "]"
    | Comma => ","
    | Dot => "."
    | Semicolon => ";"
    | Dotdot => ".."
    | Dotdotdot => "..."
    | Derive => "derive"
    | Functor => "functor"
    | Impl => "impl"
    | Include => "include"
    | Sharing => "sharing"
    | Sig => "sig"
    | Signature => "signature"
    | Struct => "struct"
    | Structure => "structure"
    | Trait => "trait"
    | Where => "where"

end
