
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
  | Dotdotdot
  | Functor
  | Include
  | Sharing
  | Sig
  | Signature
  | Struct
  | Structure
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
    | reserved "functor"   = Functor
    | reserved "include"   = Include
    | reserved "sharing"   = Sharing
    | reserved "sig"       = Sig
    | reserved "signature" = Signature
    | reserved "struct"    = Struct
    | reserved "structure" = Structure
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
end
