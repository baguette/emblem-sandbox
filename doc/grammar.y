%token EOF

/* An LL(1) grammar for Emblem */

/* Keywords */
%token and andalso as case datatype do else end exception for fn fun handle
%token if in infix infixr let local nonfix of op open orelse raise rec then try
%token type val with withtype while colon underscore pipe fatarrow arrow pound
%token repeat until
%token functor include sharing sig signature struct structure where

/* symbolic */
%token lparen rparen lcurly rcurly lsquare rsquare comma dot semicolon
%token dotdotdot dotdot

/* traits */
%token trait impl derive

/* constants */
%token int word real string char

/* identifiers */
%token identifier tyvar

/* identifiers but also special sometimes */
%token equals times

%%

/* Programs */

Program : Topdec Progsuffix
        ;

Progsuffix : semicolon Program
           | Program
           ;

Topdec : structure Strbind
       | signature Sigbind
       | functor Funbind
       | trait Trbind
       | impl Impbind
       | Dec
       | Exp
       ;


/* Core language */

Decone : val Valbind
       | fun Fvalbind
       | type Typbind
       | datatype Datbind
       | derive Tyvarseq Longvid Traitseq for datatype Datbind
       | exception Exbind
       | local Dec in Dec end
       | open identifier Identifierseq
       | infix Infixsuffix
       | infixr Infixsuffix
       | nonfix Identifierseq
       ;

Dec : Decone Decsequence
    |
    ;

Decsequence : Optionalsemi Decone Decsequence
            |
            ;

Traitseq : comma Tyvarseq Longvid Traitseq
         |
         ;

Optionalsemi : semicolon
             |
             ;

Identifierseq : identifier Identifierseq
              |
              ;

Infixsuffix : int Identifierseq
            | Identifierseq
            ;

Valbind : Pat equals Exp Andvalbind
        | rec Valbind
        ;

Andvalbind : and Valbind
           |
           ;

Fvalbind : Optionalop identifier Funargs Optionalty equals Exp Andfvalbind
         ;

Optionalop : op
           |
           ;

Funargs : Atpat Atpatseq
        ;

Atpatseq : Atpat Atpatseq
         |
         ;

Optionalty : colon Ty
           |
           ;

Andfvalbind : and Fvalbind
            |
            ;

Typbind : Tyvarseq identifier equals Ty Andtypbind
        ;

Andtypbind : and Typbind
           |
           ;

Datbind : Tyvarseq identifier equals Conbind Anddatbind
        ;

Anddatbind : and Datbind
           |
           ;

Conbind : Optionalop identifier Optionalofty Conbindseq
        ;

Optionalofty : of Ty
             |
             ;

Conbindseq : pipe Conbind
           |
           ;

Exbind : Optionalop identifier Optionalofty Andexbind
       ;

Andexbind : and Exbind
          |
          ;

Tyvarseq : tyvar
         | lparen tyvar Tyvarseqcommas rparen
         |
         ;

Tyvarseqcommas : comma tyvar Tyvarseqcommas
               |
               ;

Atpatfactor : underscore
            | int
            | char
            | string
            | word
            | real
            | lcurly Optionalpatrow rcurly
            | lparen Patseq rparen
            | lsquare Patseq rparen
            ;

Atpat : Atpatfactor
      | Optionalop Longvid
      ;

Longvid : identifier Longviddots
        ;

Optionalpatrow : Patrow
               |
               ;

Longviddots : dot identifier Longviddots
            |
            ;

Patseq : Pat Patseqcommas
       ;

Patseqcommas : comma Pat Patseqcommas
             |
             ;

Patrow : dotdotdot
       | identifier Patrowfactor
       ;

Patrowfactor : equals Pat Patrowcommas
             | Optionalty Optionalaspat Patrowcommas
             ;

Patrowcommas : comma Patrow Patrowcommas
             |
             ;

Optionalaspat : as Pat
              |
              ;

Pat : Atpatfactor Pattail
    | Optionalop Longvid Patfactor Pattail
    ;

Patfactor : Atpat
          |
          ;

Pattail :
        | identifier Pat Pattail
        | colon Ty Optionalaspat Pattail
        ;

Ty : tyvar Tytail
   | lcurly Optionaltyrow rcurly Tytail
   | lparen Tyseqcommas rparen Tytail
   ;

Tytail : Longvid Tytail
       | times Ty Tytail
       | arrow Ty Tytail
       | Tycommastail fatarrow Ty Tytail
       |
       ;

Tyseqcommas : Ty Tyseqcommastail
            ;

Tyseqcommastail : comma Ty Tyseqcommastail
                |
                ;

Tyrow : identifier colon Ty Tyrowcommas
      ;

Tyrowcommas : comma identifier colon Ty Tyrowcommas
            |
            ;

Optionaltyrow : Tyrow
              |
              ;

Atexp : int | string | char | real | word
      | Optionalop Longvid
      | lcurly Optionalexprow rcurly
      | pound identifier
      | lparen Tupleorexpseq rparen
      | lsquare Expseq rsquare
      | let Dec in Expseq end
      | do Expseq end
      ;

Tupleorexpseq : Exp Tupleorexpseqcommas
              |
              ;

Tupleorexpseqcommas : Tupleseqcommas
                    | Expseqsemis
                    ;

Tupleseqcommas : comma Exp Tupleseqcommas
               |
               ;

Expseq : Exp Expseqsemis
       ;

Expseqsemis : semicolon Exp Expseqsemis
            |
            ;

Exprow : identifier equals Exp Exprowcommas
       ;

Exprowcommas : comma identifier equals Exp Exprowcommas
             |
             ;

Optionalexprow : Exprow
               |
               ;

Appexp : Atexp Appexpseq
       ;

Appexpseq : Atexp Appexpseq
          |
          ;

Exp : Appexp Exptail
    | try Expseq handle Match end Exptail
    | raise Exp Exptail
    | if Exp then Exp else Exp Exptail
    | while Exp Exptail
    | repeat Exp until Exp Exptail
    | for Pat in Exp Exptail
    | case Exp of Match end Exptail
    | fn Funargs fatarrow Exp Exptail
    ;

Exptail : colon Ty Exptail
        | andalso Exp Exptail
        | orelse Exp Exptail
        |
        ;

Match : Mrule Mruleseq
      ;

Mruleseq : pipe Mrule Mruleseq
         |
         ;

Mrule : Optionalpipe Pat fatarrow Exp
      ;

Optionalpipe : pipe
             |
             ;


/* Modules language */


Strbind : identifier Sigmatch equals Str Andstrbind
        ;

Sigmatch : colon Sig
         |
         ;

Andstrbind : and Strbind
           |
           ;

Str : Longvid Funapp Strtail
    | struct Dec end Strtail
    | let Dec in Str end Strtail
    ;

Strtail : colon Sig Strtail
        |
        ;

Funapp : lparen Strordec rparen
       |
       ;

Strordec : Str
         | Dec
         ;

Sigbind : identifier equals Sig Andsigbind
        ;

Andsigbind : and Sigbind
           |
           ;

Sig : identifier Sigtail
    | sig Spec end Sigtail
    ;

Sigtail : where type Typrefin Sigtail
        |
        ;

Typrefin : Tyvarseq Longvid equals Ty Andtyprefin
         ;

Andtyprefin : and type Typrefin
             |
             ;

Spec : val Valdesc Spectail
     | type Spectyp Spectail
     | datatype Specdatatyp Spectail
     | exception Exndesc Spectail
     | structure Strdesc Spectail
     | include Specinclude Spectail
     ;

Spectyp : Tyvarseq identifier Spectypbind
        ;

Spectypbind : equals Ty Andtypbind
            | Andtypdesc
            ;

Specdatatyp : Datdesc
            | identifier equals datatype Longvid
            ;

Specinclude : sig Spec end Sigtail
            | Longvidseq Sigtail
            ;

Spectail : sharing Spectailsharing Spectail
         | Optionalsemi Spec Spectail
         ;

Spectailsharing : type Longvidequalseq
                | Longvidequalseq
                ;

Longvidseq : Longvid Longvidseq
           |
           ;

Longvidequalseq : Longvid Longvidequalseqseps
                ;

Longvidequalseqseps : equals Longvid Longvidequalseqseps
                    |
                    ;

Valdesc : identifier colon Ty Andvaldesc
        ;

Andvaldesc : and Valdesc
           |
           ;

Typdesc : Tyvarseq identifier Andtypdesc
        ;

Andtypdesc : and Typdesc
           |
           ;

Datdesc : Tyvarseq identifier equals Condesc Anddatdesc
        ;

Anddatdesc : and Datdesc
           |
           ;

Condesc : identifier Optionaloftyp Pipecondesc
        ;

Optionaloftyp : of Ty
              |
              ;

Pipecondesc : pipe Condesc
            |
            ;

Exndesc : identifier Optionaloftyp Andexndesc
        ;

Andexndesc : and Exndesc
           |
           ;

Strdesc : identifier colon Sig Andstrdesc
        ;

Andstrdesc : and Strdesc
           |
           ;

Funbind : identifier lparen Funbindrest
        ;

Funbindrest : identifier colon Sig rparen Sigmatch equals Str Andfunbind
            | Spec Sigmatch equals Str Andfunbind
            ;

Andfunbind : and Funbind
           |
           ;

Trbind : Tyvarseq identifier Optionalsupertrait equals Spec end
       ;

Optionalsupertrait : colon Tyvarseq Longvid Supertraittail
                   |
                   ;

Supertraittail : comma Tyvarseq Longvid Supertraittail
               |
               ;

Impbind : Ty identifier equals Str
        ;

%%
