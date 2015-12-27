/* TODO: Module grammar */
%token EOF

/* Keywords */
%token and andalso as case datatype do else end exception for fn fun handle
%token if in infix infixr let local nonfix of op open orelse raise rec then try
%token type val with withtype while colon underscore pipe fatarrow arrow pound
%token functor include sharing sig signature struct structure where

/* symbolic */
%token lparen rparen lcurly rcurly lsquare rsquare comma dot semicolon
%token dotdotdot

/* constants */
%token int word real string char

/* identifiers */
%token identifier tyvar

%%

Program : Topdec Progsuffix
        ;

Progsuffix : semicolon Program
           | Program
           ;

Topdec : Dec
       | Strdec
       | Sigdec
       | Fundec
       | Exp
       ;

Decone : val Valbind
       | fun Fvalbind
       | type Typbind
       | datatype Datbind
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

Fvalbind : Optionalop identifier Atpat Atpatseq Optionalty equals Exp Fvalbindseq Andfvalbind
         ;

Optionalop : op
           |
           ;

Optionalty : colon Ty
           |
           ;

Fvalbindseq : pipe Fvalbind Fvalbindseq
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

Atpat : underscore
      | int
      | char
      | string
      | word
      | real
      | lcurly Optionalpatrow rcurly
      | lparen Patseq rparen
      | lsquare Patseq rparen
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

Factorpat : Atpat Factorpattail
          | Optionalop Longvid Asorcons Factorpattail
          ;

Factorpattail : identifier Pat
              |
              ;

Asorcons : Optionalty as Pat
         | Atpat
         |
         ;

Pat : Factorpat Optionalty
    ;

/* TODO: make sure this factoring actually works */
Ty : tyvar Tytail
   | lcurly Optionaltyrow rcurly Tytail
   | lparen Tyseqcommas rparen Tytail
   ;

Tytail : Longvid
       | times Ty
       | arrow Ty
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

Infexp : Appexp Infexptail
       ;

/* TODO: Find out if I did this right... */
Infexptail : Infexp identifier Infexptail
           |
           ;

Exp : Infexp Exptail
    | raise Exp Exptail
    | if Exp then Exp else Exp Exptail
    | while Exp Exptail
    | for Pat in Exp Exptail
    | case Exp of Match end
    | fn Patseq2
    ;

Match : Mrule Mruleseq
      ;

Mruleseq : pipe Mrule Mruleseq
         |
         ;

Mrule : Pat fatarrow Exp
      ;

Patseq2 : Pat Patseqspaces
        ;

Patseqspaces : Pat Patseqspaces
             |
             ;

%%
