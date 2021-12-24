
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token LETREC
%token FIRST
%token SECOND

%token BOOL
%token NAT
%token STRING
%token TOP

%token LBRACK
%token RBRACK
%token LPAREN
%token RPAREN
%token DOT
%token COMMA
%token EQ
%token COLON
%token ARROW
%token CONCAT
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
    IDV EQ term EOF
      { Bind ($1, $3) }
  | term EOF
      { Eval $1 }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }   
  | atomicTerm CONCAT atomicTerm
      { TmConcat ($1, $3) }     
  | FIRST atomicTerm
      { TmProj1 $2}
  | SECOND atomicTerm
      { TmProj2 $2 }
  | atomicTerm DOT IDV
      { TmProj ($1, $3) }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmString $1 }
  | LBRACK term COMMA term RBRACK
      { TmPair ($2, $4) }
  | LBRACK fieldList RBRACK
      { TmRecord $2 }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }

fieldList :
      { [] }
  | IDV EQ term
      { [($1, $3)] }
  | IDV EQ term COMMA fieldList
      { ($1, $3) :: $5 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | TOP
      { TyTop }
  | LBRACK ty COMMA ty RBRACK
      { TyPair ($2, $4) }
  | LBRACK typeList RBRACK
      { TyRecord $2 }

typeList :
      { [] }
  | IDV COLON ty
      { [($1, $3)] }
  | IDV COLON ty COMMA typeList
      { ($1, $3) :: $5 }

