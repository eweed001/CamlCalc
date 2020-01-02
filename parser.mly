%{
open Ast
open List
%}

%token <bool> BOOL
%token <float> FLT
%token <string> ID STRING
%token ADD MULT LOG MOD EXP SUBT DIV EQUALS
%token LPAREN RPAREN ARROW SEMI RBRACK LBRACK
%token TRUE FALSE
%token FUN IN LET IF THEN ELSE
 /* RLET MLET SLET */
%token EOF
%token SIN
%token COS
%token TAN
%token SEC
%token CSC
%token COT


%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc THEN
%nonassoc ELSE
%left EQUALS
%left ADD SUBT
%left MULT DIV MOD LOG EXP


%start <Ast.phrase> prog

%%

prog:
|e = seq_expr; EOF { Expr e }
|d = defn; EOF { Defn d }
;

seq_expr:
  | e = expr; %prec below_SEMI
        { e }
  | e = expr; SEMI
        { e }
  | e = expr; SEMI ; s = seq_expr ; 
        {Sequence (e,s) }
  | e = defn; SEMI ; s = seq_expr ; 
        {DSequence (e,s) }

expr:
|e = s_expr { e }
|e = s_expr; es = nonempty_list(s_expr) { FunApp (e, es) }
|e1 = expr; ADD; e2 = expr { Binop (Func "+", e1, e2) }
|e1 = expr; SUBT; e2 = expr { Binop (Func "-", e1, e2) }
|e1 = expr; MULT; e2 = expr { Binop (Func "*", e1, e2) }
|e1 = expr; DIV; e2 = expr { Binop (Func "/", e1, e2) }
|e1 = expr; MOD; e2 = expr { Binop (Func "%", e1, e2)}
|e1 = expr; LOG; e2 = expr { Binop (Func "log", e1, e2)}
|e1 = expr; EXP; e2 = expr { Binop (Func "^", e1, e2)}
|e1 = expr; EQUALS; e2 = expr { Binop (Func "==", e1, e2)}
|IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
|IF; e1 = seq_expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
|LET; x = ID; EQUALS; e1 = expr; IN; e2 = seq_expr { Let (x, e1, e2) }
|FUN; LPAREN; xs = nonempty_list(iden); RPAREN; ARROW; e = seq_expr; { Fun (xs, e) }
|SIN; e = expr { Unop (Func_u "sin", e) }
|COS; e = expr { Unop (Func_u "cos", e) }
|TAN; e = expr { Unop (Func_u "tan", e) }
|SEC; e = expr { Unop (Func_u "sec", e) }
|CSC; e = expr { Unop (Func_u "csc", e) }
|COT; e = expr { Unop (Func_u "cot", e) }


s_expr: 
| x = iden { Var x }
| s = FLT { Float s }
| LPAREN; e = seq_expr; RPAREN
        { e }
| s = STRING { String s }
| b = BOOL { Boolean b }
| TRUE { Boolean true }
| FALSE { Boolean false }
| LBRACK; e = nonempty_list(s_expr); RBRACK { Arr (e) }
| LBRACK; RBRACK {Arr ( [] )}


defn:
|LET; x = ID; EQUALS; e1 = expr { DLet (x, e1) }


iden:
  | x = ID
        { x }
