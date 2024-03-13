%token <int> INT
%token <float> FLOAT
%token PLUS
%token MULT
%token DIV
%token EOF
%token SUB
%token FACT
%token SQUARE
%token PERCENT

%left SUB
%left PLUS 
%left MULT
%left DIV

%start <Ast.expr> prog

%%

prog:
    | e = expr; EOF { e }
    ;

expr:
    | i = INT { Int i }
    | f = FLOAT { Float f }
    | e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
    | e1 = expr; MULT; e2 = expr { Binop (Mult, e1, e2) }
    | e1 = expr; DIV; e2 = expr { Binop (Quo, e1, e2) }
    | e1 = expr; SUB; e2 = expr { Binop (Sub, e1, e2) }
    | e = expr; FACT { SpecialOp (Fact, e) }
    | e = expr; SQUARE { SpecialOp (Sqr, e) }
    | e1 = expr; PERCENT; e2 = expr; { Binop (Modulo, e1, e2) }
    | e = expr; PERCENT { SpecialOp (Percent, e) }
    ;