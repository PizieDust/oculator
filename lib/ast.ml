type bop = 
  | Add 
  | Mult
  | Quo
  | Sub
  | Fact
  | Sqr
  | Percent
  | Modulo

type expr =
  | Int of int
  | Float of float
  | Binop of bop * expr * expr
  | SpecialOp of bop * expr
