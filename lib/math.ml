open Ast

let rec factorial a = 
  if a <= 1 then 1 else a * factorial(a -1)

let square a = a * a

let percent a = a/.100.

let parse (s:string) : expr = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

let string_of_val (e: expr) : string = 
  match  e with
  | Int i -> string_of_int i
  | Float i -> string_of_float i
  | Binop _ -> failwith "Precondition violated"
  | SpecialOp _ -> failwith "Precondition violoated"

let is_value : expr -> bool = function
  | Int _i -> true
  | Float _i -> true
  | Binop _ -> false
  | SpecialOp _ -> false

let rec step : expr -> expr = function 
  | Int _i -> failwith "Already a value"
  | Float _i -> failwith "Already a value"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> 
      step_bop bop e1 e2
  | SpecialOp (bop, e1) -> special_bop bop e1    
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) when is_value e2 -> Binop (bop, step e1, e2)
  | Binop (_) -> failwith "Precondition violated"

and step_bop bop v1 v2 = match bop, v1, v2 with 
| Add, Int a, Int b -> Int (a + b)
| Add, Float a, Float b -> Float (a +. b)
| Mult, Int a, Int b -> Int (a * b)
| Mult, Float a, Float b -> Float (a *. b)
| Quo, Int a, Int b -> if b <> 0 then Float (float_of_int a /. float_of_int b) else failwith "Division by zero"
| Sub, Int a, Int b -> Int (a - b)
| Sub, Float a, Float b -> Float (a -. b)
| Modulo, Int a, Int b -> Int (a mod b)
| _ -> failwith "Precondition violated"
and special_bop bop v1 = match bop, v1 with
| Fact, Int a -> Int (factorial a)
| Sqr, Int a -> Int (square a)
| Percent, Int a -> Float (percent (float_of_int a))
| _ -> failwith "Precondition violated"

let rec eval (e: expr) : expr = 
  if is_value e then e else 
    e |> step |> eval

let interp (s: string) : string = 
  s |> parse |> eval |> string_of_val