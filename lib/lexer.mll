{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+

rule read =
    parse
    | white { read lexbuf } 
    | "x" { MULT }
    | "+" { PLUS }
    | "/" { DIV }
    | "-" { SUB }
    | "!" { FACT }
    | "^2" { SQUARE }
    | "%" { PERCENT }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }  
    | eof { EOF }