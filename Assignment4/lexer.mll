{
open Parser
}

let digit = ['0'-'9']
let smallAlpha = ['a'-'z']
let capAlpha = ['A'-'Z']
let letter = smallAlpha | capAlpha
let integ = ('0' | (['1'-'9'] digit*))
let num = ('0' | ('-'? ['1'-'9'] digit*))  
let posfloat = (integ '.' integ) | (integ '.' digit* ['e' 'E'] ['+' '-']? digit+)
let float = ('-'? posfloat) 
let identifier = letter (letter | digit | '_')*
let filename = identifier | ""
let space = [' ' '\t' '\n']+

rule tokenize = parse
    | space+ { tokenize lexbuf }
    | "//" [^ '\n']* { tokenize lexbuf }
    | "/*" ([^ '*'] | ('*' [^ '/']))* "*/" { tokenize lexbuf }

    | "Input" space* "(" space* (filename as file) space* ")" { INPUT file }
    | "Print" space* "(" space* (filename as file) space* ")" { PRINT file }

    | "vector" { TYPE_VECTOR }
    | "matrix" { TYPE_MATRIX }
    | "int" { TYPE_INT } | "bool" { TYPE_BOOL } | "float" { TYPE_FLOAT }

    | ":=" { ASSIGN }

    | (num as n) space* ("[" space* (float (space* "," space* float)* as lit) space* "]") { FLOATVECTOR (int_of_string n, "[" ^ lit ^ "]") }
    | (num as n) space* ("[" space* (num (space* "," space* num)* as lit) space* "]") { INTVECTOR (int_of_string n, "[" ^ lit ^ "]") }
    | (num as rows) space* "," space* (num as cols) space* ("[" space* (('[' num (space* "," space* num)* ']') space* ("," space* '[' num (space* "," space* num)* ']')* as lit) space* "]") { INTMATRIX (int_of_string rows, int_of_string cols, "[" ^ lit ^ "]") }
    | (num as rows) space* "," space* (num as cols) space* ("[" space* (('[' float (space* "," space* float)* ']') space* ("," space* '[' float (space* "," space* float)* ']')* as lit) space* "]") { FLOATMATRIX (int_of_string rows, int_of_string cols, "[" ^ lit ^ "]") }

    | "true" { BOOL true } | "false" { BOOL false }

    | "angle" { ANGLE } | "dim" { DIMENSION } | "scale" { SCALE }
    | "addv" { ADDV } | "dot_prod" { DOTPROD } | "len" { LEN }
    | "inv" { INV } | "transpose" { TRANSPOSE } | "det" { DETERMINANT }

    | "+" { PLUS } | "-" { MINUS } | "*" { TIMES } | "/" { DIV } | "abs" { ABS }

    | "(" { LPAREN } | ")" { RPAREN }
    | "[" { LSQ } | "]" { RSQ } 
    | "{" { LBR } | "}" { RBR }

    | "=" { EQ } | "!=" { NEQ } | "<" { LT } | ">" { GT }
    | "<=" { LEQ } | ">=" { GEQ } | "," { COMMA } | ";" { SEMICOLON }

    | "and" { AND } | "or" { OR } | "not" { NOT }

    | "if" { IF } | "then" { THEN } | "else" { ELSE }
    | "for" { FOR } | "to" { TO } | "do" { DO }
    | "while" { WHILE }

    | integ as n { INT (int_of_string n) }
    | posfloat as f { FLOAT (float_of_string f) }

    | identifier as id { VAR id }

    | eof { EOF }
    | _ { Printf.eprintf "Illegal character: %s\n" (Lexing.lexeme lexbuf); exit 1 }