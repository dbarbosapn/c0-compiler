{
module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token

-- Numeric constants
int         { INT $$ }

-- Bool constants
bool        { BOOL $$ }

-- Char constant
char        { CHAR $$ }

-- String constant
str         { STRING $$ }

-- Arithmetic Operators
"+"         { A_OP ADD }
"-"         { A_OP SUB }
"/"         { A_OP DIV }
"*"         { A_OP MULT }
"%"         { A_OP MOD }

-- Relational Operators
"=="        { R_OP EQUAL }
"<="        { R_OP LESS_OR_EQUAL }
">="        { R_OP MORE_OR_EQUAL }
"!="        { R_OP NOT_EQUAL }
"<"         { R_OP LESS }
">"         { R_OP MORE }

-- Reserved Words
"for"     { FOR }
"while"   { WHILE }
"if"      { IF }
"else"    { ELSE }
"return"  { RETURN }

-- Types
"int"       { T_INT }
"char"      { T_CHAR }
"bool"      { T_BOOL }
"void"      { T_VOID }
"string"    { T_STRING }

-- Separators
"{"     { LBRACE }
"}"     { RBRACE }
")"     { RPAREN }
"("     { LPAREN }
";"     { SEMICOLON }
-- "["     { LBRACKET } Not being used, but may later, if so then uncomment
-- "]"     { RBRACKET } Not being used, but may later, if so then uncomment
","     { COMMA }

-- Assignment
"="     { ASSIGN }

-- Ids
id      { ID $$ }

-- Precedence
%right  "="
%left   "==" "!="
%left   "<" "<=" ">=" ">"
%left   "+" "-"
%left   "*" "/" "%"
%left   "(" ")" "[" "]"
%right  "else" -- Precedence is right BECAUSE we want a shift, otherwise a reduce would come into play. See happy tutorial for more info

%%

-- Grammar

Root : Definitions                                          { Program $1 }
     | {- Empty File -}                                     { Program [] }

Definitions : Function                                      { [$1] }
            | Function Definitions                          { $1 : $2 }

Function : GDef                                             { $1 }
         | GDec                                             { $1 }

GDec : Tp id "(" FuncParams ")" ";"                         { FuncDec $1 $2 $4 }

GDef : Tp id "(" FuncParams ")" "{" MultStmt "}"            { FuncDef $1 $2 $4 $7 }
     | Tp id "(" FuncParams ")" "{" "}"                     { FuncDef $1 $2 $4 [] }

Stmt : Simple ";"                                           { Simple $1 }
     | "if" "(" Exp ")" Stmt "else" Stmt                    { IfElseStatement $3 $5 $7 }
     | "if" "(" Exp ")" Stmt                                { IfStatement $3 $5 }
     | "while" "(" Exp ")" Stmt                             { WhileStatement $3 $5 }
     | "for" "(" ForInner ")" Stmt                          { ForStatement $3 $5 }
     | "return" ";"                                         { ReturnStatement Nothing }
     | "return" Exp ";"                                     { ReturnStatement (Just $2) }
     | "{" MultStmt "}"                                     { MultipleStatements $2 }


MultStmt : Stmt                                             { [$1] }
         | Stmt MultStmt                                    { $1 : $2 }
         | {- No Statements -}                              { [] }

Simple : Exp                                                { Expression $1 }
       | AssignOp                                           { AssignOperation $1 }
       | Tp id                                              { VariableDeclaration $1 $2 Nothing }
       | Tp id "=" Exp                                      { VariableDeclaration $1 $2 (Just $4) }

Tp : "int"                                                  { TypeInt }
   | "bool"                                                 { TypeBool }
   | "char"                                                 { TypeChar }
   | "void"                                                 { TypeVoid }
   | "string"                                               { TypeString }

Exp : "(" Exp ")"                                           { $2 }
    | int                                                   { IntValue $1 }
    | bool                                                  { BoolValue $1 }
    | str                                                   { StringValue $1 }
    | char                                                  { CharValue $1 }
    | BinOp                                                 { BinaryOperation $1 }
    | id                                                    { Id $1 }
    | id "(" CallParams ")"                                 { FunctionCall $1 $3 }

ArithmeticOp : Exp "+" Exp                                  { Add $1 $3 }
             | Exp "-" Exp                                  { Subtract $1 $3 }
             | Exp "/" Exp                                  { Divide $1 $3 }
             | Exp "*" Exp                                  { Multiply $1 $3 }
             | Exp "%" Exp                                  { Modulo $1 $3 }

RelationalOp : Exp "==" Exp                                 { Equals $1 $3 }
             | Exp "<=" Exp                                 { IsLessOrEqual $1 $3 }
             | Exp ">=" Exp                                 { IsMoreOrEqual $1 $3 }
             | Exp "!=" Exp                                 { IsNotEqual $1 $3 }
             | Exp "<" Exp                                  { IsLess $1 $3 }
             | Exp ">" Exp                                  { IsMore $1 $3 }

AssignOp : id "=" Exp                                       { Assign $1 $3 }

BinOp : ArithmeticOp                                        { ArithmeticOperation $1 }
      | RelationalOp                                        { RelationalOperation $1 }

FuncParams : Tp id                                          { [DefParam $1 $2] }
           | Tp id "," FuncParams                           { (DefParam $1 $2) : $4 }
           | {- No Params -}                                { [] }

CallParams : Exp                                            { [$1] }
           | Exp "," CallParams                             { $1 : $3 }
           | {- No Params -}                                { [] }

ForInner : ";" Exp ";"                                      { (Nothing, $2, Nothing) }
         | Simple ";" Exp ";"                               { (Just $1, $3, Nothing) }
         | ";" Exp ";" Simple                               { (Nothing, $2, Just $4) }
         | Simple ";" Exp ";" Simple                        { (Just $1, $3, Just $5) }


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}
