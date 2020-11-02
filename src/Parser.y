{
module Parser where
import Lexer
}

%name parse
%tokentype { Token }
%error { parseError }

%token

-- Numeric constants
int         { INT $$ }

-- Bool constants
bool        { BOOL $$ }

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
"then"    { THEN }
"else"    { ELSE }
"return"  { RETURN }

-- Types
"int"       { T_INT }
"char"      { T_CHAR }
"bool"      { T_BOOL }

-- Separators
"{"     { LBRACE }
"}"     { RBRACE }
")"     { RPAREN }
"("     { LPAREN }
";"     { SEMICOLON }
"["     { LBRACKET }
"]"     { RBRACKET }
","     { COMMA }

-- Assignment
"="     { ASSIGN }

-- Ids
id      { ID $$ }

-- Precedence
%left   "(" ")" "[" "]"
%left   "*" "/"
%left   "+" "-"
%left   "<" "<=" ">=" ">"
%left   "==" "!="
%right  "="


%%

-- Grammar

Program : GDecl                                             {()}
        | GDef                                              {()}
        | GDecl Program                                     {()}
        | GDef Program                                      {()}

GDecl : Type id "(" FuncParams ")" ";"                      {()}

GDef : Type id "(" FuncParams ")" "{" MultStmt "}"          {()}

Stmt : Simple ";"                                           {()}
     | "if" "(" Exp ")" Stmt                                {()}
     | "if" "(" Exp ")" Stmt "else" Stmt                    {()}
     | "while" "(" Exp ")" Stmt                             {()}
     | "for" "(" ForInner ")" Stmt                          {()}
     | "return" ";"                                         {()}
     | "return" Exp ";"                                     {()}
     | "{" MultStmt "}"                                     {()}

MultStmt : Stmt                                             {()}
         | Stmt MultStmt                                    {()}

Simple : Exp                                                {()}

Type : "int"                                                {()}
     | "bool"                                               {()}
     | "char"                                               {()}

Exp : "(" Exp ")"                                           {()}
    | int                                                   {()}
    | bool                                                  {()}
    | Exp BinOp Exp                                         {()}
    | id                                                    {()}
    | id "(" ")"                                            {()}
    | id "(" CallParams ")"                                 {()}
    
ArithmeticOp : "+"                                          {()}
             | "-"                                          {()}
             | "/"                                          {()}
             | "*"                                          {()}
             | "%"                                          {()}

RelationalOp : "=="                                         {()} 
             | "<="                                         {()} 
             | ">="                                         {()} 
             | "!="                                         {()} 
             | "<"                                          {()} 
             | ">"                                          {()}

BinOp : ArithmeticOp                                        {()}
      | RelationalOp                                        {()}

FuncParams : Type id                                        {()}
           | Type id "," FuncParams                         {()}

CallParams : Exp                                            {()}
           | Exp "," CallParams                             {()}

ForInner : ";" Exp ";"                                      {()}
         | Simple ";" Exp ";"                               {()}
         | ";" Exp ";" Simple                               {()}
         | Simple ";" Exp ";" Simple                        {()}


{
parseError :: [Token] -> a
parseError toks = error "parse error"
}