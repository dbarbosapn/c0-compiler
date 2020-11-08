module AST where

data AST = Program [Definitions] deriving (Eq, Show)

data Definitions = FuncDef Type String [Parameters] [Statement] deriving (Eq, Show)

data Parameters = DefParam Type String deriving (Eq, Show)

--data VarDefinition = VarDef Type [Variable] deriving (Eq, Show)

data Variable = Var String
              | Ass String Expression
              deriving (Eq, Show)

data Statement = SimpleStatement Expression
               | VarDefinition Type [Variable]
               | IfStatement IfStmt
               | IfElseStatement IfStmt ElseStmt
               | WhileStatement Expression [Statement]
               | ForStatement (Maybe Expression, Maybe Expression, Maybe Expression) Statement
               | ReturnStatement (Maybe Expression)
               | MultipleStatements [Statement]
               deriving (Eq, Show)

data IfStmt = If Expression [Statement] deriving (Eq, Show)

data ElseStmt = Else [Statement] deriving (Eq, Show)

data Type = TypeInt
          | TypeBool
          | TypeChar
          deriving (Eq, Show)

data Expression = IntValue Int
                | BoolValue Bool
                | BinaryOperation BinaryOperation
                | Id String
                | FunctionCall String [Expression]
                | VarDecl [Parameters]
                deriving (Eq, Show)

data BinaryOperation = ArithmeticOperation ArithmeticOperation
                     | RelationalOperation RelationalOperation
                     | AssignOperationion AssignOperation
                     deriving (Eq, Show)

data ArithmeticOperation = Add Expression Expression
                         | Subtract Expression Expression
                         | Divide Expression Expression
                         | Multiply Expression Expression
                         | Modulo Expression Expression
                         deriving (Eq, Show)

data RelationalOperation = Equals Expression Expression
                         | IsLessOrEqual Expression Expression
                         | IsMoreOrEqual Expression Expression
                         | IsNotEqual Expression Expression
                         | IsLess Expression Expression
                         | IsMore Expression Expression
                         deriving (Eq, Show)

data AssignOperation = Assign String Expression
                     deriving (Eq, Show)
