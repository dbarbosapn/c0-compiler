module AST where

data AST = Program [Definitions] deriving (Eq, Show)

data Definitions = FuncDef Type String [Parameters] [Statement] deriving (Eq, Show)

data Parameters = DefParam Type String deriving (Eq, Show)

data Statement = Simple Simple
               | IfStatement Expression Statement
               | IfElseStatement Expression Statement Statement
               | WhileStatement Expression Statement
               | ForStatement (Maybe Simple, Maybe Expression, Maybe Simple) Statement
               | ReturnStatement (Maybe Expression)
               | MultipleStatements [Statement]
               deriving (Eq, Show)

data Simple = Expression Expression deriving (Eq, Show)

data Type = TypeInt
          | TypeBool
          | TypeChar
          deriving (Eq, Show)

data Expression = IntValue Int
                | BoolValue Bool
                | BinaryOperation BinaryOperation
                | Id String
                | FunctionCall String [Expression]
                deriving (Eq, Show)

data BinaryOperation = ArithmeticOperation ArithmeticOperation
                     | RelationalOperation RelationalOperation
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
