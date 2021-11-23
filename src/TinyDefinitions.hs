module TinyDefinitions where

  -- TODO: Add Nodes for Addition, Subtraction, Multiplication, Division, Remainder
  data ParseTree = AndNode ParseTree ParseTree |
                   OrNode ParseTree ParseTree  |
                   NotNode ParseTree           |
                   ValueNode ValueType         |
                   FirstNode ParseTree         |
                   SecondNode ParseTree        |
                   IfNode ParseTree ParseTree ParseTree |
                   IdNode String               |
                   LetNode String ParseTree ParseTree |
                   LambdaNode String ParseTree |
                   CallNode String ParseTree |
                   AdditionNode ParseTree ParseTree |
                   SubtractionNode ParseTree ParseTree |
                   MultiplicationNode ParseTree ParseTree |
                   DivisionNode ParseTree ParseTree |
                   RemainderNode ParseTree ParseTree |
                   EqualsNode ParseTree ParseTree | 
                   EmptyNode
                    deriving (Eq, Show)
  
  -- closure structure

  data ClosureStructure = Closure String ParseTree EnvType 
                          deriving (Eq, Show)

  -- TODO: Add IntegerType and PairType below
  data ValueType = BoolType Bool | 
                   IntegerType Integer |
                   PairType ParseTree ParseTree |
                   ClosureType ClosureStructure
                     deriving (Eq, Show)
  


  type EnvType = [(String,ValueType)]
