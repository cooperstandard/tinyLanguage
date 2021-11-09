module TinyDefinitions where

  -- TODO: Add Nodes for Addition, Subtraction, Multiplication, Division, Remainder
  data ParseTree = AndNode ParseTree ParseTree |
                   OrNode ParseTree ParseTree  |
                   NotNode ParseTree           |
                   ValueNode ValueType         |
                   IdNode String               |
                   LetNode String ParseTree ParseTree |
                   LambdaNode String ParseTree |
                   CallNode String ParseTree |
                   AdditionNode ParseTree ParseTree |
                   SubtractionNode ParseTree ParseTree |
                   MultiplicationNode ParseTree ParseTree |
                   DivisionNode ParseTree ParseTree |
                   RemainderNode ParseTree ParseTree |
                   EmptyNode
                    deriving (Show)
  
  -- closure structure

  data ClosureStructure = Closure String ParseTree EnvType 
                          deriving (Show)

  -- TODO: Add IntegerType and PairType below
  data ValueType = BoolType Bool | 
                   IntegerType Integer |
                   PairType ValueType ValueType | -- is this right?
                   ClosureType ClosureStructure
                     deriving (Show)
  


  type EnvType = [(String,ValueType)]
