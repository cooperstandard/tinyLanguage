module TinyParser where

--    A Parser for things
-- Is a function from strings
--     To lists of pairs
--   Of things and strings

import TinyDefinitions
import TinyLexer

import MonadicParserLibrary
import Control.Applicative



-- parseString
--   Consume a string containing a Tiny Language program 
--   Produce a structure representing the parse tree of the program 
-- parseString :: String -> [(ParseTree,String)]
parseString :: String -> ParseTree
parseString program = let [(tree,remainingChars)] = parse expressionParser program
                          in
                             case remainingChars of
                                  "" -> tree
                                  _ -> error ("Parse Error: " ++ remainingChars)

-- expressionParser
--   Produce a parser for an expression in the Tiny Language
--     The parser will produce a ParseTree representing the 
--         program 

expressionParser :: Parser ParseTree
                    -- bool literals, bool expressions, math literals, math expressions
expressionParser = 
                   do operatorLevelOne
                    <|>
                    -- pair, tested, works as far as I can test for now
                   do leftParenthesis
                      pairKeyword
                      expr1 <- expressionParser
                      expr2 <- expressionParser
                      rightParenthesis
                      return (ValueNode (PairType expr1 expr2))
                    <|>
                    -- let identifier be expression in expression
                   do leftParenthesis
                      letKeyword
                      i <- ident
                      beKeyword
                      expr <- expressionParser
                      inKeyword
                      -- leftParenthesis
                      body <- expressionParser -- it sees the x and has no idea what to do
                      -- rightParenthesis
                      rightParenthesis
                      return (LetNode i expr body)
                    <|>
                    -- Lambda (I think this is right), untested
                   do leftParenthesis
                      lambdaKeyword
                      paramater <- ident
                      inKeyword
                      body <- expressionParser
                      rightParenthesis
                      return (LambdaNode paramater body)
                    <|>
                    -- call, untested
                   do leftParenthesis
                      callKeyword
                      i <- ident
                      withKeyword
                      body <- expressionParser
                      rightParenthesis
                      return (CallNode i body)
                    <|>
                    --if, untested
                   do leftParenthesis
                      ifKeyword
                      cond <- expressionParser
                      thenKeyword
                      expr1 <- expressionParser
                      elseKeyword
                      expr2 <- expressionParser
                      rightParenthesis
                      return (IfNode cond expr1 expr2)
                    <|>
                   do i <- ident
                      return (IdNode i)
                    <|>
                   do leftParenthesis
                      firstKeyword
                      expr <- expressionParser
                      rightParenthesis
                      return (FirstNode expr)
                    <|>
                   do leftParenthesis
                      secondKeyword
                      expr <- expressionParser
                      rightParenthesis
                      return (SecondNode expr)
                    <|>
                   do leftParenthesis
                      expr1 <- expressionParser
                      equalsKeyword
                      expr2 <- expressionParser
                      rightParenthesis
                      return (EqualsNode expr1 expr2)

-- Lowest level of precedence of Boolean Expressions 
--    This handles the boolean or operation
boolLevelOne :: Parser ParseTree
boolLevelOne = do exprOne <- boolLevelTwo
                  do op <- orOp
                     exprTwo <- expressionParser
                     return (OrNode exprOne exprTwo)
                    <|>
                     return exprOne

-- Second level of precedence of Boolean Expressions
--    This handles the boolean and operation 
boolLevelTwo :: Parser ParseTree
boolLevelTwo = do exprOne <- boolLevelThree
                  do op <- andOp
                     exprTwo <- expressionParser
                     return (AndNode exprOne exprTwo)
                    <|>
                     return exprOne

-- Third level of precedence of Boolean Expressions
--     This handles the boolean not operation and parenthesis
boolLevelThree :: Parser ParseTree
boolLevelThree = do op <- notOp
                    expr <- expressionParser
                    return (NotNode expr)
                  <|>
                 do leftParenthesis
                    expr <- expressionParser
                    rightParenthesis
                    return expr
                  <|>
                 do boolConst
                  <|>
                 do i <- ident
                    return (IdNode i)

-- Lowest level of precedence of Math Expressions 
--    This handles the math add and subtract operations

-- TODO: add parsing for mathLevelOne
mathLevelOne :: Parser ParseTree
mathLevelOne = do exprOne <- mathLevelTwo
                  do op <- addOp
                     exprTwo <- expressionParser
                     return (AdditionNode exprOne exprTwo)
                   <|> do op <- subtractOp
                          exprTwo <- expressionParser
                          return (SubtractionNode exprOne exprTwo)
                   <|>
                     return exprOne


-- Lowest level of precedence of Math Expressions 
--    This handles the math multiplication, division and remainder operations
mathLevelTwo :: Parser ParseTree
mathLevelTwo = do exprOne <- mathLevelThree
                  do op <- multiplyOp
                     exprTwo <- expressionParser
                     return (MultiplicationNode exprOne exprTwo)
                   <|> do op <- divideOp
                          exprTwo <- expressionParser
                          return (DivisionNode exprOne exprTwo) -- TODO Remainder
                   <|>
                     return exprOne
                -- <|> 
                 -- do i <- ident
                    -- return (IdNode i)

-- Third level of precedence of Math Expressions
--     This handles parenthesis
mathLevelThree :: Parser ParseTree
mathLevelThree = do leftParenthesis
                    expr <- expressionParser
                    rightParenthesis
                    return expr
                  <|>
                  do num <- integerConst
                     return num
                  -- <|> 
                  -- do i <- ident
                     -- return (IdNode i)

operatorLevelOne :: Parser ParseTree
operatorLevelOne = do exprOne <- operatorLevelTwo
                      do op <- addOp
                         exprTwo <- expressionParser
                         return (AdditionNode exprOne exprTwo)
                       <|> do op <- subtractOp
                              exprTwo <- expressionParser
                              return (SubtractionNode exprOne exprTwo)
                       <|> do op <- orOp
                              exprTwo <- expressionParser
                              return (OrNode exprOne exprTwo)
                       <|>
                        return exprOne

operatorLevelTwo :: Parser ParseTree
operatorLevelTwo = do exprOne <- operatorLevelThree
                      do op <- multiplyOp
                         exprTwo <- expressionParser
                         return (MultiplicationNode exprOne exprTwo)
                       <|> do op <- divideOp
                              exprTwo <- expressionParser
                              return (DivisionNode exprOne exprTwo) -- TODO Remainder
                       <|> do op <- remainderOp
                              exprTwo <- expressionParser
                              return (RemainderNode exprOne exprTwo)
                       <|> do op <- andOp
                              exprTwo <- expressionParser
                              return (AndNode exprOne exprTwo)
                       <|>
                        return exprOne

operatorLevelThree :: Parser ParseTree
operatorLevelThree = do op <- notOp
                        expr <- expressionParser
                        return (NotNode expr)
                     <|>
                     do leftParenthesis
                        expr <- expressionParser
                        rightParenthesis
                        return expr
                     <|>
                     do boolConst
                     <|>
                     do integerConst
                     <|>
                     do i <- ident
                        return (IdNode i)

                  