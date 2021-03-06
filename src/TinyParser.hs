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
expressionParser =  -- bool literals, bool expressions, math literals, math expressions
                   do operatorLevelOne
                    <|>
                    -- pair
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
                      body <- expressionParser
                      rightParenthesis
                      return (LetNode i expr body)
                    <|>
                    -- Lambda identifier in body
                   do leftParenthesis
                      lambdaKeyword
                      paramater <- ident
                      inKeyword
                      body <- expressionParser
                      rightParenthesis
                      return (LambdaNode paramater body)
                    <|>
                    -- call identifier with expression
                   do leftParenthesis
                      callKeyword
                      i <- ident
                      withKeyword
                      body <- expressionParser
                      rightParenthesis
                      return (CallNode i body)
                    <|>
                    --if conditional then expression else expression
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
                    -- identifier
                   do i <- ident
                      return (IdNode i)
                    <|>
                    -- first pairNode
                   do leftParenthesis
                      firstKeyword
                      expr <- expressionParser
                      rightParenthesis
                      return (FirstNode expr)
                    <|>
                    -- second pairNode
                   do leftParenthesis
                      secondKeyword
                      expr <- expressionParser
                      rightParenthesis
                      return (SecondNode expr)
                    <|>
                    -- expression equals expression
                   do leftParenthesis
                      expr1 <- expressionParser
                      equalsKeyword
                      expr2 <- expressionParser
                      rightParenthesis
                      return (EqualsNode expr1 expr2)


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
                              return (DivisionNode exprOne exprTwo) --DONE: Remainder
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

                  