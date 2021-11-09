module TinyLexer where

import Control.Applicative 
import MonadicParserLibrary 

import TinyDefinitions

-- Build monadic parsers for each of the words in the Tiny
--   Language 

trueConst :: Parser ParseTree 
trueConst = do symbol "true"
               return (ValueNode (BoolType True))

falseConst :: Parser ParseTree 
falseConst = do symbol "false"
                return (ValueNode (BoolType False))

boolConst :: Parser ParseTree 
boolConst = do trueConst 
             <|> falseConst

andOp :: Parser String
andOp = symbol "and"

orOp :: Parser String
orOp = symbol "or"

notOp :: Parser String
notOp = symbol "not"

boolOp :: Parser String
boolOp = do andOp
           <|> orOp
           <|> notOp 

-- TODO: Add parser definitions for leftParenthesis and rightParenthesis
leftParenthesis :: Parser String
leftParenthesis = symbol "("

rightParenthesis :: Parser String
rightParenthesis = symbol ")"

-- TODO: Add parser definitions for addOp, subtractOp, multiplyOp
--           divideOp, and remainderOp
addOp :: Parser String
addOp = symbol "+"

subtractOp :: Parser String
subtractOp = symbol "-"

multiplyOp :: Parser String
multiplyOp = symbol "*"

divideOp :: Parser String
divideOp = symbol "/"

remainderOp :: Parser String
remainderOp = symbol "%"


mathOp :: Parser String
mathOp = do addOp
          <|> subtractOp
          <|> multiplyOp
          <|> divideOp
          <|> remainderOp

integerConst :: Parser ParseTree
integerConst = do num <- integer
                  return (ValueNode (IntegerType num))

equalKeyword :: Parser String
equalKeyword = symbol "equals"

callKeyword :: Parser String
callKeyword = symbol "call"

inKeyword :: Parser String
inKeyword = symbol "in"

lambdaKeyword :: Parser String
lambdaKeyword = symbol "lambda"

letKeyword :: Parser String
letKeyword = symbol "let"




