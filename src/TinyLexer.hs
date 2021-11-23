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
addOp = symbol "add"

subtractOp :: Parser String
subtractOp = symbol "subtract"

multiplyOp :: Parser String
multiplyOp = symbol "multiply"

divideOp :: Parser String
divideOp = symbol "divide"

remainderOp :: Parser String
remainderOp = symbol "remainder"


mathOp :: Parser String
mathOp = do addOp
          <|> subtractOp
          <|> multiplyOp
          <|> divideOp
          <|> remainderOp

integerConst :: Parser ParseTree
integerConst = do num <- integer
                  return (ValueNode (IntegerType num))

equalsKeyword :: Parser String
equalsKeyword = symbol "equals"

callKeyword :: Parser String
callKeyword = symbol "call"

inKeyword :: Parser String
inKeyword = symbol "in"

lambdaKeyword :: Parser String
lambdaKeyword = symbol "lambda"

letKeyword :: Parser String
letKeyword = symbol "let"


pairKeyword :: Parser String
pairKeyword = symbol "pair"

firstKeyword :: Parser String
firstKeyword = symbol "first"

secondKeyword :: Parser String
secondKeyword = symbol "second"

beKeyword :: Parser String
beKeyword = symbol "be"

ifKeyword :: Parser String
ifKeyword = symbol "if"

thenKeyword :: Parser String
thenKeyword = symbol "then"

elseKeyword :: Parser String
elseKeyword = symbol "else"

withKeyword :: Parser String
withKeyword = symbol "with"





