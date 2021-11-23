{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TinyEvaluator where

import TinyParser
import TinyLexer
import Env

import TinyDefinitions

-- compileAndRun
--   Consumes a String which is the program
--   Produces the result of lexing, parsing, and evaluating the program
compileAndRun :: String -> ValueType
compileAndRun program = evaluate (parseString program) emptyEnv

compileAndRunFile :: String -> IO ValueType
compileAndRunFile path = do program <- readFile path
                            return (evaluate (parseString program) emptyEnv)

-- evaluate
--   Consume a Parse Tree
--   Produce the result value of evaluating the given Parse Tree
evaluate :: ParseTree -> EnvType -> ValueType
evaluate tree env = case tree of
                      (ValueNode (BoolType val)) -> BoolType val
                      (ValueNode (ClosureType val)) -> ClosureType val
                      (ValueNode (IntegerType val)) -> IntegerType val
                      (ValueNode (PairType val1 val2)) -> PairType (ValueNode (evaluate val1 env)) (ValueNode (evaluate val2 env)) -- TODO: I think this is right
-- TODO: Add ValueNode which contains PairType
                      (IdNode var) -> applyEnv var env
                      (NotNode val) -> let param = evaluate val env
                                       in
                                            case param of
                                               (BoolType True) -> BoolType False
                                               (BoolType False) -> BoolType True
                      (AndNode valOne valTwo) -> let paramOne = evaluate valOne env
                                                     paramTwo = evaluate valTwo env
                                                   in
                                                     case paramOne of
                                                        (BoolType True) ->
                                                           case paramTwo of
                                                             (BoolType True) -> BoolType True
                                                             (BoolType False) -> BoolType False
                                                        (BoolType False) -> BoolType False
                      (OrNode valOne valTwo) -> let paramOne = evaluate valOne env
                                                    paramTwo = evaluate valTwo env
                                                  in
                                                    case paramOne of
                                                       (BoolType True) -> BoolType True
                                                       (BoolType False) ->
                                                           case paramTwo of
                                                                (BoolType True) -> BoolType True
                                                                (BoolType False) -> BoolType False
                      (AdditionNode valOne valTwo) -> let IntegerType paramOne = evaluate valOne env
                                                          IntegerType paramTwo = evaluate valTwo env
                                                        in
                                                          IntegerType (paramOne + paramTwo)
                      (SubtractionNode valOne valTwo) -> let IntegerType paramOne = evaluate valOne env
                                                             IntegerType paramTwo = evaluate valTwo env
                                                        in
                                                          IntegerType (paramOne - paramTwo)
                      (MultiplicationNode valOne valTwo) -> let IntegerType paramOne = evaluate valOne env
                                                                IntegerType paramTwo = evaluate valTwo env
                                                        in
                                                          IntegerType (paramOne * paramTwo)
                      (DivisionNode valOne valTwo) -> let IntegerType paramOne = evaluate valOne env
                                                          IntegerType paramTwo = evaluate valTwo env
                                                        in
                                                          IntegerType (div paramOne paramTwo)
                      (RemainderNode valOne valTwo) -> let IntegerType paramOne = evaluate valOne env
                                                           IntegerType paramTwo = evaluate valTwo env
                                                         in
                                                           IntegerType (mod paramOne paramTwo)
                      (LetNode id val body) -> let valResult = evaluate val env
                                                           in
                                                             evaluate body
                                                               (extendEnv (id,valResult) env)
                      (LambdaNode id body) -> ClosureType (Closure id body env)
                      (CallNode functionName expr) ->
                           let result = applyEnv functionName env
                              in
                                case result of
                                    ClosureType (Closure paramName functionBody functionEnv) ->
                                         evaluate functionBody (extendEnv (paramName, evaluate expr env) env)
                                              --(extendEnv (functionName, result) (extendEnv (paramName, (evaluate expr env)) functionEnv)))
                                    _ -> error "Illegal function call"
                      (FirstNode tuple) -> let PairType (ValueNode val) _ = evaluate tuple env
                                                                          in
                                                                            val
                      (SecondNode tuple) -> let PairType _ (ValueNode val) = evaluate tuple env
                                                                          in
                                                                            val
                      (IfNode condition val1 val2) -> let BoolType statement = evaluate condition env
                                                                              in
                                                                                (if statement
                                                                                  then evaluate val1 env
                                                                                  else evaluate val2 env) -- ifnode
                      (EqualsNode valOne valTwo) -> let val1 = evaluate valOne env --Only have cases for integers and bools, assumes both are the same type
                                                        in
                                                          case val1 of
                                                               IntegerType num -> let IntegerType val2 = evaluate valTwo env
                                                                                          in 
                                                                                            BoolType (num == val2)
                                                               BoolType bool -> let BoolType val2 = evaluate valTwo env
                                                                                   in
                                                                                     BoolType (bool == val2)
                                                        
                    