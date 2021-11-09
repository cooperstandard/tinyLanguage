
module Env where

  emptyEnv = []

  extendEnv :: (a,b) -> [(a,b)] -> [(a,b)]
  extendEnv (key,val) env = (key,val):env

  applyEnv :: (Eq a) => a -> [(a,b)] -> b
  applyEnv k [] = error "Variable undefined"
  applyEnv k ((key,val):kvs) = if k == key
                                 then val
                                 else (applyEnv k kvs)

  emptyEnv' = \k -> error "Variable undefined"

  extendEnv' (key,val) fenv = \k -> if k == key
                                       then val
                                       else (fenv key)

  applyEnv' k fenv = fenv k
