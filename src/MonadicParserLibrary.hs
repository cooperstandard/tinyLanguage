{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- Monadic Parsing
--   From Programming in Haskell by Graham Hutton

--    A Parser for things
-- Is a function from strings
--     To lists of pairs
--   Of things and strings

module MonadicParserLibrary where

   import Control.Applicative
   import Data.Char 

   -- Declare a new type for Parser
   --   This allows the Parser type to be an instance of a class
   --   The constructor P is a "dummy" constructor and serves no real purpose.
   newtype Parser a = P (String -> [(a, String)])

   -- parse
   --   Consume a Parser and a String
   --   Produce the result of apply the parser p to the given string
   parse :: Parser a -> String -> [(a, String)]
   parse (P p) input = p input

   -- item
   --   Consume a Parser and a character
   --   Produce a list with the character as the first value upon success and
   --      the empty list upon failure
   item :: Parser Char
   item = P (\input -> case input of
                            [] -> []
                            (x:xs) -> [(x,xs)])


   -- the Parser type is a Functor
   --   First step toward allowing Parsers to be sequenced
   --   apply a function g to the result value of a Parser if the parser
   --     succeeds or propagate failure otherwise
   --   Examples: 
   --     parse (fmap toUpper item) "abc" ==> [('A',"bc")]
   --     parse (fmap toUpper item) ""    ==> []
   instance Functor Parser where
      -- fmap :: (a -> b) -> Parser a -> Parser b
      fmap g p = P (\inp -> case parse p inp of
                                 [] -> []
                                 [(v,out)] -> [(g v, out)])

   -- the Parser type is Applicative
   --    pure transforms a value into a parser which always succeeds 
   --         with value as the result and does not consume input
   --         Example: parse (pure 1) "abc" ==> [(1,"abc")]
   --    <*> starts with a parse that returns a function
   --        next takes a parser which returns an argument
   --        produces a parser that produces the result of applying
   --        the function to the argument and only succeeds if all 
   --        components succeed
   --        Example: three :: Parser (Char,Char)
   --                 three = pure g <*> item <*> item <*> item
   --                         where g x y z = (x,z)
   --                 parse three "abcdef" ==> [(('a,','c'),"def")]
   --                 parse three "ab" ==> []
   instance Applicative Parser where
     -- pure :: a -> Parser a
     pure v = P (\inp -> [(v,inp)])

     -- <*> :: Parser (a -> b) -> Parser a -> Parser b
     pg <*> px = P (\inp -> case parse pg inp of
                                 [] -> []
                                 [(g,out)] -> parse (fmap g px) out)

   -- the Parser type is a Monad
   --   This will allow do notation
   --     Example: three :: Parser (Char,Char)
   --              three = do x <- item
   --                      item
   --                      z <- item
   --                      return (x,z)
   --     Note that return is another name for pure which builds parsers
   --          that always succeed.
   instance Monad Parser where
     -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
     p >>= f = P (\inp -> case parse p inp of
                               [] -> []
                               [(v,out)] -> parse (f v) out)

   -- the Parser type is an Alternative
   --     Examples: parse empty "abc" ==> []
   --               parse (empty <|> return 'd') "abc" ==> [('d',"abc")]
   instance Alternative Parser where
     -- empty :: Parser a
     empty = P (\inp -> [])

     -- (<|>) :: Parser a -> Parser a -> Parser a
     p <|> q = P (\inp -> case parse p inp of
                               [] -> parse q inp
                               [(v,out)] -> [(v,out)])

   -- Parser which satisfies a predicate
   sat :: (Char -> Bool) -> Parser Char
   sat pred = do x <- item
                 if pred x then return x else empty

   digit :: Parser Char
   digit = sat isDigit

   lower :: Parser Char
   lower = sat isLower

   upper :: Parser Char
   upper = sat isUpper

   letter :: Parser Char
   letter = sat isAlpha

   alphanum :: Parser Char
   alphanum = sat isAlphaNum

   char :: Char -> Parser Char
   char x = sat (==x)

   -- Example: parse (string "abc") "abcdef"  ==> [("abc", "def")]
   string :: String -> Parser String
   string [] = return []
   string (x:xs) = do char x
                      string xs
                      return (x:xs)

   ident :: Parser String
   ident = do x <- lower
              xs <- many alphanum
              return (x:xs)

   nat :: Parser Integer 
   nat = do xs <- some digit
            return (read xs)

   space :: Parser ()
   space = do many (sat isSpace)
              return ()

   int :: Parser Integer
   int = do char '-'
            n <- nat
            return (-n)
          <|> nat

   token :: Parser a -> Parser a
   token p = do space
                v <- p
                space
                return v

   identifier :: Parser String
   identifier = token ident

   natural :: Parser Integer 
   natural = token nat

   integer :: Parser Integer
   integer = token int 

   symbol :: String -> Parser String
   symbol xs = token (string xs)

   -- Example:
   -- Parse a list of natural numbers
   listOfNats :: Parser [Integer]
   listOfNats = do symbol "["
                   n <- natural
                   ns <- many (do symbol "," 
                                  natural)
                   symbol "]"
                   return (n:ns)
   
   expr :: Parser Integer
   expr = do t <- term
             do symbol "+"
                e <- expr
                return (t+e)
              <|> return t

   term :: Parser Integer 
   term = do f <- factor 
             do symbol "*"
                t <- term
                return (f*t)
              <|> return f

   factor :: Parser Integer
   factor = do symbol "("
               e <- expr
               symbol ")"
               return e
             <|> natural

   eval :: String -> Integer
   eval xs = case (parse expr xs) of
                [(n, [])] -> n
                [(_,out)] -> error ("Unused input " ++ out)
                []        -> error "Invalid input"
