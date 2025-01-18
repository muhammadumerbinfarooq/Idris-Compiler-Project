module Compiler

import Data.Vect
import Data.String
import Data.List
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Applicative
import Control.Arrow

-- Define a data type for tokens
data Token = Ident String
            | Keyword String
            | Symbol Char
            | Literal Int
            | EOF

-- Define a data type for abstract syntax trees (ASTs)
data AST = Var String
          | App AST AST
          | Lam String AST
          | Lit Int

-- Define a function to tokenize the source code
tokenize : String -> List Token
tokenize src = tokenize' (words src) []
  where
    tokenize' : List String -> List Token -> List Token
    tokenize' [] acc = EOF :: acc
    tokenize' (x :: xs) acc =
      case x of
        "let" => tokenize' xs (Keyword "let" :: acc)
        "in" => tokenize' xs (Keyword "in" :: acc)
        "fn" => tokenize' xs (Keyword "fn" :: acc)
        _ => case span isAlpha x of
               (var, "") => tokenize' xs (Ident var :: acc)
               _ => tokenize' xs (Symbol (head x) :: acc)

-- Define a function to parse the tokens into an AST
parse : List Token -> Maybe AST
parse toks = case toks of
               [] => Nothing
               (Ident x) :: xs => Just (Var x)
               (Keyword "fn") :: (Ident x) :: xs =>
                 Just (Lam x (fromMaybe (Lit 0) (parse xs)))
               (Keyword "let") :: (Ident x) :: xs =>
                 Just (Lam x (fromMaybe (Lit 0) (parse xs)))
               (Lit x) :: xs => Just (Lit x)
               _ => Nothing

-- Define a function to generate bytecode from the AST
generateBytecode : AST -> List Int
generateBytecode ast = case ast of
                           Var x => [0x01, length x]
                           App f a => generateBytecode f ++ generateBytecode a ++ [0x02]
                           Lam x b => [0x03, length x] ++ generateBytecode b
                           Lit x => [0x04, x]

-- Define a function to execute the bytecode
executeBytecode : List Int -> Maybe Int
executeBytecode bc = executeBytecode' bc []
  where
    executeBytecode' : List Int -> List Int -> Maybe Int
    executeBytecode' [] acc = Just (sum acc)
    executeBytecode' (0x01 :: x :: xs) acc = executeBytecode' xs (x :: acc)
    executeBytecode' (0x02 :: xs) acc = case acc of
                                              (f :: a :: acc') => executeBytecode' xs ((f + a) :: acc')
                                              _ => Nothing
    executeBytecode' (0x03 :: x :: xs) acc = executeBytecode' xs (x :: acc)
    executeBytecode' (0x04 :: x :: xs) acc = executeBytecode' xs (x :: acc)
    executeBytecode' _ _ = Nothing

-- Define a main function
main : IO ()
main = do
  src <- getLine
  let toks = tokenize src
  let ast = parse toks
  let bc = generateBytecode (fromMaybe (Lit 0) ast)
  let result = executeBytecode bc
  print result
