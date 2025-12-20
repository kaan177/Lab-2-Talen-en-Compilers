module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive e a = case (step e a) of
    (Done s _ _) -> print ("final state: " ++ "\r\n" ++ "\r\n" ++ printSpace s ++ "\r\n")
    (Fail errormessage)     -> print ("error: " ++ errormessage)
    (Ok (ArrowState s p h st)) -> print (printSpace s ++ "\r\n" ++ "\r\n" ++ "press enter to continue... " ++ "\r\n") >> nextstep e (ArrowState s p h st) 


nextstep :: Environment -> ArrowState -> IO ()
nextstep e a = do
  line <- getLine
  if null line
    then print("\r\n") >> interactive e a
    else nextstep e a


batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a = undefined

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  chars <- readFile "examples/Add.arrow"
  putStrLn "Input program:"
  putStrLn ""
  putStrLn chars
  putStrLn ""
  let tokens = alexScanTokens chars
  putStrLn "Tokens:"
  putStrLn ""
  print tokens
  let arr = parser tokens
  putStrLn "Parsed program:"
  putStrLn ""
  print arr

