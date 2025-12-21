module Main where

import Algebra
import Model
import Interpreter
import Lexer
import Parser
import Options.Applicative.Common (runParser)
import Data.Char (GeneralCategory(Space))

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive e a = case (step e a) of
    (Done s p h) -> putStr ("final state: " ++ "\r\n" ++ "\r\n" ++ printSpace s p h++ "\r\n")
    (Fail errormessage)     -> putStr ("error: " ++ errormessage)
    (Ok (ArrowState s p h st)) -> let
      printText = putStr (printSpace s p h ++ "\r\n" ++ "\r\n" ++ "press enter to continue... " ++ "\r\n")
      recursivecall =  putStr("\r\n") >> interactive e (ArrowState s p h st) 
      in printText >> getLine >> recursivecall --Get line is hier om te wachten voor user input


batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a = case (step e a) of
    (Done s p h) -> (s, p, h)
    (Ok aNew)    -> batch e aNew
    (Fail s)     -> error s


parsecommand :: String -> (Bool, String, String, (Pos, Heading))
parsecommand s = (\ (_ : _ : _ : m : e : s : x : y : h : _) -> (parsemode m, e, s, ((read x :: Int, read y :: Int), parseheading h)))  (words s)

parsemode :: String -> Bool
parsemode "batch" = True
parsemode "interactive" = False
parsemode s = error ("No instance for mode: " ++ s)

parseheading :: String -> Heading
parseheading "up" = North
parseheading "down" = South
parseheading "right" = East
parseheading "left" = West
parseheading s = error ("No instance for heading: " ++ s)

-- This function is just here to play around with and test your lexer/parser.
-- When implementing exercise 11, delete this comment and this function,
-- and write a new main function.
main :: IO ()
main = do
  line <- getLine
  (isBatch, envFileName, spaceFileName, (pos, heading)) <- return (parsecommand line)
  environmentText <- readFile envFileName
  spaceText <- readFile spaceFileName
  if isBatch
    then let 
      environment = toEnvironment environmentText
      space = runParceSpace spaceText 
      arrowstate = ArrowState space pos heading [Ident "start"]
      (finalSpace, p , h) = batch environment arrowstate
      in putStr ("final state: " ++ "\r\n" ++ "\r\n" ++ printSpace finalSpace p h ++ "\r\n")
    else let 
      environment = toEnvironment environmentText
      space = runParceSpace spaceText 
      arrowstate = ArrowState space pos heading [Ident "start"]
      in interactive environment arrowstate
  
  
  {-
  do
  chars <- readFile "examples/RemoveDebris.arrow"
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
-}
