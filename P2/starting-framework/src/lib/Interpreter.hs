module Interpreter where

import ParseLib.Core
import ParseLib.Derived

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra


data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq,Ord)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents



-- | Parses a space file, such as the ones in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural)
                <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ Map.fromList $ concat $
            zipWith (\r cs ->
              zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)

    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable)
      <* spaces


-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]


-- Exercise 7
printSpace :: Space -> String
printSpace s =
  let m = Map.fromList contentsTable
      r = (m Map.!) <$> s
      i@(mr, mc) = last $ fst <$> Map.toList r
   in show i ++ "\r\n" ++ interspace (mc + 1) "\r\n" (snd <$> Map.toList r) ++ "\r\n"
  where
    interspace :: Int -> String -> String -> String
    interspace n s str
      | n < length str = take n str ++ s ++ interspace n s (drop n str)
      | otherwise = str


-- These three should be defined by you
type Ident = String
type Commands = Cmds
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment input = let 
              program = parser (alexScanTokens input) 
              (Program rules) = program
              in if checkProgram program then mapAllRules rules else undefined

--Adds all rules to a map
mapAllRules :: [Rule] -> Environment
mapAllRules = foldr (\(Rule a b) -> Map.insert a b) Map.empty

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


