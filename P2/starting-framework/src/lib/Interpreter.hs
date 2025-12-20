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
import Data.Maybe

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
type Commands = [Cmd]
data Heading = North | East | South | West

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
              in if checkProgram program then mapAllRules rules else undefined --Replace with Map.Empty?

--Adds all rules to a map
mapAllRules :: [Rule] -> Environment
mapAllRules = foldr (\(Rule a b) -> Map.insert a (cmdsToCmdList b)) Map.empty

cmdsToCmdList :: Cmds -> [Cmd]
cmdsToCmdList EmptyCmds = []
cmdsToCmdList (Cmds cmd cmds) = cmd : cmdsToCmdList cmds

-- | Exercise 9
--NOTE: when step is called on first turn, we will already have Stack contain the start function
step :: Environment -> ArrowState -> Step
step environment (ArrowState space pos heading []) = Done space pos heading
step environment (ArrowState space pos heading (topStack:restStack)) = let
                                                        cmd = topStack
                                                        (newState, possibleError) = case cmd of
                                                          Go -> handleGoCase (ArrowState space pos heading restStack)
                                                          Take -> handleTakeCase (ArrowState space pos heading restStack)
                                                          Mark -> handleMarkCase (ArrowState space pos heading restStack)
                                                          NothingCmd -> (ArrowState space pos heading restStack, "")
                                                          Turn dir -> (handleTurnCase (ArrowState space pos heading restStack) dir, "")
                                                          Case dir alts -> (handleCaseCase (ArrowState space pos heading restStack) dir alts, "")
                                                          Ident funcName -> handleIdentCase environment (ArrowState space pos heading restStack) funcName
                                                          --If no error then newState, otherwise error
                                                        in if possibleError == "" then Ok newState else Fail possibleError

handleGoCase :: ArrowState -> (ArrowState, String)
handleGoCase (ArrowState space pos heading stack) = let
                                                   newPos = forwardPos pos heading
                                                   in if isNothing $ Map.lookup newPos space
                                                    --Is not is space, then would be out of bounds
                                                    then (ArrowState space pos heading stack, "Position is out of bounds, pos: " ++ show newPos)
                                                    --Otherwise we have a valid position
                                                    else let
                                                      content = space Map.! newPos
                                                      finalPos = if content == Empty || content == Lambda || content == Debris then newPos else pos
                                                      in (ArrowState space finalPos heading stack, "")

handleTakeCase :: ArrowState -> (ArrowState, String)
handleTakeCase (ArrowState space pos heading stack) = let
                                                      newPos = forwardPos pos heading
                                                      in if isNothing $ Map.lookup newPos space
                                                        --Is not is space, then would be out of bounds
                                                        then (ArrowState space pos heading stack, "Position is out of bounds, pos: " ++ show newPos)
                                                        --Otherwise we have a valid position
                                                        else let
                                                          content = space Map.! newPos
                                                          --If content in front is Lambda or Debris, replace with empty, otherwise do nothing
                                                          newSpace = if content == Lambda || content == Debris then Map.insert newPos Empty space else space
                                                          in (ArrowState newSpace pos heading stack, "")

handleMarkCase :: ArrowState -> (ArrowState, String)
handleMarkCase (ArrowState space pos heading stack) = let
                                                      newPos = forwardPos pos heading
                                                      in if isNothing $ Map.lookup newPos space
                                                        --Is not is space, then would be out of bounds
                                                        then (ArrowState space pos heading stack, "Position is out of bounds, pos: " ++ show newPos)
                                                        --Otherwise we have a valid position
                                                        else let
                                                          newSpace = Map.insert newPos Lambda space
                                                          in (ArrowState newSpace pos heading stack, "")

handleTurnCase :: ArrowState -> Dir -> ArrowState
handleTurnCase (ArrowState space pos heading stack) Model.Left  = ArrowState space pos (rotateLeft heading) stack
handleTurnCase (ArrowState space pos heading stack) Model.Right = ArrowState space pos (rotateRight heading) stack
handleTurnCase (ArrowState space pos heading stack) _           = ArrowState space pos heading stack --Do nothing on turn forward

handleCaseCase :: ArrowState -> Dir -> Alts -> ArrowState
handleCaseCase (ArrowState space pos heading stack) dir alts = let
                                                            posToCheck = forwardPos pos (dirToHeading dir heading)
                                                            contents = if isJust $ Map.lookup posToCheck space then space Map.! posToCheck else Boundary
                                                            altList = altsToaltList alts
                                                            (Alt _ cmds) = findAltMatch altList contents
                                                            newCommands = cmdsToCmdList cmds
                                                            in ArrowState space pos heading (newCommands ++ stack)

--NOTE: Stack in state has already have its top Cmd removed
handleIdentCase :: Environment -> ArrowState -> String -> (ArrowState, String)
handleIdentCase env (ArrowState space pos heading stack) ruleName = let
                                     ruleExists = Map.lookup ruleName env
                                     --Error handling
                                     in if isNothing ruleExists then (ArrowState space pos heading stack, "Could not find rule with name: " ++ ruleName)
                                     --Return next state
                                     else (ArrowState space pos heading ((env Map.! ruleName) ++ stack), "")


--Helpers
forwardPos :: Pos -> Heading -> Pos
forwardPos (x, y) heading = let
                              (dirX, dirY) = case heading of
                                North -> (0, 1)
                                East  -> (1, 0)
                                South -> (0, -1)
                                West  -> (-1, 0)
                                in (x+dirX, y+dirY)

dirToHeading :: Dir -> Heading -> Heading
dirToHeading Model.Front heading = heading
dirToHeading Model.Left  heading = rotateLeft  heading
dirToHeading Model.Right heading = rotateRight heading

rotateRight :: Heading -> Heading
rotateRight heading = let
                      newHeading = case heading of
                        North -> East
                        East  -> South
                        South -> West
                        West  -> North
                      in newHeading

rotateLeft :: Heading -> Heading
rotateLeft heading = let
                      newHeading = case heading of
                        North -> West
                        East  -> North
                        South -> East
                        West  -> South
                      in newHeading

altsToaltList :: Alts -> [Alt]
altsToaltList EmptyAlts = []
altsToaltList (Alts alt alts) = alt : altsToaltList alts

findAltMatch :: [Alt] -> Contents -> Alt
--Can safely use head because we already checked earlier that there is always an exaustive case listing
--Head is only used because it is possible that we have a normal match and the CatchAll pattern match (in which case that would be the last one)
findAltMatch alts contents = head $ filter (`checkAltMatch` contents) alts

checkAltMatch :: Alt -> Contents -> Bool
checkAltMatch (Alt pat _) contents =
                                (pat == EmptyPat    && contents == Empty)    ||
                                (pat == LambdaPat   && contents == Lambda)   ||
                                (pat == DebrisPat   && contents == Debris)   ||
                                (pat == AsteroidPat && contents == Asteroid) ||
                                (pat == BoundaryPat && contents == Boundary) ||
                                (pat == CatchAllPat)