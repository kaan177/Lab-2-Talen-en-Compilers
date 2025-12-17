module Algebra where

import Model


-- Exercise 5
type Algebra = (Program -> Bool, [Rule] -> Program, String -> [Cmds] -> Rule)
fold :: Algebra -> Bool
fold (doProgram, doRule, doCommands) = undefined



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram (Program rules) = undefined

-- Check if rule name exists
checkAllRulesExist :: [Rule] -> Bool
checkAllRulesExist rules = all (checkRuleExists rules . getRuleName) rules

checkRuleExists :: [Rule] -> String -> Bool
checkRuleExists existingRules ruleName = any (\x -> getRuleName x == ruleName) existingRules

--Check if rule named start exists
checkRuleNamedStart :: [Rule] -> Bool
checkRuleNamedStart rules = checkRuleExists rules "start"

--Check that no rule is defined twice
checkNoDuplicates :: [Rule] -> Bool
checkNoDuplicates = not . checkDuplicates . map getRuleName

--Check all pattern matches
checkPatternMatches :: [Cmd] -> Bool
checkPatternMatches = all checkPatternMatch

checkPatternMatch :: Cmd -> Bool
checkPatternMatch cmd = let
                            patterns = getCommandPatterns cmd
                            hasEmptyPattern = elem "EmptyPat" (map patternToString patterns)
                            hasLabdaPattern = elem "LambdaPat" (map patternToString patterns)
                            hasDebrisPattern = elem "DebrisPat" (map patternToString patterns)
                            hasAsteroidPattern = elem "AsteroidPat" (map patternToString patterns)
                            hasBoundaryPattern = elem "BoundaryPat" (map patternToString patterns)
                            hasCatchAllPattern = elem "CatchAllPat" (map patternToString patterns)
                            --If does not contain patterns, has catch all pattern, or contains all other pattern types then valid
                            in null patterns || hasCatchAllPattern || (hasEmptyPattern && hasLabdaPattern && hasDebrisPattern && hasAsteroidPattern && hasBoundaryPattern)


getCommandPatterns :: Cmd -> [Pat]
getCommandPatterns (Case _ (Alts a as)) = let
                        --Turn Alt into [Pat]
                        getPattern (Alt pat _) = pat
                        in map getPattern (a:as)
getCommandPatterns _ = []

--Helpers
--Extracts rule name
getRuleName :: Rule -> String
getRuleName (Rule s cmds) = s

--Checks if a list contains duplicates
checkDuplicates :: Eq e => [e] -> Bool
checkDuplicates xs = f xs []
                where
                    f [] _ = False
                    f (x:xs) visited = elem x visited || f xs (x:visited)

--Easy way to check which patterns we have
patternToString :: Pat -> String
patternToString EmptyPat = "EmptyPat"
patternToString LambdaPat = "LambdaPat"
patternToString DebrisPat = "DebrisPat"
patternToString AsteroidPat = "AsteroidPat"
patternToString BoundaryPat = "BoundaryPat"
patternToString CatchAllPat = "CatchAllPat"


-- Probably wrong but keeping just in case
-- getCommandPatterns :: [Cmd] -> [Pat]
-- getCommandPatterns cmds = let
--                         --Get all that are of type Case
--                         hasPattern (Case _ a) = Just a
--                         hasPattern _ = Nothing
--                         maybeAlts = map hasPattern cmds
--                         --Turn [Maybe Alts] into [Alts] (And only the Alts that are not EmptyAlts)
--                         getAlts [] = []
--                         getAlts ((Just (Alts a b)):xs) = Alts a b : getAlts xs
--                         getAlts (_:xs) = getAlts xs
--                         alts = getAlts maybeAlts
--                         --Turn [Alts] into [Maybe Pat]
--                         getPattern (Alt pat _) = Just pat
--                         getPattern _ = Nothing
--                         getIndividualAlts (Alts a as) = a:as 
--                         maybePatterns = map getPattern ( concatMap getIndividualAlts alts)
--                         in undefined
                        