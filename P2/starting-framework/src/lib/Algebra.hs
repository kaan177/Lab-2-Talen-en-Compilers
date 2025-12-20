module Algebra where

import Model


-- Exercise 5
--This exercise is weird. Program can only be a List of rule and nothing else so fold will always only get a program and cannot call recursively. Thus it does not make sense to have a single fold type
type Algebra a = (Program -> a, Rule -> a, Cmd -> a, Alt -> a)
fold :: Algebra a -> b -> a
fold (doProgram, doRule, doCommand, doAlt) = undefined



-- Exercise 6
--Was easier to do without Algebra (because I did not understand how to use Algebra correctly in this context)
checkProgram :: Program -> Bool
checkProgram (Program rules) = checkAllRulesExist rules && checkRuleNamedStart rules && checkNoDuplicates rules && checkPatternMatches (concatMap getRuleCommands rules)

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
getCommandPatterns (Case _ alts) = getCommandPatterns' alts
getCommandPatterns _ = []

getCommandPatterns' :: Alts -> [Pat]
getCommandPatterns' (Alts alt alts) = let
                        --Turn Alt into [Pat]
                        getPattern (Alt pat _) = pat
                        in getPattern alt : getCommandPatterns' alts
getCommandPatterns' _ = []

--Helpers
--Extracts rule name
getRuleName :: Rule -> String
getRuleName (Rule s cmds) = s

getRuleCommands :: Rule -> [Cmd]
getRuleCommands (Rule _ cmds) = cmdsToCmdList' cmds

cmdsToCmdList' :: Cmds -> [Cmd]
cmdsToCmdList' EmptyCmds = []
cmdsToCmdList' (Cmds cmd cmds) = cmd : cmdsToCmdList' cmds

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
