module Algebra where

import Model


-- Exercise 5
type Algebra a = (a, a, a, a, Dir -> a, Dir -> Alts -> a, String -> a, a -> a -> a)
fold :: Algebra a -> a -> Cmd -> a
fold (doGo, doTake, doMark, doNothing, doTurn, doCase, doIdent, combineCases) base = combineCases base . go
    where
        -- Cmd -> a
        go Go = doGo
        go Take = doTake
        go Mark = doMark
        go NothingCmd = doNothing
        go (Turn dir) = doTurn dir
        --Not sure if should have run doCase every time or just call all commands within it, but works fine both ways for what it is used for
        go (Case dir EmptyAlts) = doCase dir EmptyAlts
        go (Case dir (Alts oriAlt@(Alt _ cmds) EmptyAlts)) = combineCases (doCase dir (Alts oriAlt EmptyAlts)) (goCmds cmds)
        go (Case dir (Alts oriAlt@(Alt _ cmds) oriAlts@(Alts alt restAlts))) = combineCases (doCase dir (Alts oriAlt oriAlts)) (combineCases (goCmds cmds) (go (Case dir (Alts alt restAlts)))) --Replace doCase with go!!!!
        go (Ident s) = doIdent s
        -- Cmds -> a
        goCmds EmptyCmds = base
        goCmds (Cmds cmd cmds) = combineCases (go cmd) (goCmds cmds)

-- Exercise 6
checkProgram :: Program -> Bool
checkProgram (Program rules) = checkAllRulesExist rules && checkRuleNamedStart rules && checkNoDuplicates rules && checkPatternMatches (concatMap getRuleCommands rules)

-- Check if rule name exists
checkAllRulesExist :: [Rule] -> Bool
checkAllRulesExist rules = all (all (fold
                            (True, --doGo
                            True, --doTake
                            True, --doMark
                            True, --doNothing
                            const True, --doTurn
                            const . const True, --doCase
                            checkRuleExists rules, --doIdent
                            (&&)) --combineCases
                            True --base case
                            ) . getRuleCommands) rules

checkRuleExists :: [Rule] -> String -> Bool
checkRuleExists existingRules ruleName = any (\x -> getRuleName x == ruleName) existingRules

--Check if rule named start exists
checkRuleNamedStart :: [Rule] -> Bool
checkRuleNamedStart rules = checkRuleExists rules "start"

--Check that no rule is defined twice
checkNoDuplicates :: [Rule] -> Bool
checkNoDuplicates = not . checkDuplicates . map getRuleName

--Check all pattern matches
--Does not check for cases within cases, just hope that it does not occur
checkPatternMatches :: [Cmd] -> Bool
checkPatternMatches = all (fold
                            (True, --doGo
                            True, --doTake
                            True, --doMark
                            True, --doNothing
                            const True, --doTurn
                            \dir alts -> checkPatternMatch (Case dir alts), --doCase
                            const True, --doIdent
                            (&&)) --combineCases
                            True --base case
                            )

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

--Gets the commands in a rule and when it is a Case command, it also extracts the rules in there
getRuleCommandsIncludingCases :: Rule -> [Cmd]
getRuleCommandsIncludingCases (Rule _ cmds) = let
                                              cmdList = cmdsToCmdList' cmds
                                              allCmds = concatMap getCaseCommandsOrSelf cmdList
                                              in allCmds

cmdsToCmdList' :: Cmds -> [Cmd]
cmdsToCmdList' EmptyCmds = []
cmdsToCmdList' (Cmds cmd cmds) = cmd : cmdsToCmdList' cmds

removeNonIdentCommands :: [Cmd] -> [Cmd]
removeNonIdentCommands = filter checkIsIdentCommand

checkIsIdentCommand :: Cmd -> Bool
checkIsIdentCommand (Ident _) = True
checkIsIdentCommand _         = False

getCommandName :: Cmd -> String
getCommandName (Ident s) = s
getCommandName _         = "Does not have name" --Should not happen

--Get the cmds hiddin in a case command
getCaseCommandsOrSelf :: Cmd -> [Cmd]
getCaseCommandsOrSelf (Case _ alts) = getAltsCommands alts
getCaseCommandsOrSelf cmd = [cmd]

getAltsCommands :: Alts -> [Cmd]
getAltsCommands (Alts (Alt _ cmds) EmptyAlts) = cmdsToCmdList' cmds
getAltsCommands (Alts (Alt _ cmds) alts) = cmdsToCmdList' cmds ++ getAltsCommands alts

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
