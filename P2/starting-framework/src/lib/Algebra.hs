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
checkRuleExists :: [Rule] -> String -> Bool
checkRuleExists existingRules ruleName = any (\x -> getRuleName x == ruleName) existingRule

--Check if rule named start exists
checkRuleNamedStart :: [Rule] -> Bool
checkRuleNamedStart rules = checkRuleExists rules "start"

--Check that no rule is defined twice
checkNoDuplicates :: [Rule] -> Bool
checkNoDuplicates = not . checkDuplicates

--Check all pattern matches
--TODO

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
