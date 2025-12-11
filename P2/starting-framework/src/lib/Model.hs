module Model where


-- Exercise 1
data Token = TArrow | TPeriod | TComma | TGo | TTake | TMark 
 | TNothing | TTurn | TCase | TOf | TLeft | TRight | TFront
 | TSemicolon | TEmpty | TLambda | TDebris | TAsteroid
 | TBoundary | TUnderscore | TIdent String deriving Show


-- Exercise 2
data Program = Program deriving Show

data Rule = Rule (String -> Cmds)
data Cmds = EmptyCmds | Cmds Cmd [Cmd]
data Cmd = Go | Take | Mark | NothingCmd | Turn Dir | Case (Dir -> Pat -> Cmds)
data Dir = Left | Right | Front
data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | CatchAllPat
