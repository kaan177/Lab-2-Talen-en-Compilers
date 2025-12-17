module Model where


-- Exercise 1
data Token = TArrow | TPeriod | TComma | TGo | TTake | TMark 
 | TNothing | TTurn | TCase | TOf | TEnd | TLeft | TRight | TFront
 | TSemicolon | TEmpty | TLambda | TDebris | TAsteroid
 | TBoundary | TUnderscore | TIdent String 
 deriving Show


-- Exercise 2
data Program = Program [Rule] deriving Show

data Rule = Rule String Cmds deriving Show
data Cmds = EmptyCmds | Cmds Cmd Cmds deriving Show
data Cmd = Go | Take | Mark | NothingCmd | Turn Dir | Case Dir Alts | Ident String deriving Show
data Dir = Left | Right | Front deriving Show
data Alts = EmptyAlts | Alts Alt Alts deriving Show
data Alt = Alt Pat Cmds deriving Show
data Pat = EmptyPat | LambdaPat | DebrisPat | AsteroidPat | BoundaryPat | CatchAllPat deriving Show
