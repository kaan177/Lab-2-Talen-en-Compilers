{
module Parser where

import Model
}

%name parser
%tokentype { Token }

%token
  '->' { TArrow }
  '.' { TPeriod }
  ',' { TComma }
  go  { TGo }
  take { TTake }
  mark { TMark }
  nothing { TNothing }
  turn { TTurn }
  case { TCase }
  of { TOf }
  end { TEnd }
  left { TLeft }
  right { TRight }
  front { TFront }
  ';' { TSemicolon }
  empty { TEmpty }
  lambda { TLambda }
  debris { TDebris }
  asteroid { TAsteroid }
  boundary { TBoundary }
  '_' { TUnderscore }
  ident { TIdent $$ }
   


%%

Program : Rules           { Program $1 }
Rules   : {- empty -}     { [] }
        | Rules Rule      { $2 : $1 }
Rule    : ident '->' Commands '.' {Rule $1 $3}
Commands: {- empty -}     { EmptyCmds }
        | Command         { Cmds $1 EmptyCmds }
        | Command ',' Commands { Cmds $1 $3 }
Command : go              { Go }
        | take            { Take }
        | mark            { Mark }
        | nothing         { NothingCmd }
        | turn Direction  { Turn $2 }
        | case Direction of Alts end { Case $2 $4}
        | ident           { Ident $1}
Direction: left           { Model.Left }
         | right          { Model.Right }
         | front          { Model.Front }
Alts    : {- empty -}     { EmptyAlts }
        | Alt             { Alts $1 EmptyAlts}
        | Alt  ';' Alts        { Alts $1 $3 }
Alt     : Pattern '->' Commands { Alt $1 $3 }
Pattern : empty           { EmptyPat }
        | lambda          { LambdaPat }
        | debris          { DebrisPat }
        | asteroid        { AsteroidPat }
        | boundary        { BoundaryPat }
        | '_'             { CatchAllPat }
        

{

happyError _ = error "parse error"

}