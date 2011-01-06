{
module Language.Scade.Lexer where

import Language.Scade.Tokens

}

%wrapper "basic"

$digit2 = 0-1
$digit8 = 0-7
$digit10 = 0-9
$digit16 = [a-fA-F0-9]
$letter = [a-zA-Z\_]

tokens :- 
  $white+                                              ;
  "/*" ([\x00-\xff] # \* | \* [\x00-\xff] # \/)* "*/"  ;
  "#pragma" ([\x00-\xff] # \#)* "#end"                 ;
  "#" (. # \ )+                                        ;
  abstract                                             { key KeyAbstract }
  activate                                             { key KeyActivate }
  and                                                  { key KeyAnd }
  assume                                               { key KeyAssume }
  automaton                                            { key KeyAutomaton }
  bool                                                 { key KeyBool }
  case                                                 { key KeyCase }
  default                                              { key KeyDefault }
  div                                                  { key KeyDiv }
  else                                                 { key KeyElse }
  end                                                  { key KeyEnd }
  enum                                                 { key KeyEnum }
  every                                                { key KeyEvery }
  fby                                                  { key KeyFBY }
  flatten                                              { key KeyFlatten }
  fold                                                 { key KeyFold }
  foldi                                                { key KeyFoldi }
  foldw                                                { key KeyFoldw }
  function                                             { key KeyFunction }
  guarantee                                            { key KeyGuarantee }
  if                                                   { key KeyIf }
  imported                                             { key KeyImported }
  initial                                              { key KeyInitial }
  int                                                  { key KeyInt }
  last                                                 { key KeyLast }
  let                                                  { key KeyLet }
  make                                                 { key KeyMake }
  map                                                  { key KeyMap }
  mapfold                                              { key KeyMapFold }
  mod                                                  { key KeyMod }
  node                                                 { key KeyNode }
  not                                                  { key KeyNot }
  numeric                                              { key KeyNumeric }
  of                                                   { key KeyOf }
  open                                                 { key KeyOpen }
  or                                                   { key KeyOr }
  package                                              { key KeyPackage }
  pre                                                  { key KeyPre }
  private                                              { key KeyPrivate }
  public                                               { key KeyPublic }
  real                                                 { key KeyReal }
  restart                                              { key KeyRestart }
  resume                                               { key KeyResume }
  returns                                              { key KeyReturns }
  reverse                                              { key KeyReverse }
  state                                                { key KeyState }
  tel                                                  { key KeyTel }
  then                                                 { key KeyThen }
  transpose                                            { key KeyTranspose }
  true                                                 { key KeyTrue }
  type                                                 { key KeyType }
  unless                                               { key KeyUnless }
  var                                                  { key KeyVar }
  when                                                 { key KeyWhen }
  where                                                { key KeyWhere }
  xor                                                  { key KeyXor }
  "("                                                  { const $ Bracket Parentheses False }
  ")"                                                  { const $ Bracket Parentheses True }
  "<<"                                                 { const $ Bracket Params False }
  ">>"                                                 { const $ Bracket Params True }
  "["                                                  { const $ Bracket Square False }
  "]"                                                  { const $ Bracket Square True }
  "{"                                                  { const $ Bracket Curly False }
  "}"                                                  { const $ Bracket Curly True }
  ">="                                                 { op OpGreaterEq }
  "<="                                                 { op OpLessEq }
  "="                                                  { op OpEqual }
  "+"                                                  { op OpPlus }
  "^"                                                  { op OpPower }
  "<>"                                                 { op OpDifferent }
  "<"                                                  { op OpLess }
  ">"                                                  { op OpGreater }
  "->"                                                 { op OpTo }
  "-"                                                  { op OpMinus }
  "*"                                                  { op OpTimes }
  "|"                                                  { op OpOr }
  "/"                                                  { op OpDiv }
  "@"                                                  { op OpAt }
  "$*$"                                                { pop OpTimes }
  "$-$"                                                { pop OpMinus }
  "$+$"                                                { pop OpPlus }
  "$and$"                                              { pop OpAnd }
  "$<=$"                                               { pop OpLessEq }
  "$>=$"                                               { pop OpGreaterEq }
  "::"                                                 { const DoubleColon }
  ":"                                                  { const Colon }
  ";"                                                  { const Semicolon }
  ","                                                  { const Comma }
  ".."                                                 { const DotDot }
  "."                                                  { const Dot }
  "'" $letter($digit10|$letter)*                       { \s -> Name (tail s) }
  "'" (. # \'){1,2} "'"                                { \s -> Char (read s) }
  "-"? $digit10* "." $digit10+ (("e"|"E") ("+"|"-")? $digit10+)? { \s -> ConstFloat (read s) }
  "-"? $digit10+                                       { \s -> ConstInt $ read s }
  $letter($digit10|$letter)*                           { \s -> Identifier s }
{
key :: Keyword -> String -> Token
key w _ = Key w

op :: Operator -> String -> Token
op o _ = Op o

pop :: Operator -> String -> Token
pop o _ = PrefixOp' o
}
