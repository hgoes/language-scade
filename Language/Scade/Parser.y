{
module Language.Scade.Parser where

import Language.Scade.Tokens
import Language.Scade.Syntax
}

%name scade
%tokentype { Token }
%error { parseError }

%token
  external     { Key KeyExternal }
  function     { Key KeyFunction }
  id           { Identifier $$ }
  node         { Key KeyNode }
  private      { Key KeyPrivate }
  public       { Key KeyPublic }
  '<<'         { Bracket Params False }
  '>>'         { Bracket Params True }
  ','          { Comma }
  '.'          { Dot }
  '('          { Bracket Parentheses False }
  ')'          { Bracket Parentheses True }
  '['          { Bracket Square False }
  ']'          { Bracket Square True }
  '{'          { Bracket Curly False }
  '}'          { Bracket Curly True }
  ';'          { Semicolon }
  ':'          { Colon }
  '::'          { DoubleColon }
  '..'         { DotDot }
  '='          { Op OpEqual }
  '+'          { Op OpPlus }
  '-'          { Op OpMinus }
  '*'          { Op OpTimes }
  '/'          { Op OpDiv }
  '<>'         { Op OpDifferent }
  '^'          { Op OpPower }
  '->'          { Op OpTo }
  '<'          { Op OpLess }
  '>'          { Op OpGreater }
  '<='          { Op OpLessEq }
  '>='          { Op OpGreaterEq }
  '|'          { Op OpOr }
  '$_$'        { PrefixOp' $$ }
  '@'          { Op OpAt }
  abstract     { Key KeyAbstract }
  activate     { Key KeyActivate }
  assume       { Key KeyAssume }
  automaton    { Key KeyAutomaton }
  and          { Key KeyAnd }
  bool         { Key KeyBool }
  case         { Key KeyCase }
  char         { Key KeyChar }
  clock        { Key KeyClock }
  const        { Key KeyConst }
  default      { Key KeyDefault }
  div          { Key KeyDiv }
  do           { Key KeyDo }
  else         { Key KeyElse }
  elsif        { Key KeyElsif }
  emit         { Key KeyEmit }
  end          { Key KeyEnd }
  enum         { Key KeyEnum }
  every        { Key KeyEvery }
  false        { Key KeyFalse }
  fby          { Key KeyFBY }
  final        { Key KeyFinal }
  flatten      { Key KeyFlatten }
  fold         { Key KeyFold }
  foldi        { Key KeyFoldi }
  foldw        { Key KeyFoldw }
  foldwi       { Key KeyFoldwi }
  guarantee    { Key KeyGuarantee }
  if           { Key KeyIf }
  imported     { Key KeyImported }
  int          { Key KeyInt }
  initial      { Key KeyInitial }
  last         { Key KeyLast }
  let          { Key KeyLet }
  make         { Key KeyMake }
  map          { Key KeyMap }
  mapfold      { Key KeyMapFold }
  mapw         { Key KeyMapw }
  mapwi        { Key KeyMapwi }
  match        { Key KeyMatch }
  mod          { Key KeyMod }
  not            { Key KeyNot }
  numeric      { Key KeyNumeric }
  of           { Key KeyOf }
  open         { Key KeyOpen }
  or           { Key KeyOr }
  package      { Key KeyPackage }
  pre          { Key KeyPre }
  probe        { Key KeyProbe }
  real         { Key KeyReal }
  restart      { Key KeyRestart }
  resume       { Key KeyResume }
  returns      { Key KeyReturns }
  reverse      { Key KeyReverse }
  sig          { Key KeySig }
  state        { Key KeyState }
  synchro      { Key KeySynchro }
  tel          { Key KeyTel }
  then         { Key KeyThen }
  times        { Key KeyTimes }
  transpose    { Key KeyTranspose }
  true         { Key KeyTrue }
  type         { Key KeyType }
  unless       { Key KeyUnless }
  until        { Key KeyUntil }
  var          { Key KeyVar }
  when         { Key KeyWhen }
  where        { Key KeyWhere }
  xor          { Key KeyXor }
  const_int    { ConstInt $$ }
  name         { Name $$ }
  const_char   { Char $$ }
  const_float  { ConstFloat $$ }
  '_'          { Identifier "_" }

%left ';' ','
%left else then
%left '->'
%left '=' '<>' '<' '>' '<=' '>='
%left and or xor
%left not
%left pre times
%left '+' '-'
%left '*' '/' mod div
%left '@'
%left reverse transpose int real
%left '^'
%left '['

%%

Declarations : Declaration Declarations { $1:$2 }
             |                          { [] }

Declaration : open Path ';'     { OpenDecl $2 }
            | type TypeDecls    { TypeBlock $2 }
            | const ConstDecls  { ConstBlock $2 }
            | PackageDecl       { $1 }
            | UserOpDeclaration { $1 }

TypeDecls : InterfaceStatus id TypeDefOpt ';' TypeDecls { (TypeDecl $1 $2 $3):$5 }
          |                                             { [] }

TypeDefOpt : '=' TypeExpr            { Just (Left $2) }
           | '=' enum '{' IdList '}' { Just (Right $4) }

ConstDecls : ConstDecl ';' ConstDecls { $1:$3 }
           |                          { [] }

ConstDecl : InterfaceStatus id ':' TypeExpr OptConstInit { ConstDecl $1 $2 $4 $5 }

OptConstInit : '=' Expr { Just $2 }
             |          { Nothing }

UserOpDeclaration : OpKind Imported InterfaceStatus id SizeDecl 
                    '(' ParamList ')' returns '(' ParamList ')'
                    WhereDecl
                    OptBody
                    { UserOpDecl $1 $2 $3 $4 $5 $7 $11 $13 $14 }

OptBody : ';'                                                { DataDef [] [] [] }
        | Equation ';'                                       { DataDef [] [] [$1] }
        | SignalBlockOpt LocalBlockOpt LetBlock OptSemicolon { DataDef $1 $2 $3 }

Imported : imported { True }
         |          { False }

WhereDecl : where name WhereDeclN numeric { $2:$3 }
          |                               { [] }

WhereDeclN : ',' name WhereDeclN { $2:$3 }
           |                     { [] }

PackageDecl : package Visibility id Declarations end ';' { PackageDecl $2 $3 $4 }

DataDef : Equation ';'                          { DataDef [] [] [$1] }
        | SignalBlock LocalBlockOpt LetBlockOpt { DataDef $1 $2 $3 }
        | LocalBlock LetBlockOpt                { DataDef [] $1 $2 }
        | LetBlock                              { DataDef [] [] $1 }
        |                                       { DataDef [] [] [] }


DataDefOpt : DataDef { $1 }
           | ';'     { DataDef [] [] [] }

LetBlock : let Equations tel { $2 }

LetBlockOpt : LetBlock { $1 }
            |          { [] }

OptSemicolon : ';' { () }
             |     { () }

OpKind : function { Function }
       | node     { Node }

InterfaceStatus : Visibility          { InterfaceStatus $1 False }
                | Visibility external { InterfaceStatus $1 True }

Visibility : private { Just Private }
           | public  { Just Public }
           |         { Nothing }

SizeDecl : '<<' IdList '>>' { Just $ SizeDecl $2 }
         |                  { Nothing }

IdList : id IdListN { $1:$2 }
       |            { [] }

IdListN : ',' id IdListN { $2:$3 }
        |                { [] }

ParamList : VarDecl ParamListN { $1:$2 }
          |                    { [] }

ParamListN : ';' VarDecl ParamListN { $2:$3 }
           |                        { [] }

VarDecl : VarId VarDeclN ':' TypeExpr DefaultDecl LastDecl { VarDecl ($1:$2) $4 $5 $6 }

VarDeclN : ',' VarId VarDeclN { $2:$3 }
         |                    { [] }

VarDecls : VarDecl ';' VarDecls { $1:$3 }
         |                      { [] }

VarId : Clock Probe id { VarId $3 $1 $2 }

Clock : clock { True }
      |       { False }

Probe : probe { True }
      |       { False }

DefaultDecl : default '=' Expr { Just $3 }
            |                  { Nothing }

LastDecl : last '=' Expr { Just $3 }
         |               { Nothing }

TypeExpr : bool                              { TypeBool }
         | int                               { TypeInt }
         | real                              { TypeReal }
         | char                              { TypeChar }
         | TypeExpr '^' Expr                 { TypePower $1 $3 }
         | Path                              { TypePath $1 }
         | name                              { TypeVar $1 }
         | '{' id ':' TypeExpr RecordEls '}' { TypeRecord (($2,$4):$5) }
           
RecordEls : ',' id ':' TypeExpr RecordEls { ($2,$4):$5 }
          |                               { [] }

Equation : LHS '=' Expr               { SimpleEquation $1 $3 }
         | AssertType id ':' Expr     { AssertEquation $1 $2 $4 }
         | emit EmissionBody          { EmitEquation $2 }
         | StateMachine Return        { let (xs,e) = $2 in StateEquation $1 xs e }
         | activate Id IfBlock Return { let (xs,e) = $4 in ClockedEquation $2 (Left $3) xs e }

IfBlock : if Expr then IfBlockOrDataDef else IfBlockOrDataDef { IfBlock $2 $4 $6 }

IfBlockOrDataDef : DataDef { Left $1 }
                 | IfBlock { Right $1 }
            
Equations : Equation ';' Equations { $1:$3 }
          |                        { [] }

AssertType : assume    { Assume }
           | guarantee { Guarantee }

LHS : '(' ')'    { [] }
    | LHSId LHSN { $1:$2 }

LHSN : ',' LHSId LHSN { $2:$3 }
     |                { [] }

LHSId : id { case $1 of { "_" -> Bottom ; _ -> Named $1 } }

Expr : Path                                                { IdExpr $1 }
     | name                                                { NameExpr $1 }
     | last name                                           { LastExpr $2 }
     | const_int                                           { ConstIntExpr $1 }
     | true                                                { ConstBoolExpr True }
     | false                                               { ConstBoolExpr False }
     | const_float                                         { ConstFloatExpr $1 }
     | '(' const_int ':' name ')'                          { ConstPolyIntExpr $2 $4 }
     | '(' List ')'                                        { ListExpr $2 }
     | '[' List ']'                                        { ArrayExpr $2 }
     | if Expr then Expr else Expr                         { IfExpr $2 $4 $6 }
     | Expr '+' Expr                                       { BinaryExpr BinPlus $1 $3 }
     | Expr '-' Expr                                       { BinaryExpr BinMinus $1 $3 }
     | Expr '*' Expr                                       { BinaryExpr BinTimes $1 $3 }
     | Expr '/' Expr                                       { BinaryExpr BinDiv $1 $3 }
     | Expr mod Expr                                       { BinaryExpr BinMod $1 $3 }
     | Expr div Expr                                       { BinaryExpr BinRDiv $1 $3 }
     | Expr '=' Expr                                       { BinaryExpr BinEquals $1 $3 }
     | Expr '<>' Expr                                      { BinaryExpr BinDifferent $1 $3 }
     | Expr and Expr                                       { BinaryExpr BinAnd $1 $3 } 
     | Expr or Expr                                        { BinaryExpr BinOr $1 $3 }
     | Expr xor Expr                                       { BinaryExpr BinXor $1 $3 }
     | Expr '->' Expr                                       { BinaryExpr BinAfter $1 $3 }
     | Expr '<' Expr                                       { BinaryExpr BinLesser $1 $3 }
     | Expr '>' Expr                                       { BinaryExpr BinGreater $1 $3 }
     | Expr '<=' Expr                                       { BinaryExpr BinLessEq $1 $3 }
     | Expr '>=' Expr                                       { BinaryExpr BinGreaterEq $1 $3 }
     | Expr '^' Expr                                       { BinaryExpr BinPower $1 $3 }
     | not Expr                                              { UnaryExpr UnNot $2 }
     | pre Expr                                            { UnaryExpr UnPre $2 }
     | '-' Expr                                            { UnaryExpr UnNeg $2 }
     | int Expr                                            { UnaryExpr UnCastInt $2 }
     | real Expr                                           { UnaryExpr UnCastReal $2 }
     | Operator '(' List ')'                               { ApplyExpr $1 $3 }
     | fby '(' List ';' Expr ';' List ')'                  { FBYExpr $3 $5 $7 }
     | reverse Expr                                        { ReverseExpr $2 }
     | transpose '(' Expr ';' const_int ';' const_int ')'  { TransposeExpr $3 $5 $7 }
     | '(' case Expr of '|' Pattern ':' Expr CaseExprs ')' { CaseExpr $3 (($6,$8):$9) }
     | '(' Expr '.' '[' Expr ']' Indices default Expr ')'  { DefaultIndexExpr $2 ($5:$7) $9 }
     | Expr '[' Expr ']'                                   { IndexExpr $1 $3 }
     | Expr '[' Expr '..' Expr ']'                         { StaticProjectionExpr $1 $3 $5 }
     | Expr '@' Expr                                       { AppendExpr $1 $3 }
     | Expr times Expr                                     { TimesExpr $1 $3 }

Indices : '[' Expr ']' Indices { $2:$4 }
        |                      { [] }

CaseExprs : '|' Pattern ':' Expr CaseExprs { (($2,$4):$5) }
          |                                { [] }

ActivateCondition : ClockExpr                 { ActivateClock $1 }
                  | Expr default Expr         { ActivateDefault $1 $3 }
                  | Expr initial default Expr { ActivateInitialDefault $1 $4 }

Path : id PathN { Path ($1:$2) }

PathN : '::' id PathN { $2:$3 }
      |              { [] }

SignalBlock : sig id SigN ';' { $2:$3 }

SignalBlockOpt : SignalBlock { $1 }
               |             { [] }


SigN : ',' id SigN { $2:$3 }
     |             { [] }

LocalBlock : var VarDecls { $2 }
           
LocalBlockOpt : LocalBlock { $1 }
              |            { [] }

List : Expr ListN { $1:$2 }
     |            { [] }

ListN : ',' Expr ListN { $2:$3 }
      |                { [] }

Operator : Prefix                                            { PrefixOp $1 }
         | '(' Prefix '<<' List '>>' ')'                     { PrefixParamOp $2 $4 }
         | '(' make Path ')'                                 { Make $3 }
         | '(' flatten Path ')'                              { Flatten $3 }
         | '(' Iterator Operator '<<' Expr '>>' ')'          { IteratorOp $2 $3 $5 }
         | '(' activate Operator every ActivateCondition ')' { ActivateOp $3 $5 }
         | '(' restart Operator every Expr ')'               { RestartOp $3 $5 }
         | '(' mapw Operator '<<' Expr '>>' if Expr default Expr ')' { MapWOp $3 $5 $8 $10 }
         | '(' mapwi Operator '<<' Expr '>>' if Expr default Expr ')' { MapWiOp $3 $5 $8 $10 }
         | '(' foldw Operator '<<' Expr '>>' if Expr ')'     { FoldWOp $3 $5 $8 }
         | '(' foldwi Operator '<<' Expr '>>' if Expr ')'    { FoldWiOp $3 $5 $8 }



Prefix : Path  { PrefixPath $1 }
       | '$_$' { PrefixBinOp (case $1 of { OpPlus -> BinPlus ; OpMinus -> BinMinus ; OpTimes -> BinTimes ; OpDiv -> BinDiv ; OpAnd -> BinAnd ; OpOr -> BinOr ; OpLessEq ->  BinLessEq }) }

Iterator : mapfold { ItMapFold }
         | fold    { ItFold }
         | foldi   { ItFoldI }
         | map     { ItMap }

StateMachine : automaton Id StateDecl StateDecls { StateMachine $2 ($3:$4) }

StateDecls : StateDecl StateDecls { $1:$2 }
           |                      { [] }

Id : id { Just $1 }
   |    { Nothing }

EmissionBody : name PostfixIfExpr                       { EmissionBody [$1] $2 }
             | '(' name EmissionBodyN ')' PostfixIfExpr { EmissionBody ($2:$3) $5 }

EmissionBodyN : ',' name EmissionBodyN { $2:$3 }
              |                        { [] }

PostfixIfExpr : if Expr { Just $2 }
              |         { Nothing}

Actions : do '{' Emit EmissionBody ActionsN '}' { ActionEmission (($3,$4):$5) }
        | do DataDef                            { ActionDef $2 }

ActionsN : ';' Emit EmissionBody ActionsN { ($2,$3):$4 }
         |                                { [] }

Emit : emit { True }
     |      { False }

ActionsOpt : Actions { Just $1 }
           |         { Nothing }

Transition : if Expr ActionsOpt Fork { Transition $2 $3 $4 }

Transitions : Transition ';' Transitions { $1:$3 }
            |                            { [] }

Fork : TargetType id                                 { TargetFork $1 $2 }
     | if Expr ActionsOpt Fork ElifFork ElseFork end { ConditionalFork (($2,$3,$4):$5) $6 }
          
ElifFork : elsif Expr ActionsOpt Fork ElifFork { ($2,$3,$4):$5 }
         |                                     { [] }

ElseFork : else ActionsOpt Fork { Just ($2,$3) }
         |                      { Nothing }
          
TargetType : restart { Restart }
           | resume  { Resume }

StateDecl : Initial Final state id StateUnless DataDef StateUntil { State $1 $2 $4 $6 $5 (fst $7) (snd $7) }

StateUnless : unless Transition ';' Transitions { $2:$4 }
            |                                   { [] }

StateUntil : until Transitions StateSynchro { ($2,$3) }
           |                                { ([],Nothing) }

StateSynchro : synchro ActionsOpt Fork ';' { Just ($2,$3) }
             |                             { Nothing }
                                           
Initial : initial { True }
        |         { False }

Final : final { True }
      |       { False }

Return : returns ReturnIds { $2 }

ReturnIds : id ',' ReturnIds { let (xs,e) = $3 in ($1:xs,e) }
          | id               { ([$1],False) }
          | '..'             { ([],True) }

ClockExpr : id                       { ClockId $1 }
          | not id                     { ClockNotId $2 }
          | '(' id match Pattern ')' { ClockMatch $2 $4 }

Pattern : Path       { PatPath $1 }
        | const_char { PatChar $1 }
        | const_int  { PatInt $1 }
        | true       { PatBool True }
        | false      { PatBool False }
        | '_'        { PatBottom }

{
parseError xs = error ("Parse error at "++show (take 5 xs))
}
