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

%right times
%left ';' ','
%left else then
%left '->'
%left or xor
%left and
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' mod div
%left pre
%left reverse transpose int real
%left when
%left not
%left '^'
%left '@'
%left '.' '['

%%

list(p,q) : p lists(p,q) { $1:$2 }
          |              { [] }

list1(p,q) : p lists(p,q) { $1:$2 }

lists(p,q) : q p lists(p,q) { $2:$3 }
           |                { [] }

maybe(p) : p { Just $1 }
         |   { Nothing }

bool(p) : p { True }
        |   { False }

many(p) : p many(p) { $1:$2 }
        |           { [] }

many1(p) : p many(p) { $1:$2 }

fby(p,q) : p q { $1 }

Declarations : many(Declaration) { $1 }

Declaration : open Path ';'                  { OpenDecl $2 }
            | type many(fby(TypeDecl,';'))   { TypeBlock $2 }
            | const many(fby(ConstDecl,';')) { ConstBlock $2 }
            | PackageDecl                    { $1 }
            | UserOpDeclaration              { $1 }

TypeDecl : InterfaceStatus id maybe(TypeDef) { TypeDecl $1 $2 $3 }

TypeDef : '=' TypeExpr                  { $2 }
        | '=' enum '{' list(id,',') '}' { TypeEnum $4 }

ConstDecl : InterfaceStatus id ':' TypeExpr maybe(ConstInit) { ConstDecl $1 $2 $4 $5 }

ConstInit : '=' Expr { $2 }

UserOpDeclaration : OpKind bool(imported) InterfaceStatus id maybe(SizeDecl)
                    '(' list(VarDecl,';') ')' returns '(' list(VarDecl,';') ')'
                    WhereDecl
                    OptBody
                    { UserOpDecl $1 $2 $3 $4 $5 $7 $11 $13 $14 }

OptBody : ';'                                             { DataDef [] [] [] }
        | Equation ';'                                    { DataDef [] [] [$1] }
        | SignalBlockOpt LocalBlockOpt LetBlock bool(';') { DataDef $1 $2 $3 }

WhereDecl : where list1(name,',') numeric { $2 }
          |                               { [] }

PackageDecl : package maybe(Visibility) id Declarations end ';' { PackageDecl $2 $3 $4 }

DataDef : Equation ';'                          { DataDef [] [] [$1] }
        | SignalBlock LocalBlockOpt LetBlockOpt { DataDef $1 $2 $3 }
        | LocalBlock LetBlockOpt                { DataDef [] $1 $2 }
        | LetBlock                              { DataDef [] [] $1 }
        |                                       { DataDef [] [] [] }

LetBlock : let many(fby(Equation,';')) tel { $2 }

LetBlockOpt : LetBlock { $1 }
            |          { [] }

OpKind : function { Function }
       | node     { Node }

InterfaceStatus : maybe(Visibility)          { InterfaceStatus $1 False }
                | maybe(Visibility) external { InterfaceStatus $1 True }

Visibility : private { Private }
           | public  { Public }

SizeDecl : '<<' list(id,',') '>>' { SizeDecl $2 }

VarDecl : list1(VarId,',') ':' TypeExpr maybe(DefaultDecl) maybe(LastDecl) { VarDecl $1 $3 $4 $5 }

VarId : bool(clock) bool(probe) id { VarId $3 $1 $2 }

DefaultDecl : default '=' Expr { $3 }

LastDecl : last '=' Expr { $3 }

TypeExpr : bool                        { TypeBool }
         | int                         { TypeInt }
         | real                        { TypeReal }
         | char                        { TypeChar }
         | TypeExpr '^' Expr           { TypePower $1 $3 }
         | Path                        { TypePath $1 }
         | name                        { TypeVar $1 }
         | '{' list1(RecordEl,',') '}' { TypeRecord $2 }

RecordEl : id ':' TypeExpr { ($1,$3) }

Equation : LHS '=' Expr                      { SimpleEquation $1 $3 }
         | AssertType id ':' Expr            { AssertEquation $1 $2 $4 }
         | emit EmissionBody                 { EmitEquation $2 }
         | StateMachine Return               { let (xs,e) = $2 in StateEquation $1 xs e }
         | activate maybe(id) IfBlock Return { let (xs,e) = $4 in ClockedEquation $2 (Left $3) xs e }

IfBlock : if Expr then IfBlockOrDataDef else IfBlockOrDataDef { IfBlock $2 $4 $6 }

IfBlockOrDataDef : DataDef { Left $1 }
                 | IfBlock { Right $1 }
            
AssertType : assume    { Assume }
           | guarantee { Guarantee }

LHS : '(' ')'          { [] }
    | list1(LHSId,',') { $1 }

LHSId : id { case $1 of { "_" -> Bottom ; _ -> Named $1 } }

Expr : Path                                                   { IdExpr $1 }
     | name                                                   { NameExpr $1 }
     | last name                                              { LastExpr $2 }
     | const_int                                              { ConstIntExpr $1 }
     | true                                                   { ConstBoolExpr True }
     | false                                                  { ConstBoolExpr False }
     | const_float                                            { ConstFloatExpr $1 }
     | '(' const_int ':' name ')'                             { ConstPolyIntExpr $2 $4 }
     | '(' list(Expr,',') ')'                                 { ListExpr $2 }
     | '[' list(Expr,',') ']'                                 { ArrayExpr $2 }
     | if Expr then Expr else Expr                            { IfExpr $2 $4 $6 }
     | Expr '+' Expr                                          { BinaryExpr BinPlus $1 $3 }
     | Expr '-' Expr                                          { BinaryExpr BinMinus $1 $3 }
     | Expr '*' Expr                                          { BinaryExpr BinTimes $1 $3 }
     | Expr '/' Expr                                          { BinaryExpr BinDiv $1 $3 }
     | Expr mod Expr                                          { BinaryExpr BinMod $1 $3 }
     | Expr div Expr                                          { BinaryExpr BinRDiv $1 $3 }
     | Expr '=' Expr                                          { BinaryExpr BinEquals $1 $3 }
     | Expr '<>' Expr                                         { BinaryExpr BinDifferent $1 $3 }
     | Expr and Expr                                          { BinaryExpr BinAnd $1 $3 } 
     | Expr or Expr                                           { BinaryExpr BinOr $1 $3 }
     | Expr xor Expr                                          { BinaryExpr BinXor $1 $3 }
     | Expr '->' Expr                                         { BinaryExpr BinAfter $1 $3 }
     | Expr '<' Expr                                          { BinaryExpr BinLesser $1 $3 }
     | Expr '>' Expr                                          { BinaryExpr BinGreater $1 $3 }
     | Expr '<=' Expr                                         { BinaryExpr BinLessEq $1 $3 }
     | Expr '>=' Expr                                         { BinaryExpr BinGreaterEq $1 $3 }
     | Expr '^' Expr                                          { BinaryExpr BinPower $1 $3 }
     | not Expr                                               { UnaryExpr UnNot $2 }
     | pre Expr                                               { UnaryExpr UnPre $2 }
     | '-' Expr                                               { UnaryExpr UnNeg $2 }
     | int Expr                                               { UnaryExpr UnCastInt $2 }
     | real Expr                                              { UnaryExpr UnCastReal $2 }
     | Operator '(' list(Expr,',') ')'                        { ApplyExpr $1 $3 }
     | fby '(' list(Expr,',') ';' Expr ';' list(Expr,',') ')' { FBYExpr $3 $5 $7 }
     | reverse Expr                                           { ReverseExpr $2 }
     | transpose '(' Expr ';' const_int ';' const_int ')'     { TransposeExpr $3 $5 $7 }
     | '(' case Expr of many1(CaseExpr) ')'                   { CaseExpr $3 $5 }
     | '(' Expr '.' '[' Expr ']' Indices default Expr ')'     { DefaultIndexExpr $2 ($5:$7) $9 }
     | Expr '[' Expr ']'                                      { IndexExpr $1 $3 }
     | Expr '[' Expr '..' Expr ']'                            { StaticProjectionExpr $1 $3 $5 }
     | Expr '@' Expr                                          { AppendExpr $1 $3 }
     | Expr times Expr                                        { TimesExpr $1 $3 }

Indices : '[' Expr ']' Indices { $2:$4 }
        |                      { [] }

CaseExpr : '|' Pattern ':' Expr { ($2,$4) }

ActivateCondition : ClockExpr                 { ActivateClock $1 }
                  | Expr default Expr         { ActivateDefault $1 $3 }
                  | Expr initial default Expr { ActivateInitialDefault $1 $4 }

Path : list1(id,'::') { Path $1 }

SignalBlock : sig list1(id,',') ';' { $2 }

SignalBlockOpt : SignalBlock { $1 }
               |             { [] }

LocalBlock : var many(fby(VarDecl,';')) { $2 }
           
LocalBlockOpt : LocalBlock { $1 }
              |            { [] }

Operator : Prefix                                            { PrefixOp $1 }
         | '(' Prefix '<<' list(Expr,',') '>>' ')'           { PrefixParamOp $2 $4 }
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

StateMachine : automaton maybe(id) many1(StateDecl) { StateMachine $2 $3 }

EmissionBody : name maybe(PostfixIfExpr)                    { EmissionBody [$1] $2 }
             | '(' list1(name,',') ')' maybe(PostfixIfExpr) { EmissionBody $2 $4 }

PostfixIfExpr : if Expr { $2 }

Actions : do '{' list1(ActionBody,';') '}' { ActionEmission $3 }
        | do DataDef                       { ActionDef $2 }

ActionBody : bool(emit) EmissionBody { ($1,$2) }

Transition : if Expr maybe(Actions) Fork { Transition $2 $3 $4 }

Fork : TargetType id                                     { TargetFork $1 $2 }
     | if Expr maybe(Actions) Fork ElifFork ElseFork end { ConditionalFork (($2,$3,$4):$5) $6 }
          
ElifFork : elsif Expr maybe(Actions) Fork ElifFork { ($2,$3,$4):$5 }
         |                                         { [] }

ElseFork : else maybe(Actions) Fork { Just ($2,$3) }
         |                          { Nothing }
          
TargetType : restart { Restart }
           | resume  { Resume }

StateDecl : bool(initial) bool(final) state id StateUnless DataDef StateUntil { State $1 $2 $4 $6 $5 (fst $7) (snd $7) }

StateUnless : unless many1(fby(Transition,';')) { $2 }
            |                                   { [] }

StateUntil : until many(fby(Transition,';')) maybe(StateSynchro) { ($2,$3) }
           |                                                     { ([],Nothing) }

StateSynchro : synchro maybe(Actions) Fork ';' { ($2,$3) }

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
