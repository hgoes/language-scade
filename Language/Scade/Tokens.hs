module Language.Scade.Tokens where

data Token = Key Keyword
           | Identifier String
           | Name String
           | Bracket BracketType Bool
           | Colon
           | DoubleColon
           | Semicolon
           | Comma
           | Dot
           | DotDot
           | Op Operator
           | PrefixOp' Operator
           | ConstInt Integer
           | Char Char
           | ConstFloat Double
           deriving Show

data Keyword = KeyAbstract | KeyActivate | KeyAnd | KeyAssume | KeyAutomaton
             | KeyBool
             | KeyCase | KeyChar | KeyClock | KeyConst
             | KeyDefault | KeyDiv | KeyDo
             | KeyElse | KeyElsif | KeyEmit | KeyEnd | KeyEnum | KeyEvery | KeyExternal
             | KeyFalse | KeyFBY | KeyFinal | KeyFlatten | KeyFold | KeyFoldi | KeyFoldw | KeyFoldwi | KeyFunction
             | KeyGuarantee | KeyGroup
             | KeyIf | KeyImported | KeyInitial | KeyInt | KeyIs
             | KeyLast | KeyLet
             | KeyMake | KeyMap | KeyMapFold | KeyMapi | KeyMapw | KeyMapwi | KeyMatch | KeyMerge | KeyMod
             | KeyNode | KeyNot | KeyNumeric
             | KeyOf | KeyOnReset | KeyOpen | KeyOr
             | KeyPackage | KeyParameter | KeyPre | KeyPrivate | KeyProbe | KeyPublic
             | KeyReal | KeyRestart | KeyResume | KeyReturns | KeyReverse
             | KeySensor | KeySig | KeySpecialize | KeyState | KeySynchro
             | KeyTel | KeyThen | KeyTimes | KeyTranspose | KeyTrue | KeyType
             | KeyUnless | KeyUntil
             | KeyVar
             | KeyWhen | KeyWhere | KeyWith
             | KeyXor
             deriving Show

data BracketType = Parentheses
                 | Square
                 | Curly
                 | Params
                 deriving Show

data Operator = OpPlus
              | OpMinus
              | OpTimes
              | OpDiv
              | OpMod
              | OpEqual
              | OpDifferent
              | OpLess
              | OpGreater
              | OpLessEq
              | OpGreaterEq
              | OpAt
              | OpPower
              | OpTo
              | OpOr
              | OpAnd
              deriving Show