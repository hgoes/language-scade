{-# LANGUAGE DeriveDataTypeable #-}
module Language.Scade.Syntax where

import Data.Data
import Data.Typeable

data Declaration = OpenDecl Path
                 | TypeBlock [TypeDecl]
                 | PackageDecl (Maybe Visibility) String [Declaration]
                 | UserOpDecl
                   { userOpKind :: UserOpKind
                   , userOpImported :: Bool
                   , userOpInterface :: InterfaceStatus
                   , userOpName :: String
                   , userOpSize :: Maybe SizeDecl 
                   , userOpParams :: [VarDecl]
                   , userOpReturns :: [VarDecl]
                   , userOpNumerics :: [String]
                   , userOpContent :: DataDef
                   }
                 | ConstBlock [ConstDecl]
                 deriving (Show, Data, Typeable)

data UserOpKind = Function
                | Node
                deriving (Show, Data, Typeable)

data ConstDecl = ConstDecl InterfaceStatus String TypeExpr (Maybe Expr)
               deriving (Show, Data, Typeable)

data TypeDecl = TypeDecl InterfaceStatus String (Maybe TypeExpr)
              deriving (Show, Data, Typeable)

data InterfaceStatus = InterfaceStatus
                       { visibility :: Maybe Visibility
                       , external :: Bool
                       } deriving (Show, Data, Typeable)

data Visibility = Private
                | Public
                deriving (Show, Data, Typeable)

newtype SizeDecl = SizeDecl [String] deriving (Show, Data, Typeable)

data VarDecl = VarDecl
                 { varNames :: [VarId]
                 , varType :: TypeExpr 
                 , varDefault :: Maybe Expr
                 , varLast :: Maybe Expr
                 }
             deriving (Show,Eq,Data,Typeable)

data VarId = VarId
             { name :: String
             , is_clock :: Bool
             , is_probe :: Bool
             } deriving (Show,Eq,Data,Typeable)

data TypeExpr = TypeBool
              | TypeInt
              | TypeReal
              | TypeChar
              | TypePower TypeExpr Expr
              | TypePath Path
              | TypeVar String
              | TypeRecord [(String,TypeExpr)]
              | TypeEnum [String]
              deriving (Show,Eq,Data,Typeable) -- missing: a whole shitload

data DataDef = DataDef
               { dataSignals :: [String]
               , dataLocals :: [VarDecl]
               , dataEquations :: [Equation]
               } deriving (Show,Eq,Data,Typeable)

data Equation = SimpleEquation [LHSId] Expr
              | AssertEquation AssertType String Expr
              | EmitEquation EmissionBody
              | StateEquation StateMachine [String] Bool
              | ClockedEquation (Maybe String) (Either IfBlock MatchBlock) [String] Bool
              deriving (Show,Eq,Data,Typeable)

data IfBlock = IfBlock Expr (Either DataDef IfBlock) (Either DataDef IfBlock)
             deriving (Show,Eq,Data,Typeable)

data MatchBlock = MatchBlock Expr [(Pattern,DataDef)]
                deriving (Show,Eq,Data,Typeable)

data LHSId = Named String
           | Bottom
           deriving (Show,Eq,Data,Typeable)

newtype Path = Path [String] deriving (Show,Eq,Ord,Data,Typeable)

data Expr = IdExpr Path
          | NameExpr String
          | LastExpr String
          | ConstIntExpr Integer
          | ConstBoolExpr Bool
          | ConstFloatExpr Double
          | ConstPolyIntExpr Integer String
          | BinaryExpr BinOp Expr Expr
          | UnaryExpr UnaryOp Expr
          | ListExpr [Expr]
          | ArrayExpr [Expr]
          | IfExpr Expr Expr Expr
          | ApplyExpr Operator [Expr]
          | FBYExpr [Expr] Expr [Expr]
          | ReverseExpr Expr
          | CaseExpr Expr [(Pattern,Expr)]
          | IndexExpr Expr Expr
          | DefaultIndexExpr Expr [Expr] Expr
          | StaticProjectionExpr Expr Expr Expr
          | AppendExpr Expr Expr
          | TransposeExpr Expr Integer Integer
          | TimesExpr Expr Expr
          deriving (Show,Eq,Data,Typeable)

data ActivateCondition = ActivateClock ClockExpr
                       | ActivateDefault Expr Expr
                       | ActivateInitialDefault Expr Expr
                       deriving (Show,Eq,Data,Typeable)

data UnaryOp = UnNot
             | UnPre
             | UnNeg
             | UnCastInt
             | UnCastReal
             deriving (Show,Eq,Data,Typeable)

data BinOp = BinPlus
           | BinMinus
           | BinTimes
           | BinDiv
           | BinMod
           | BinRDiv
           | BinEquals
           | BinDifferent
           | BinLesser
           | BinGreater
           | BinLessEq
           | BinGreaterEq
           | BinAfter
           | BinAnd
           | BinOr
           | BinXor
           | BinPower
           deriving (Show,Eq,Data,Typeable)

data Operator = PrefixOp Prefix
              | PrefixParamOp Prefix [Expr]
              | IteratorOp Iterator Operator Expr
              | ActivateOp Operator ActivateCondition
              | Flatten Path
              | Make Path
              | RestartOp Operator Expr
              | MapWOp Operator Expr Expr Expr
              | MapWiOp Operator Expr Expr Expr
              | FoldWOp Operator Expr Expr
              | FoldWiOp Operator Expr Expr
              deriving (Show,Eq,Data,Typeable)

data Prefix = PrefixPath Path
            | PrefixBinOp BinOp
            deriving (Show,Eq,Data,Typeable)

data Iterator = ItMap
              | ItFold
              | ItMapFold
              | ItMapI
              | ItFoldI
              deriving (Show,Eq,Data,Typeable)

data StateMachine = StateMachine (Maybe String) [State] deriving (Show,Eq,Data,Typeable)

data State = State
             { stateInitial :: Bool
             , stateFinal :: Bool
             , stateName :: String
             , stateData :: DataDef
             , stateUnless :: [Transition]
             , stateUntil :: [Transition]
             , stateSynchro :: Maybe (Maybe Actions,Fork)
             } deriving (Show,Eq,Data,Typeable)

data Transition = Transition Expr (Maybe Actions) Fork deriving (Show,Eq,Data,Typeable)

data EmissionBody = EmissionBody [String] (Maybe Expr) deriving (Show,Eq,Data,Typeable)

data Actions = ActionEmission [(Bool,EmissionBody)]
             | ActionDef DataDef
             deriving (Show,Eq,Data,Typeable)

data Fork = TargetFork TargetType String
          | ConditionalFork [(Expr,Maybe Actions,Fork)] (Maybe (Maybe Actions,Fork))
          deriving (Show,Eq,Data,Typeable)

data TargetType = Restart
                | Resume
                deriving (Show,Eq,Data,Typeable)

data ClockExpr = ClockId String
               | ClockNotId String
               | ClockMatch String Pattern 
               deriving (Show,Eq,Data,Typeable)

data Pattern = PatPath Path
             | PatChar Char
             | PatInt Integer
             | PatBool Bool
             | PatBottom
             deriving (Show,Eq,Data,Typeable)

data AssertType = Assume
                | Guarantee
                deriving (Show,Eq,Data,Typeable)