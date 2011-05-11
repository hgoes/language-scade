module Language.Scade.Syntax where

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
                 deriving Show

data UserOpKind = Function
                | Node
                deriving Show

data ConstDecl = ConstDecl InterfaceStatus String TypeExpr (Maybe Expr)
               deriving Show

data TypeDecl = TypeDecl InterfaceStatus String (Maybe (Either TypeExpr [String]))
              deriving Show

data InterfaceStatus = InterfaceStatus
                       { visibility :: Maybe Visibility
                       , external :: Bool
                       } deriving Show

data Visibility = Private
                | Public
                deriving Show

newtype SizeDecl = SizeDecl [String] deriving Show

data VarDecl = VarDecl
                 { varNames :: [VarId]
                 , varType :: TypeExpr 
                 , varDefault :: Maybe Expr
                 , varLast :: Maybe Expr
                 }
             deriving (Show,Eq)

data VarId = VarId
             { name :: String
             , is_clock :: Bool
             , is_probe :: Bool
             } deriving (Show,Eq)

data TypeExpr = TypeBool
              | TypeInt
              | TypeReal
              | TypeChar
              | TypePower TypeExpr Expr
              | TypePath Path
              | TypeVar String
              | TypeRecord [(String,TypeExpr)]
              deriving (Show,Eq) -- missing: a whole shitload

data DataDef = DataDef
               { dataSignals :: [String]
               , dataLocals :: [VarDecl]
               , dataEquations :: [Equation]
               } deriving (Show,Eq)

data Equation = SimpleEquation [LHSId] Expr
              | AssertEquation AssertType String Expr
              | EmitEquation EmissionBody
              | StateEquation StateMachine [String] Bool
              | ClockedEquation (Maybe String) (Either IfBlock MatchBlock) [String] Bool
              deriving (Show,Eq)

data IfBlock = IfBlock Expr (Either DataDef IfBlock) (Either DataDef IfBlock)
             deriving (Show,Eq)

data MatchBlock = MatchBlock Expr [(Pattern,DataDef)]
                deriving (Show,Eq)

data LHSId = Named String
           | Bottom
           deriving (Show,Eq)

newtype Path = Path [String] deriving (Show,Eq)

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
          deriving (Show,Eq)

data ActivateCondition = ActivateClock ClockExpr
                       | ActivateDefault Expr Expr
                       | ActivateInitialDefault Expr Expr
                       deriving (Show,Eq)

data UnaryOp = UnNot
             | UnPre
             | UnNeg
             | UnCastInt
             | UnCastReal
             deriving (Show,Eq)

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
           deriving (Show,Eq)

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
              deriving (Show,Eq)

data Prefix = PrefixPath Path
            | PrefixBinOp BinOp
            deriving (Show,Eq)

data Iterator = ItMap
              | ItFold
              | ItMapFold
              | ItMapI
              | ItFoldI
              deriving (Show,Eq)

data StateMachine = StateMachine (Maybe String) [State] deriving (Show,Eq)

data State = State
             { stateInitial :: Bool
             , stateFinal :: Bool
             , stateName :: String
             , stateData :: DataDef
             , stateUnless :: [Transition]
             , stateUntil :: [Transition]
             , stateSynchro :: Maybe (Maybe Actions,Fork)
             } deriving (Show,Eq)

data Transition = Transition Expr (Maybe Actions) Fork deriving (Show,Eq)

data EmissionBody = EmissionBody [String] (Maybe Expr) deriving (Show,Eq)

data Actions = ActionEmission [(Bool,EmissionBody)]
             | ActionDef DataDef
             deriving (Show,Eq)

data Fork = TargetFork TargetType String
          | ConditionalFork [(Expr,Maybe Actions,Fork)] (Maybe (Maybe Actions,Fork))
          deriving (Show,Eq)

data TargetType = Restart
                | Resume
                deriving (Show,Eq)

data ClockExpr = ClockId String
               | ClockNotId String
               | ClockMatch String Pattern 
               deriving (Show,Eq)

data Pattern = PatPath Path
             | PatChar Char
             | PatInt Integer
             | PatBool Bool
             | PatBottom
             deriving (Show,Eq)

data AssertType = Assume
                | Guarantee
                deriving (Show,Eq)