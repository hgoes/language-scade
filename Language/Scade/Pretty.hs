module Language.Scade.Pretty where

import Language.Scade.Syntax
import Text.PrettyPrint

prettyScade :: [Declaration] -> Doc
prettyScade = vcat . map prettyDecl

prettyDecl :: Declaration -> Doc
prettyDecl (OpenDecl path) = text "open" <+> prettyPath path
prettyDecl decl@(UserOpDecl {})
  = kind <+> imported <+> iface <+> name <+> sized <+> params <+> text "returns" <+> returns $+$ body
  where
    kind = case userOpKind decl of
      Node -> text "node"
      Function -> text "function"
    imported = if userOpImported decl
               then text "imported"
               else empty
    iface = prettyInterfaceStatus $ userOpInterface decl
    name = text $ userOpName decl
    sized = mb prettySizeDecl $ userOpSize decl
    renderArgs = parens . hsep . punctuate (text ";") . map prettyVarDecl
    params = renderArgs $ userOpParams decl
    returns = renderArgs $ userOpReturns decl
    body = prettyDataDef $ userOpContent decl
prettyDecl (PackageDecl vis name decls) = text "package" <+> visib <+> text name $+$ nest 2 (prettyScade decls)
  where
    visib = mb prettyVisibility vis
prettyDecl _ = empty

prettyPath :: Path -> Doc
prettyPath (Path xs) = hsep $ punctuate (text "::") $ map text xs

prettyInterfaceStatus :: InterfaceStatus -> Doc
prettyInterfaceStatus iface = mb prettyVisibility (visibility iface) <+> (if external iface then text "external" else empty)

prettyVisibility :: Visibility -> Doc
prettyVisibility visib = case visib of
  Public -> text "public"
  Private -> text "private"

mb :: (a -> Doc) -> Maybe a -> Doc
mb f Nothing = empty
mb f (Just x) = f x

prettySizeDecl :: SizeDecl -> Doc
prettySizeDecl (SizeDecl names) = text "<<" <> hsep (punctuate (text ",") (map text names)) <> text ">>"

prettyVarDecl :: VarDecl -> Doc
prettyVarDecl vdecl = names <+> text ":" <+> tp
  where
    names = hsep $ punctuate (text ",") $ map prettyVarId (varNames vdecl)
    tp = prettyTypeExpr $ varType vdecl

prettyVarId :: VarId -> Doc
prettyVarId vid = (if is_clock vid then text "clock" else empty)
                  <+> (if is_probe vid then text "probe" else empty)
                  <+> (text $ name vid)

prettyTypeExpr :: TypeExpr -> Doc
prettyTypeExpr TypeBool = text "bool"
prettyTypeExpr TypeInt = text "int"
prettyTypeExpr TypeReal = text "real"
prettyTypeExpr TypeChar = text "char"
prettyTypeExpr (TypePower tp ex) = (prettyTypeExpr tp) <> text "^" <> prettyExpr 2 ex
prettyTypeExpr (TypePath path) = prettyPath path
prettyTypeExpr (TypeVar v) = text "'" <> text v
prettyTypeExpr (TypeRecord xs) = braces $ commaList $ map (\(n,tp) -> text n <> text ":" <> prettyTypeExpr tp) xs

prettyExpr :: Int -> Expr -> Doc
prettyExpr _ (IdExpr path) = prettyPath path
prettyExpr _ (ConstIntExpr n) = integer n
prettyExpr p (IfExpr e1 e2 e3) = precedence 14 p $ 
                                 text "if" <+> prettyExpr 14 e1 <+>
                                 text "then" <+> prettyExpr 14 e2 <+>
                                 text "else" <+> prettyExpr 14 e3
prettyExpr p (BinaryExpr op e1 e2) = precedence prec p $
                                     prettyExpr prec e1 <+>
                                     sym <+>
                                     prettyExpr prec e2
  where
    (prec,sym) = opAttrs op
--prettyExpr p (ApplyExpr op args) = 
prettyExpr _ (FBYExpr e1 e2 e3) = text "fby" <> (parens $
                                  commaList (map (prettyExpr 15) e1) <>
                                  text ";" <+>
                                  prettyExpr 15 e2 <>
                                  text ";" <+>
                                  commaList (map (prettyExpr 15) e3))
prettyExpr _ _ = text "_"

opAttrs :: BinOp -> (Int,Doc)
opAttrs BinPlus = (9,text "+")
opAttrs BinMinus = (9,text "-")
opAttrs BinTimes = (8,text "*")
opAttrs BinDiv = (8,text "/")
opAttrs BinEquals = (10,text "=")
opAttrs BinDifferent = (10,text "<>")
opAttrs _ = (0,text "_")

commaList :: [Doc] -> Doc
commaList = hsep . punctuate (text ",")

precedence :: Int -> Int -> Doc -> Doc
precedence cur ctx doc
  | cur > ctx = parens doc
  | otherwise = doc

prettyDataDef :: DataDef -> Doc
prettyDataDef def = signals $+$ vars $+$ eqs
  where
    signals = case dataSignals def of
      [] -> empty
      xs -> text "sig" <+> hsep (punctuate (text ",") (map text xs)) <+> text ";"
    vars = case dataLocals def of
      [] -> empty
      xs -> text "var" $+$ nest 2 (vcat $ map (\x -> prettyVarDecl x <> text ";") xs)
    eqs = case dataEquations def of
      [] -> empty
      xs -> text "let" $+$ (nest 2 $ vcat $ map (\eq -> prettyEquation eq <> text ";") xs) $+$ text "tel"

prettyEquation :: Equation -> Doc
prettyEquation (SimpleEquation ids expr) = lhs <+> text "=" <+> rhs
  where
    lhs = hsep $ punctuate (text ",") $ map prettyLHSId ids
    rhs = prettyExpr 15 expr
prettyEquation _ = text "<eq>"

prettyLHSId :: LHSId -> Doc
prettyLHSId (Named str) = text str
prettyLHSId Bottom = text "_"