implementation module GinTypes

import StdEnv
import GenEq, GenPrint, GenParse, GenVisualize, GenUpdate, GenMerge
import JSON
import PPrint

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GFormalParameter
derive gMerge           GTypeExpression, GTypeDefinition, GFormalParameter
derive gEq              GTypeExpression, GTypeDefinition, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool
typeIsDefined GUndefinedTypeExpression = False
typeIsDefined (GBasicTypeExpression _) = True
typeIsDefined (GAbstractTypeExpression _) = True
typeIsDefined (GList e) = typeIsDefined e
typeIsDefined (GTuple es) = all typeIsDefined es
typeIsDefined (GArray es) = typeIsDefined es
typeIsDefined (GConstructor t) = True
typeIsDefined (GTypeApplication a b) = typeIsDefined a && typeIsDefined b
typeIsDefined (GTypeVariable v) = True

printGTypeExpression :: GTypeExpression -> Doc
printGTypeExpression GUndefinedTypeExpression = text "<<undefined type expression>>"
printGTypeExpression (GBasicTypeExpression t) = text t
printGTypeExpression (GAbstractTypeExpression id) = text id
printGTypeExpression (GList e) = brackets (printGTypeExpression e)
printGTypeExpression (GTuple es) = tupled (map printGTypeExpression es)
printGTypeExpression (GArray es) = braces (printGTypeExpression es)
printGTypeExpression (GConstructor t) = text t
printGTypeExpression (GTypeApplication a b) = parens (printGTypeExpression a <+> printGTypeExpression b)
printGTypeExpression (GTypeVariable v) = text v

printGTypeDefinition :: GTypeDefinition -> Doc
printGTypeDefinition gt = text "::" <+> text gt.GTypeDefinition.name <+> text "=" 
                          <+> printGTypeExpression gt.GTypeDefinition.expression

gTask :: GTypeExpression -> GTypeExpression
gTask e = GTypeApplication (GAbstractTypeExpression "Task") e

gVoid :: GTypeExpression
gVoid = GAbstractTypeExpression "Void"
