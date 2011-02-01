implementation module GinTypes

import GenEq
import iTasks
import PPrint

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool
typeIsDefined GUndefinedTypeExpression	= False
typeIsDefined (GList e)					= typeIsDefined e
typeIsDefined (GTuple es)				= all typeIsDefined es
typeIsDefined (GConstructor t)			= True
typeIsDefined (GTypeApplication a b)	= typeIsDefined a && typeIsDefined b
typeIsDefined (GTypeVariable v)			= True

printGTypeExpression :: GTypeExpression -> Doc
printGTypeExpression GUndefinedTypeExpression	= text "<<undefined type expression>>"
printGTypeExpression (GList e)					= brackets (printGTypeExpression e)
printGTypeExpression (GTuple es)				= tupled (map printGTypeExpression es)
printGTypeExpression (GConstructor t)			= text t
printGTypeExpression (GTypeApplication a b)		= parens (printGTypeExpression a <+> printGTypeExpression b)
printGTypeExpression (GTypeVariable v)			= text v

printGTypeDefinition :: GTypeDefinition -> Doc
printGTypeDefinition gt = text "::" <+> text gt.GTypeDefinition.name
                          <+> printGTypeRhs gt.GTypeDefinition.rhs

printGTypeRhs :: GTypeRhs -> Doc                          
printGTypeRhs (GAlgebraicTypeRhs conss) = text "=" <+> hsep (punctuate (text "|") (map printGDataConstructor conss))
printGTypeRhs (GRecordTypeRhs fields)   = text "=" <+> braces (hsep ((punctuate comma (map printGRecordField fields))))
printGTypeRhs (GSynonymTypeRhs exp)     = text ":==" <+> printGTypeExpression exp
printGTypeRhs GAbstractTypeRhs          = empty

printGDataConstructor :: GDataConstructor -> Doc
printGDataConstructor cons = text cons.GDataConstructor.name 
                         <+> hsep (map printGTypeExpression cons.GDataConstructor.arguments)

printGRecordField :: GRecordField -> Doc
printGRecordField field = text field.GRecordField.name
                          <+> text "::" <+> printGTypeExpression field.GRecordField.type

gTask :: GTypeExpression -> GTypeExpression
gTask e = GTypeApplication (GConstructor "Task") e

gVoid :: GTypeExpression
gVoid = GConstructor "Void"
