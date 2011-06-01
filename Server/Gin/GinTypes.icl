implementation module GinTypes

import StdBool
import StdList
import GenEq
import Maybe

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, ::Visualization
from iTasks import class iTask, generic gVisualize, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

import GinPrinter

derive bimap (,)
derive bimap Maybe

derive class iTask      GTypeExpression, GTypeDefinition, GTypeRhs, GDataConstructor, GRecordField, GFormalParameter

typeIsDefined :: GTypeExpression -> Bool
typeIsDefined GUndefinedTypeExpression	= False
typeIsDefined (GList e)					= typeIsDefined e
typeIsDefined (GTuple es)				= all typeIsDefined es
typeIsDefined (GConstructor t)			= True
typeIsDefined (GTypeApplication es)		= all typeIsDefined es
typeIsDefined (GTypeVariable v)			= True

printGTypeExpression :: GTypeExpression -> a | Printer a
printGTypeExpression (GConstructor t)			= text t
printGTypeExpression (GList e)					= brackets (printGTypeExpression e)
printGTypeExpression (GTuple es)				= tupled (map printGTypeExpression es)
printGTypeExpression (GTypeApplication es)		= parens (fillSep (punctuate space (map printGTypeExpression es)))
printGTypeExpression (GTypeVariable v)			= text v
printGTypeExpression (GFunction e1 e2)			= parens (printGTypeExpression e1 </> text "->" </> printGTypeExpression e2)
printGTypeExpression GUndefinedTypeExpression	= text "<<undefined type expression>>"

printGTypeDefinition :: GTypeDefinition -> a | Printer a
printGTypeDefinition gt = def (	text "::" </> text gt.GTypeDefinition.name
                          		</> printGTypeRhs gt.GTypeDefinition.rhs
                          	  )

printGTypeRhs :: GTypeRhs -> a | Printer a                          
printGTypeRhs (GAlgebraicTypeRhs conss) = text "=" </> fillSep (punctuate (text "|") (map printGDataConstructor conss))
printGTypeRhs (GRecordTypeRhs fields)   = text "=" </> braces (fillSep ((punctuate comma (map printGRecordField fields))))
printGTypeRhs (GSynonymTypeRhs exp)     = text ":==" </> printGTypeExpression exp
printGTypeRhs GAbstractTypeRhs          = empty

printGDataConstructor :: GDataConstructor -> a | Printer a
printGDataConstructor cons = text cons.GDataConstructor.name 
                         </> fillSep (map printGTypeExpression cons.GDataConstructor.arguments)

printGRecordField :: GRecordField -> a | Printer a
printGRecordField field = text field.GRecordField.name
                          </> text "::" </> printGTypeExpression field.GRecordField.type

instance toString GTypeExpression
where
	toString typeExp = prettyPrint (printGTypeExpression typeExp)

isTask :: !GTypeExpression -> Bool
isTask (GTypeApplication [GConstructor "Task", _])	= True
isTask _										 	= False

gTask :: GTypeExpression -> GTypeExpression
gTask e = GTypeApplication [GConstructor "Task", e]

gVoid :: GTypeExpression
gVoid = GConstructor "Void"
