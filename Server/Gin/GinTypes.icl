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
typeIsDefined (GTypeApplication a b)	= typeIsDefined a && typeIsDefined b
typeIsDefined (GTypeVariable v)			= True

printGTypeExpression :: GTypeExpression -> a | Printer a
printGTypeExpression GUndefinedTypeExpression	= text "<<undefined type expression>>"
printGTypeExpression (GList e)					= brackets (printGTypeExpression e)
printGTypeExpression (GTuple es)				= tupled (map printGTypeExpression es)
printGTypeExpression (GConstructor t)			= text t
printGTypeExpression (GTypeApplication a b)		= parens (printGTypeExpression a </> printGTypeExpression b)
printGTypeExpression (GTypeVariable v)			= text v

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

gTask :: GTypeExpression -> GTypeExpression
gTask e = GTypeApplication (GConstructor "Task") e

gVoid :: GTypeExpression
gVoid = GConstructor "Void"
