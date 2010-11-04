definition module TaskContainer
 
import iTasks

:: DT a			= DT !(Task a)	& iTask a

:: DV0 a		= DV0 !a		& iTask a
:: DV1 t a		= DV1 !(t a)	& iTask a
:: DV2 t a b	= DV2 !(t a b)	& iTask a & iTask b


:: DF0 a b		= DF0 (a		-> Task b)	& iTask b	// comp crashed als ik cr weglaat

validTaskVal 	:: Dynamic -> Bool
validTask 		:: Dynamic -> Bool
validTaskFun 	:: Dynamic -> Bool

showDynType 	:: !Dynamic -> String
showDynVal  	:: !Dynamic -> String
showDynValType 	:: !String !Dynamic -> String 

typeErrorMess 	:: !String !Dynamic -> String 
typeErrorMess2 	:: !String !Dynamic !Dynamic -> String 


toStringDynamic :: !Dynamic -> ([String], String)
