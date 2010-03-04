definition module TaskContainer
 
import iTasks

:: DynTaskVal a		= DynTaskVal !a				& iTask a
:: DynTask a 		= DynTask    !(Task a) 		& iTask a
:: DynTaskFun a b	= DynTaskFun !(a -> Task b) & iTask b

validTaskVal 	:: Dynamic -> Bool
validTask 		:: Dynamic -> Bool
validTaskFun 	:: Dynamic -> Bool

showDynType 	:: !Dynamic -> String
showDynVal  	:: !Dynamic -> String
showDynValType 	:: !String !Dynamic -> String 

typeErrorMess 	:: !String !Dynamic -> String 
typeErrorMess2 	:: !String !Dynamic !Dynamic -> String 
