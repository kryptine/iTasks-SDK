definition module TaskContainer
 
import iTasks

:: T  a b	= T  !a & iTask b

showDynType 	:: !Dynamic -> String
showDynVal  	:: !Dynamic -> String
showDynValType 	:: !String !Dynamic -> String 

typeErrorMess 	:: !String !Dynamic -> String 
typeErrorMess2 	:: !String !Dynamic !Dynamic -> String 
