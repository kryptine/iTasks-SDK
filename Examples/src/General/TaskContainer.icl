implementation module TaskContainer
 
import iTasks
from	EstherBackend import toStringDynamic

:: DynTaskVal a		= DynTaskVal !a				& iTask a
:: DynTask a 		= DynTask    !(Task a) 		& iTask a
:: DynTaskFun a b	= DynTaskFun !(a -> Task b) & iTask b

validTaskVal :: Dynamic -> Bool
validTaskVal (v :: DynTaskVal a)	= True
validTaskVal _						= False

validTask :: Dynamic -> Bool
validTask 	(t :: DynTask a)		= True
validTask _							= False

validTaskFun :: Dynamic -> Bool
validTaskFun (f :: DynTaskFun a b) 	= True

validTaskFun (f :: A.a: 		a -> Task a 		| iTask a) 						= True

validTaskFun (f :: A.a: 		a -> Task Void) 									= True
validTaskFun (f :: A.a: 		a -> Task Int) 										= True
validTaskFun (f :: A.a: 		a -> Task Real) 									= True
validTaskFun (f :: A.a: 		a -> Task Bool) 									= True
validTaskFun (f :: A.a: 		a -> Task String) 									= True

validTaskFun (f :: A.a: 		a -> Task Void		| iTask a) 						= True
validTaskFun (f :: A.a: 		a -> Task Int		| iTask a) 						= True
validTaskFun (f :: A.a: 		a -> Task Real		| iTask a) 						= True
validTaskFun (f :: A.a: 		a -> Task Bool		| iTask a) 						= True
validTaskFun (f :: A.a: 		a -> Task String	| iTask a) 						= True

validTaskFun _ 						= False

showDyn :: Dynamic -> (String,String)
showDyn dyn	
# (v,t) =  toStringDynamic dyn 
=	case dyn of
		(fun :: a -> b) -> ("<function> ",t) 
		_				-> (foldr (+++) "" v,t)

showDynType :: !Dynamic -> String
showDynType  dyn = snd (showDyn dyn)

showDynVal  :: !Dynamic -> String
showDynVal dyn = fst (showDyn dyn)

showDynValType :: !String !Dynamic -> String 
showDynValType s d 	= let (v,t) = showDyn d in s +++ ", " +++ v +++ "::" +++ t

typeErrorMess :: !String !Dynamic -> String 
typeErrorMess s (mes :: String) 	= s +++ ": Type Error Reported: " +++ mes
typeErrorMess s d1 					= s +++ ": Type Error: " +++ showDynType d1

typeErrorMess2 :: !String !Dynamic !Dynamic -> String 
typeErrorMess2 s d1 d2 = s +++ ": Cannot Unify: " +++ showDynType d1 +++ " with "  +++ showDynType d2

