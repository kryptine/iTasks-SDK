implementation module TaskContainer
 
import iTasks


validTaskVal :: Dynamic -> Bool
validTaskVal (_ :: DV0 a)		= True
validTaskVal (_ :: DV1 t a)		= True
validTaskVal (_ :: DV2 t a b)	= True
validTaskVal _					= False

validTask :: Dynamic -> Bool
validTask 	(_ :: DT a)			= True
validTask _						= False

validTaskFun :: Dynamic -> Bool
validTaskFun (_ :: DF0 a b) 							= True
validTaskFun (_ :: DF0 a b | iTask a) 					= True
validTaskFun (_ :: DF0 a b | iTask a & iTask b) 		= True
validTaskFun (_ :: DF0 (t a) b | iTask a) 				= True
validTaskFun (_ :: DF0 (t a b) c| iTask a & iTask b) 	= True

validTaskFun _ 								= False

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

import Debug

toStringDynamic :: !Dynamic -> ([String], String)
toStringDynamic d = (v, t)
where
	v = case d of (x :: a) -> debugShowWithOptions [DebugTerminator "", DebugMaxChars 79] x

	t = removeForAll (typeCodeOfDynamic d)
	where
		removeForAll (TypeScheme _ t) = toString t
		removeForAll t = toString t
