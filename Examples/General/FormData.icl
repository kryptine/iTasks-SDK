implementation module FormData
 
import 	iTasks, TaskContainer, GoogleMaps
from 	StdFunc import o
from	StdMisc import abort
				
derive class iTask	Form, FormShape, Tup
derive bimap		Maybe, (,)

:: Form			= 	{ formShape :: ![FormShape]
			  		, formDyn	:: !Dynamic
			  		}
:: FormShape 	= 	Integer 	
				| 	Real 		
				| 	String 	
				| 	Bool 		
				| 	Tuple  	!(!FormShape, !FormShape)
				| 	List 	!FormShape
				|	Hide	!FormShape
				| 	Option 	!FormShape
				| 	Labeled !(!String, !FormShape)
				| 	Notes 	
				| 	Date 		
				| 	Time 		
				| 	Document 	
				| 	GoogleMap

:: Tup a b = Tup a b

emptyForm :: Form
emptyForm 		= 	{ formShape = []
					, formDyn = dynamic "Form empty" :: String
					}

formShapeToFormDyn :: [FormShape] -> Task Dynamic
formShapeToFormDyn bs = convertFormShapes bs >>= return o tupling
where
	tupling [] 		= dynamic DV0 Void 
	tupling [d]		= d
	tupling [d:ds]	= case (d, tupling ds) of 
							(DV0 d1 :: DV0 a, DV0 d2 :: DV0 b) -> dynamic DV0 (Tup d1 d2) 
							_ -> abort "Fatal Error in shapeToForm !!!"

	convertFormShapes :: [FormShape] -> Task [Dynamic]
	convertFormShapes [] 		= return []
	convertFormShapes [b:bs] 	= convert b >>= \d -> convertFormShapes bs >>= \ds -> return [d:ds] 
	where
		convert :: FormShape -> Task Dynamic
		convert	Integer					= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Int)	
		convert	Real					= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Real)	
		convert	String					= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 String)	
		convert	Bool					= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Bool)	
		convert	(Tuple (b1, b2))		= convert b1 >>= \db1 -> convert b2 >>= \db2 -> returnTuple db1 db2	
		where
			returnTuple (DV0 t1 :: DV0 a) (DV0 t2 :: DV0 b) 
										= return (dynamic DV0 (t1,t2) :: (DV0 (a,b)))
		convert (List b)				= convert b >>= \dl -> returnList dl
		where
			returnList (DV0 v :: DV0 a)	= return (dynamic DV0 [] :: DV0 [a])
		convert (Hide b)				= convert b >>= returnHidden
		where
			returnHidden (DV0 nb :: DV0 a)= return (dynamic DV0 (Hidden nb) :: DV0 (Hidden a))
		convert (Option b)				= convert b >>= \db -> returnOption db
		where
			returnOption (DV0 v :: DV0 a) = return (dynamic DV0 Nothing :: DV0 (Maybe a))
		convert (Labeled (s, b))		= convert b >>= \nb ->	returnLabel s nb
		where
				returnLabel s (DV0 v :: DV0 a) 
									= return (dynamic DV0 (HtmlDisplay s,v) :: DV0 (HtmlDisplay String,a))
		convert	Notes				= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Note)	
		convert	Date				= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Date)	
		convert	Time				= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Time)	
		convert	Document			= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 Document)	
		convert	GoogleMap			= getDefaultValue >>= \v -> return (dynamic DV0 v :: DV0 GoogleMap)	
		convert _					= abort "Fatal Error in Convert !!!"


// ****************************


