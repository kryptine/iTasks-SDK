implementation module FormData
 
import 	iTasks, TaskContainer, GeoDomain
from 	StdFunc import o
from	StdMisc import abort
				
derive gPrint 		Form, FormShape, Tup
derive gParse 		Form, FormShape, Tup
derive gUpdate 		Form, FormShape, Tup
derive gVisualize 	Form, FormShape

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


emptyForm :: Form
emptyForm 		= 	{ formShape = []
					, formDyn = dynamic "Form empty" :: String
					}

formShapeToFormDyn :: [FormShape] -> Task Dynamic
formShapeToFormDyn bs = convertFormShapes bs >>= return o tupling
where
	tupling [] 		= dynamic DynTaskVal Void 
	tupling [d]		= d
	tupling [d:ds]	= case (d, tupling ds) of 
							(DynTaskVal d1 :: DynTaskVal a, DynTaskVal d2 :: DynTaskVal b) -> dynamic DynTaskVal (Tup d1 d2) 
							_ -> abort "Fatal Error in shapeToForm !!!"

	convertFormShapes :: [FormShape] -> Task [Dynamic]
	convertFormShapes [] 		= return []
	convertFormShapes [b:bs] 	= convert b >>= \d -> convertFormShapes bs >>= \ds -> return [d:ds] 
	where
		convert :: FormShape -> Task Dynamic
		convert	Integer					= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Int)	
		convert	Real					= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Real)	
		convert	String					= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal String)	
		convert	Bool					= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Bool)	
		convert	(Tuple (b1, b2))		= convert b1 >>= \db1 -> convert b2 >>= \db2 -> returnTuple db1 db2	
		where
			returnTuple (DynTaskVal t1 :: DynTaskVal a) (DynTaskVal t2 :: DynTaskVal b) 
										= return (dynamic DynTaskVal (t1,t2) :: (DynTaskVal (a,b)))
		convert (List b)				= convert b >>= \dl -> returnList dl
		where
			returnList (DynTaskVal v :: DynTaskVal a)	= return (dynamic DynTaskVal [] :: DynTaskVal [a])
		convert (Hide b)				= convert b >>= returnHidden
		where
			returnHidden (DynTaskVal nb :: DynTaskVal a)= return (dynamic DynTaskVal (Hidden nb) :: DynTaskVal (Hidden a))
		convert (Option b)				= convert b >>= \db -> returnOption db
		where
			returnOption (DynTaskVal v :: DynTaskVal a) = return (dynamic DynTaskVal Nothing :: DynTaskVal (Maybe a))
		convert (Labeled (s, b))		= convert b >>= \nb ->	returnLabel s nb
		where
				returnLabel s (DynTaskVal v :: DynTaskVal a) 
									= return (dynamic DynTaskVal (Static s,v) :: DynTaskVal (Static String,a))
		convert	Notes				= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Note)	
		convert	Date				= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Date)	
		convert	Time				= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Time)	
		convert	Document			= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal Document)	
		convert	GoogleMap			= getDefaultValue >>= \v -> return (dynamic DynTaskVal v :: DynTaskVal GoogleMap)	
		convert _					= abort "Fatal Error in Convert !!!"


// ****************************

:: Tup a b = Tup a b

gVisualize{|Tup|} f1 f2 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2) = case old of (VValue (Tup o1 o2) omask) = (VValue o1 omask, VValue o2 omask) ; _ = (VBlank, VBlank)
			# (viz1,rh1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,rh2,vst) = f2 v2 v2 vst
			= ([TUIFragment (TUIPanel {TUIPanel | layout="form", buttons = Nothing, autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = label2s optional label, unstyled=True, renderingHint=0, //Tuple always full width
											 items = [ 
											 	TUIPanel {TUIPanel| layout = "form", buttons = Nothing, autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz1, renderingHint = rh1, unstyled=True},
											 	TUIPanel {TUIPanel| layout = "form", buttons = Nothing, autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz2, renderingHint = rh2, unstyled=True}
											 ]})]			 
			  , 0
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (Tup o1 o2) omask, VValue(Tup n1 n2) nmask)
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,rh2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					= (viz1 ++ [TextFragment ", "] ++ viz2,6,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,rh2,vst) = f2 VBlank VBlank vst
					= (viz1 ++ [TextFragment ", "] ++ viz2,6,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})			
	
coerceToTUIDefs :: [Visualization] -> [TUIDef]
coerceToTUIDefs visualizations = [d \\ (TUIFragment d) <- visualizations]

 
