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
					, formDyn = dynamic T Void :: T Void Void
					}

formShapeToFormDyn :: [FormShape] -> Task Dynamic
formShapeToFormDyn bs = convertFormShapes bs >>= return o tupling
where
	tupling [] 		= dynamic T Void :: T Void Void
	tupling [d]		= d
	tupling [d:ds]	= case (d, tupling ds) of 
							(T d1 :: T a a, T d2 :: T b b) -> dynamic T (Tup d1 d2) :: T (Tup a b) (Tup a b)	
							_ -> abort "Fatal Error in shapeToForm !!!"

	convertFormShapes :: [FormShape] -> Task [Dynamic]
	convertFormShapes [] 		= return []
	convertFormShapes [b:bs] 	= convert b >>= \d -> convertFormShapes bs >>= \ds -> return [d:ds] 
	where
		convert :: FormShape -> Task Dynamic
		convert	Integer					= getDefaultValue >>= \v -> return (dynamic T v :: T Int Int)	
		convert	Real					= getDefaultValue >>= \v -> return (dynamic T v :: T Real Real)	
		convert	String					= getDefaultValue >>= \v -> return (dynamic T v :: T String String)	
		convert	Bool					= getDefaultValue >>= \v -> return (dynamic T v :: T Bool Bool)	
		convert	(Tuple (b1, b2))		= convert b1 >>= \db1 -> convert b2 >>= \db2 -> returnTuple db1 db2	
		where
			returnTuple (T t1 :: T a a) (T t2 :: T b b) 
										= return (dynamic T (t1,t2) :: (T (a,b)(a,b)))
//										= return (dynamic T2 (t1,t2) :: (T2 (a,b) a b))
		convert (List b)				= convert b >>= \dl -> returnList dl
		where
			returnList (T v :: T a a)	= return (dynamic T [] :: T [a] [a])
		convert (Hide b)				= convert b >>= returnHidden
		where
			returnHidden (T nb :: T a a)= return (dynamic T (Hidden nb) :: T (Hidden a) (Hidden a))
		convert (Option b)				= convert b >>= \db -> returnOption db
		where
			returnOption (T v :: T a a) = return (dynamic T Nothing :: T (Maybe a) (Maybe a))
		convert (Labeled (s, b))		= convert b >>= \nb ->	returnLabel s nb
		where
				returnLabel s (T v :: T a a) 
									= return (dynamic T (Static s,v) :: T (Static String,a) (Static String,a))
		convert	Notes				= getDefaultValue >>= \v -> return (dynamic T v :: T Note Note)	
		convert	Date				= getDefaultValue >>= \v -> return (dynamic T v :: T Date Date)	
		convert	Time				= getDefaultValue >>= \v -> return (dynamic T v :: T Time Time)	
		convert	Document			= getDefaultValue >>= \v -> return (dynamic T v :: T Document Document)	
		convert	GoogleMap			= getDefaultValue >>= \v -> return (dynamic T v :: T GoogleMap GoogleMap)	
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

 
