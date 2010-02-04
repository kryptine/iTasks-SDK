module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
from	EstherBackend import toStringDynamic
				
derive gPrint 		FormType, FlowType, EditorInfo, AssignInfo, FormStore, FlowStore, Elem
derive gParse 		FormType, FlowType, EditorInfo, AssignInfo, FormStore, FlowStore, Elem
derive gUpdate 		FormType, FlowType, EditorInfo, AssignInfo, FormStore, FlowStore, Elem
derive gVisualize 	FormType, FlowType, EditorInfo, AssignInfo, FormStore, FlowStore

Start :: *World -> *World 
Start w = startEngine dynFormEditor w

dynFormEditor :: [Workflow]
dynFormEditor
= [	{ Workflow 
	| name		= "makeForm"
	, label		= "makeForm"
	, roles		= []
	, mainTask	= test >>| return Void
	}
  ]

:: T a b	= T !a & iTask b

:: FormType = Integer 	
			| Real 		
			| String 	
			| Bool 		
			| Tuple  	!(!FormType, !FormType)
			| List 		!FormType
			| Option 	!FormType
			| Labeled 	!(!String, !FormType)
			| Notes 	
			| Date 		
			| Time 		
			| Document 	
			| GoogleMap 

:: FlowType	= Editor 	!EditorInfo
			| DisplayIt !EditorInfo
			| Return
			| First
			| Second
			| Or  ![FlowType] ![FlowType]
			| And ![FlowType] ![FlowType]

:: EditorInfo 
			= { prompt  	:: !String
			  ,	assignTo 	:: !Maybe !AssignInfo
			  }
:: AssignInfo
			= { idOfUser	:: !Int
			  ,	taskName 	:: !String
			  }

// ------------

test =  looping [] (dynamic (T "No form defined yet") :: T String String) [] (dynamic (T "No flow defined yet") :: T String String)

looping :: [FormType] Dynamic [FlowType] Dynamic -> Task Void
looping formType form flowType flow
	=					enterChoice "Kies maar" [ FormTypeNew, 	FormTypeEdit 
												, FormNew, 		FormEdit
												, FormStore, 	FormRead
												, FlowNew, 		FlowEdit
												, FlowStore, 	FlowRead
												, FlowStart
												, Exit
												]
		>>= \choice ->	case choice of
							FormTypeNew 	-> editFormTypes [] 		>>= \(formType,form) 	-> 	looping formType form flowType flow
							FormTypeEdit 	-> editFormTypes formType	>>= \(formType,form) 	-> 	looping formType form flowType flow

							FormNew			-> makeForm formType		>>= \form 				-> 	looping formType form flowType flow
							FormEdit		-> editForm form			>>= \form 				-> 	looping formType form flowType flow

							FormStore		-> storeForm formType form	>>| 			   			looping formType form flowType flow
							FormRead		-> readForm formType form	>>= \(formType,form)	->	looping formType form flowType flow

							FlowNew			-> makeFlow form []			>>= \flow 				-> 	looping formType form flowType flow
							FlowEdit		-> makeFlow form flowType	>>= \flow 				-> 	looping formType form flowType flow
							FlowStart		-> startFlow flow			>>|				   			looping formType form flowType flow

							FlowStore		-> storeFlow flowType form	>>| 			   			looping formType form flowType flow
							FlowRead		-> readFlow flowType form	>>= \(flowType,form)	->	looping formType form flowType flow
							
							Exit			-> return Void

FormTypeNew 	:== "Form Type / Create New"
FormTypeEdit	:== "Form Type / Edit"
FormNew			:== "Form / Create New"
FormEdit		:== "Form / Edit"
FormRead		:== "Form / Read"
FormStore		:== "Form / Store"

FlowNew 		:== "Flow / New"
FlowEdit 		:== "Flow / Edit"
FlowStart 		:== "Flow / Start"
FlowRead		:== "Flow / Read"
FlowStore		:== "Flow / Store"

Exit			:== "Exit"
// ------------

editFormTypes :: [FormType] -> Task ([FormType],Dynamic)
editFormTypes formType 
	= 						updateInformation "Construct a form by chosing the types of the form fields:" formType
		>>= \formType -> 	createForm formType 
		>>= \form ->		return (formType, form)

createForm :: [FormType] -> Task Dynamic
createForm bs = convertFormTypes bs >>= return o tupling
where
	tupling [] 		= dynamic T Void :: T Void Void
	tupling [d]		= d
	tupling [d:ds]	= case (d, tupling ds) of 
							(T d1 :: T a a, T d2 :: T b b) -> dynamic T (Elem d1 d2) :: T (Elem a b) (Elem a b)	

	convertFormTypes :: [FormType] -> Task [Dynamic]
	convertFormTypes [] 		= return []
	convertFormTypes [b:bs] 	= convert b >>= \d -> convertFormTypes bs >>= \ds -> return [d:ds] 
	where
		convert :: FormType -> Task Dynamic
		convert	Integer				= getDefaultValue >>= \v -> return (dynamic T v :: T Int Int)	
		convert	Real				= getDefaultValue >>= \v -> return (dynamic T v :: T Real Real)	
		convert	String				= getDefaultValue >>= \v -> return (dynamic T v :: T String String)	
		convert	Bool				= getDefaultValue >>= \v -> return (dynamic T v :: T Bool Bool)	
		convert	(Tuple (b1, b2))	= 				convert b1 
										>>= \db1 -> convert b2
										>>= \db2 -> returnTuple db1 db2	
		convert (List b)			=				convert b
										>>= \dl ->	returnList dl
		convert (Option b)			=				convert b
										>>= \db ->	returnOption db
		convert (Labeled (s, b))	=				convert b
										>>= \db ->	returnLabel s db
		convert	Notes				= getDefaultValue >>= \v -> return (dynamic T v :: T Note Note)	
		convert	Date				= getDefaultValue >>= \v -> return (dynamic T v :: T Date Date)	
		convert	Time				= getDefaultValue >>= \v -> return (dynamic T v :: T Time Time)	
		convert	Document			= getDefaultValue >>= \v -> return (dynamic T v :: T Document Document)	
		convert	GoogleMap			= getDefaultValue >>= \v -> return (dynamic T v :: T GoogleMap GoogleMap)	

		returnTuple (T t1 :: T a a) (T t2 :: T b b) = return (dynamic T (t1,t2) :: T (a,b) (a,b))
		
		returnList (T v :: T a a) = return (dynamic T [] :: T [a] [a])
		
		returnOption (T v :: T a a) = return (dynamic T Nothing :: T (Maybe a) (Maybe a))

		returnLabel s (T v :: T a a) = return (dynamic T (s,v) :: T (String,a) (String,a))

// ------------

makeForm :: [FormType] -> Task Dynamic
makeForm formType 
	= 					createForm formType 
		>>= \dyn ->		editForm dyn

// ------------

editForm ::  Dynamic -> Task Dynamic
editForm d=:(T v :: T a a)	
	= 			updateInformation "Initialize form where needed..." v
		>>= 	returnShow d 		
where
	returnShow :: Dynamic a -> Task Dynamic | iTask a
	returnShow d=:(T v :: T a^ b) nv = return (dynamic T nv :: T a^ a^)

// ------------

:: FormStore = {formName :: !String, formType :: ![FormType], form :: !Dynamic, formDBRef :: !DBRef !FormStore}

instance DB FormStore where
	databaseId	:: DBid [FormStore]
	databaseId = mkDBid "FormStore"
	
	getItemId	:: FormStore -> DBRef FormStore
	getItemId a = a.formDBRef

	setItemId	:: (DBRef FormStore) FormStore -> FormStore
	setItemId dbref a = {a & formDBRef = dbref}

storeForm formType form 
	=						enterInformation "Define name of form to store:"
		>>= \name ->		return (extName name)
		>>= \extname ->		dbReadAll
		>>= \all ->			if	(isMember extname [this.formName \\ this <- all])
								(			requestConfirmation ("Name " +++ extname +++ " already exists, do you want to overwrite?")
								 >>= \ok -> if ok (updateItem all extname) (storeForm formType form)
								)
								(storeItem extname)
								 
	where
		extName name = name +++ " :: " +++ showDynType2 form
		showDynType2 (T x :: T a b) = showDynType (dynamic undef :: a) 

		updateItem all name
			=				return (hd [this \\ this <- all | this.formName == name])
			>>= \oform ->	dbUpdateItem {oform & formName = name, formType = formType, form = form}

		storeItem name
			=				dbCreateItem
			>>= \oform ->	dbUpdateItem {oform & formName = name, formType = formType, form = form}

undef = undef
readForm formType form  
	=						dbReadAll
		>>= \all ->			case all of
							 [] ->					updateInformation "No stored forms can be found." Void
							 		>>|				return (formType,form)
							 all ->					enterChoice "Choose form you want to use:" [this.formName \\ this <- all]
									>>= \name ->	return (hd [(this.formType,this.form) \\ this <- all | this.formName == name])

// ------------

makeFlow form flowtype
	=						updateInformation "Construct a flow:" flowtype
		>>= \flowtype ->	checkFlow form flowtype

checkFlow :: Dynamic [FlowType] -> Task Dynamic  
checkFlow (T v:: T a a) [Editor info : flows]
	= checkFlow (dynamic T (assignTask info v) :: T (Task a) a) flows
checkFlow (T t:: T (Task a) a) [Editor info : flows]
	= checkFlow (dynamic T (t >>= \a -> assignTask info a) :: T (Task a) a) flows

checkFlow (T v:: T a a) [DisplayIt info : flows]
	= checkFlow (dynamic T (showMessageTask info v) :: T (Task Void) Void) flows
checkFlow (T t:: T (Task a) a) [DisplayIt info : flows]
	= checkFlow (dynamic T (t >>= \a -> showMessageTask info a) :: T (Task Void) Void) flows

checkFlow dyn [Return : flows]
	= checkFlow dyn flows

checkFlow dyn [Or dfa dfb : flows]
	= checkFlow dyn dfa >>= \da -> checkFlow dyn dfb >>= \db -> orTask da db flows

checkFlow dyn [And dfa dfb : flows]
	= checkFlow dyn dfa >>= \da -> checkFlow dyn dfb >>= \db -> andTask da db flows

checkFlow (T (x,y) :: T (a,b) c) [First : flows]			// (a,b) would make more sense
	= checkFlow (dynamic T x :: T a c) flows
//checkFlow (T t :: T (Task (a,b)) (a,b)) [First : flows]			// (a,b) would make more sense
//	= checkFlow (dynamic T (t >>= \(x,y) -> return x) :: T (Task a) a) flows
checkFlow d1 [First : flows]			
	= dynError2 "First" d1 (dynamic fst :: A.a b : (a,b) -> a)
	
checkFlow dyn _
	= return dyn

assignTask info=:{assignTo = Just some} v 		
	= assign some.idOfUser NormalPriority Nothing (updateInformation info.prompt v <<@ some.taskName)
assignTask info v 		
	= updateInformation info.prompt v

showMessageTask info=:{assignTo = Just some} v 		
	= assign some.idOfUser NormalPriority Nothing (showMessageAbout info.prompt v <<@ some.taskName)
showMessageTask info v 		
	= showMessageAbout info.prompt v

orTask (T ta :: T (Task a) a) (T tb :: T (Task a) a) flows  
	= checkFlow (dynamic T (ta -||- tb) :: T (Task a) a) flows	
orTask d1 d2 flows  
	= dynError2 "Or" d1 d2

andTask (T ta :: T (Task a) a) (T tb :: T (Task b) b) flows  
	= checkFlow (dynamic T (ta -&&- tb) :: T (Task (a,b)) (a,b)) flows	
andTask d1 d2 flows  
	= dynError2 "And" d1 d2

// ------------

:: FlowStore = {flowName :: !String, flowType :: ![FlowType], flow :: !Dynamic, flowDBRef :: !DBRef !FlowStore}

instance DB FlowStore where
	databaseId	:: DBid [FlowStore]
	databaseId = mkDBid "FlowStore"
	
	getItemId	:: FlowStore -> DBRef FlowStore
	getItemId a = a.flowDBRef

	setItemId	:: (DBRef FlowStore) FlowStore -> FlowStore
	setItemId dbref a = {a & flowDBRef = dbref}

storeFlow flowType flow 
	=						enterInformation "Define name of flow to store:"
		>>= \name ->		return (extName name)
		>>= \extname ->		dbReadAll
		>>= \all ->			if	(isMember extname [this.flowName \\ this <- all])
								(			requestConfirmation ("Name " +++ extname +++ " already exists, do you want to overwrite?")
								 >>= \ok -> if ok (updateItem all extname) (storeFlow flowType flow)
								)
								(storeItem extname)
								 
	where
		extName name = name +++ " :: " +++ showDynType flow
//		showDynType2 (T x :: T a b) = showDynType (dynamic undef :: a) 

		updateItem all name
			=				return (hd [this \\ this <- all | this.flowName == name])
			>>= \oflow ->	dbUpdateItem {oflow & flowName = name, flowType = flowType, flow = flow}

		storeItem name
			=				dbCreateItem
			>>= \oflow ->	dbUpdateItem {oflow & flowName = name, flowType = flowType, flow = flow}

readFlow flowType flow  
	=						dbReadAll
		>>= \all ->			case all of
							 [] ->					updateInformation "No stored flows can be found." Void
							 		>>|				return (flowType,flow)
							 all ->					enterChoice "Choose flow you want to use:" [this.flowName \\ this <- all]
									>>= \name ->	return (hd [(this.flowType,this.flow) \\ this <- all | this.flowName == name])
// ------------

startFlow dyn 
	= 				getCurrentUser
		>>= \me ->	spawnProcess me.userId True (evalFlow dyn)
		>>|			return Void
where
	evalFlow (T t:: T (Task a) a)	= t >>| return Void
	evalFlow d=:(T v:: T a a)		= showMessage (showDynValType "Result" (dynamic v :: a))
	evalFlow d						= showMessage (dynErrorMess "Eval" d) 

// ****************************

:: Elem a b = Elem a b

gVisualize{|Elem|} f1 f2 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2) = case old of (VValue (Elem o1 o2) omask) = (VValue o1 omask, VValue o2 omask) ; _ = (VBlank, VBlank)
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
				(VValue (Elem o1 o2) omask, VValue(Elem n1 n2) nmask)
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

// **************

showDyn :: Dynamic -> (String,String)
showDyn dyn	
# (v,t) =  toStringDynamic dyn 
=	case dyn of
		(fun :: a -> b) -> ("<function> ",t) 
		_				-> (foldr (+++) "" v,t)

showDynType  = snd o showDyn
showDynVal   = fst o showDyn
showDynValType s d = let (v,t) = showDyn d in s +++ ", " +++ v +++ "::" +++ t

dynError  s d1    = return (dynamic T (dynErrorMess s d1):: T String String)
dynErrorMess s d1 = s +++ ", Type Error: " +++ showDynType d1

dynError2 s d1 d2 = return (dynamic T (dynErrorMess2 s d1 d2):: T String String)
dynErrorMess2 s d1 d2 = s +++ ", Cannot Unify: " +++ showDynType d1 +++ " with "  +++ showDynType d2

// ****************

/*
:: T2 a b = T2 a & TC b

f :: Dynamic -> Dynamic
f (T2 (x,y) :: T2 (a,b) (a,b)) = dynamic x :: T2 a a

*/
