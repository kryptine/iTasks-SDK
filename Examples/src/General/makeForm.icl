module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
from	EstherBackend import toStringDynamic
from	StdMisc import abort
				
derive gPrint 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, AssignInfo, DynFormFlowStore, Elem
derive gParse 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, AssignInfo, DynFormFlowStore, Elem
derive gUpdate 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, AssignInfo, DynFormFlowStore, Elem
derive gVisualize 	DynFormFlow, DynForm, DynFlow, FormType, FlowType, AssignInfo, DynFormFlowStore

derive bimap Maybe, (,)

Start :: *World -> *World 
Start w = startEngine dynFormEditor w

dynFormEditor :: [Workflow]
dynFormEditor
= [	{ Workflow 
	| name		= "Interactive Workflows / 1. FORM Editor"
	, label		= "FORM Editor"
	, roles		= []
	, mainTask	= loopFORM Nothing
	}
	,{ Workflow 
	| name		= "Interactive Workflows / 2. FLOW Editor"
	, label		= "FLOW Editor"
	, roles		= []
	, mainTask	= loopFLOW Nothing
	}
	,{ Workflow 
	| name		= "Interactive Workflows / 3. Show Definitions"
	, label		= "Show All Defintions"
	, roles		= []
	, mainTask	= showAll
	}
	,{ Workflow 
	| name		= "Interactive Workflows / 4. Run FLOW"
	, label		= "Run Flow"
	, roles		= []
	, mainTask	= loopStart
	}
  ]

:: T  a b	= T  !a & iTask b
:: T2 a b c	= T2 !a & iTask b & iTask c


:: DynFormFlow	=	Form DynForm
				|	Flow DynFlow
				|	NoDynFormFlow
:: DynForm		= 	{ formType 	:: ![FormType]
			  		, dynForm	:: !Dynamic
			  		}
:: DynFlow		= 	{ flowType 	:: ![FlowType]
			  		, dynFlow 	:: !Dynamic
			  		}
:: FormType 	= 	Integer 	
				| 	Real 		
				| 	String 	
				| 	Bool 		
				| 	Tuple  	!(!FormType, !FormType)
				| 	List 	!FormType
				|	Hide	!FormType
				| 	Option 	!FormType
				| 	Labeled !(!String, !FormType)
				| 	Notes 	
				| 	Date 		
				| 	Time 		
				| 	Document 	
				| 	GoogleMap 
:: FlowType		= 	Editor 			!String	
				| 	DisplayIt		!String 		
				| 	Return
				|	Assign			!AssignInfo ![FlowType]
				| 	Or  !([FlowType], ![FlowType])
				| 	And !([FlowType], ![FlowType])
				|	FormFromStore 	!String
				|	FlowFromStore 	!String
				| 	First
				| 	Second
:: AssignInfo	= 	{ idOfUser	:: !Int
			  		, taskName 	:: !String
			  		}
:: DynFormFlowStore
				= 	{ dynFormFlowName 	:: !String
					, dynFormFlowType	:: !String
			  		, dynFormFlow 		:: !DynFormFlow
			  		, formDBRef 		:: !DBRef !DynFormFlowStore
			  		}

emptyForm 		= 	{ formType = []
					, dynForm = dynamic T "Dynamic Form not defined!" :: T String String
					}

emptyFlow 		= 	{ flowType = []
					, dynFlow = dynamic T "Dynamic Flow not defined!" :: T String String
					}
// ------------

undef = undef 

FormTypeNew 		:== "Form Type / New"
FormTypeEdit		:== "Form Type / Edit"
FormNew				:== "Form / New"
FormEdit			:== "Form / Edit"

DynFormFlowRead		:== "DynFormFlow / Read"
DynFormFlowStore	:== "DynFormFlow / Store"

FlowNew 			:== "Flow Type / New"
FlowEdit 			:== "Flow Type / Edit"

FlowStart 			:== "Flow / Start"

Exit				:== "Exit"

// 1. FORM Editor

loopFORM :: (Maybe DynForm) -> Task Void
loopFORM Nothing
	=	enterChoice "Welcome to the FORM Editor, make a choice ..." [FormTypeNew, DynFormFlowRead, Exit]
		>>= \choice ->	case choice of
							FormTypeNew 	-> editFormTypes emptyForm 		>>=  	loopFORM 
							DynFormFlowRead	-> readForm						>>=		loopFORM

							_				-> return Void
loopFORM (Just form)
	=	enterChoice "FORM Editor, make a choice ..." [FormTypeNew, FormTypeEdit, FormNew, FormEdit
													  ,DynFormFlowRead, DynFormFlowStore, Exit]
		>>= \choice ->	case choice of
							FormTypeNew 	-> editFormTypes emptyForm 		>>=  	loopFORM 
							FormTypeEdit 	-> editFormTypes form 			>>=  	loopFORM 

							FormNew			-> makeForm form				>>= 	loopFORM 
							FormEdit		-> editForm form				>>= 	loopFORM

							DynFormFlowRead	-> readForm						>>=		loopFORM
							DynFormFlowStore-> storeForm form				>>|		loopFORM (Just form)

							_				-> return Void

// 2. FLOW Editor

loopFLOW :: (Maybe DynFlow) -> Task Void
loopFLOW Nothing
	=	enterChoice "Welcome to the FLOW Editor, make a choice ..." [FlowNew, DynFormFlowRead, Exit]
		>>= \choice ->	case choice of
							FlowNew 		-> makeFlow emptyFlow 			>>=  	loopFLOW o Just 
							DynFormFlowRead	-> readFlow						>>=		loopFLOW

							_				-> return Void
loopFLOW (Just flow)
	=	enterChoice "FORM Editor, make a choice ..." [FlowNew, FlowEdit 
													  ,DynFormFlowRead, DynFormFlowStore, Exit]
		>>= \choice ->	case choice of
							FlowNew 		-> makeFlow emptyFlow 			>>=  	loopFLOW o Just
							FlowEdit 		-> makeFlow flow 				>>=  	loopFLOW o Just 

							DynFormFlowRead	-> readFlow						>>=		loopFLOW
							DynFormFlowStore-> storeFlow flow				>>|		loopFLOW (Just flow)

							_				-> return Void

loopStart
	=	enterChoice "Run a stored flow, make a choice ..." [FlowStart, Exit]
		>>= \choice ->	case choice of
							FlowStart		-> startFlow					>>|		loopStart 
							Exit			-> return Void

// ------------

editFormTypes :: DynForm -> Task (Maybe DynForm)
editFormTypes form
	= 						updateInformation "Construct the shape of the form:" form.formType
		>>= \formType ->	case formType of
								[] -> 	return Nothing
								_ ->					makeDynForm formType 
										>>= \dynForm -> editForm {formType = formType, dynForm = dynForm}

makeForm :: DynForm -> Task (Maybe DynForm)
makeForm form 
	= 						makeDynForm form.formType 
		>>= \dynForm ->		editForm {form & dynForm = dynForm}

editForm ::  DynForm -> Task (Maybe DynForm)
editForm form=:{dynForm = (T v :: T a a)}	
	= 			updateInformation "Set default values ..." v
		>>= 	returnShow form.dynForm 		
where
	returnShow :: Dynamic a -> Task (Maybe DynForm) | iTask a
	returnShow d=:(T v :: T a^ b) nv = return (Just {form & dynForm = dynamic T nv :: T a^ a^})
	returnShow _ nv = return Nothing
editForm form	= return Nothing

// ------------

makeDynForm :: [FormType] -> Task Dynamic
makeDynForm bs = convertFormTypes bs >>= return o tupling
where
	tupling [] 		= dynamic T Void :: T Void Void
	tupling [d]		= d
	tupling [d:ds]	= case (d, tupling ds) of 
							(T d1 :: T a a, T d2 :: T b b) -> dynamic T (Elem d1 d2) :: T (Elem a b) (Elem a b)	
							_ -> abort "Fatal Error in makeDynForm !!!"

	convertFormTypes :: [FormType] -> Task [Dynamic]
	convertFormTypes [] 		= return []
	convertFormTypes [b:bs] 	= convert b >>= \d -> convertFormTypes bs >>= \ds -> return [d:ds] 
	where
		convert :: FormType -> Task Dynamic
		convert	Integer					= getDefaultValue >>= \v -> return (dynamic T v :: T Int Int)	
		convert	Real					= getDefaultValue >>= \v -> return (dynamic T v :: T Real Real)	
		convert	String					= getDefaultValue >>= \v -> return (dynamic T v :: T String String)	
		convert	Bool					= getDefaultValue >>= \v -> return (dynamic T v :: T Bool Bool)	
		convert	(Tuple (b1, b2))		= convert b1 >>= \db1 -> convert b2 >>= \db2 -> returnTuple db1 db2	
		where
			returnTuple (T t1 :: T a a) (T t2 :: T b b) 
										= return (dynamic T2 (t1,t2) :: (T2 (a,b) a b))
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
		
// ------------

instance DB DynFormFlowStore where
	databaseId	:: DBid [DynFormFlowStore]
	databaseId = mkDBid "FormStore"
	
	getItemId	:: DynFormFlowStore -> DBRef DynFormFlowStore
	getItemId a = a.formDBRef

	setItemId	:: (DBRef DynFormFlowStore) DynFormFlowStore -> DynFormFlowStore
	setItemId dbref a = {a & formDBRef = dbref}

storeForm :: DynForm -> Task Void
storeForm dynForm = storeFormFlow (Form dynForm)

storeFlow :: DynFlow -> Task Void
storeFlow dynFlow = storeFormFlow (Flow dynFlow)

storeFormFlow :: DynFormFlow -> Task Void
storeFormFlow dynFormFlow
	=						enterInformation "Define name to store:"
		>>= \extname ->		dbReadAll
		>>= \all ->			if	(isMember extname [this.dynFormFlowName \\ this <- all])
								(			requestConfirmation ("Name already exists, do you want to overwrite?")
								 >>= \ok -> if ok (updateItem all extname) (storeFormFlow dynFormFlow)
								)
								(storeItem extname)
	where
		showDynType2 (T x :: T a b) = showDynType (dynamic undef :: a) 
		showDynType2 (x :: a) 		= showDynType (dynamic undef :: a) 

		type = case dynFormFlow of
					(Form dynForm) = showDynType dynForm.dynForm
					(Flow dynFlow) = showDynType dynFlow.dynFlow
					_				= "Error, Unknown Type !"

		updateItem all name
			=				return (hd [this \\ this <- all | this.dynFormFlowName == name])
			>>= \oform ->	dbUpdateItem {oform & dynFormFlowName = name, dynFormFlowType = type, dynFormFlow = dynFormFlow}
			>>|				return Void

		storeItem name
			=				dbCreateItem
			>>= \oform ->	dbUpdateItem {oform & dynFormFlowName = name, dynFormFlowType = type, dynFormFlow = dynFormFlow}
			>>|				return Void

readForm :: Task (Maybe DynForm)
readForm 
	= 						readstoreForm (\this -> case this.dynFormFlow of
														(Form _) -> True
														else -> False) 
			>>= \found ->	case found of
								(Form dynForm)	->	return (Just dynForm)	
								_ 				-> 	return Nothing

readFlow :: Task (Maybe DynFlow)
readFlow 
	= 						readstoreForm (\this -> case this.dynFormFlow of
														(Flow _) -> True
														else -> False) 
			>>= \found ->	case found of
								(Flow dynFlow)	->	return (Just dynFlow)	
								_ 				-> 	return Nothing

readstoreForm :: (DynFormFlowStore -> Bool) ->  Task DynFormFlow
readstoreForm pred  
	=						readAll
		>>= \all ->			let names = [showName this \\ this <- all | pred this] in
								case names of
								 [] ->					updateInformation "No definitions stored !" Void
								 		>>|				return NoDynFormFlow
								 names ->				enterChoice "Choose definition you want to use:" names
										>>= \choice ->	return (hd [this.dynFormFlow \\ this <- all | showName this == choice])
where
	showName this = this.dynFormFlowName +++ " :: " +++ this.dynFormFlowType

	readAll :: Task [DynFormFlowStore]
	readAll = dbReadAll

showAll :: Task Void
showAll
	=						readAll
		>>= \all ->			case all of
							 [] ->	showMessage "There are no definitions stored yet." 
							 all -> showMessageAbout "The following definitions have been stored:" all
where
	readAll :: Task [DynFormFlowStore]
	readAll = dbReadAll

// ------------

makeFlow :: DynFlow -> Task DynFlow
makeFlow flow
	=						updateInformation "Construct a flow:" flow.flowType
		>>= \flowType ->	try (checkIt flowType) (errorRaised flowType)
where
	checkIt flowType 
		=					checkFlows flowType
			>>= \dynFlow ->	showMessage ("Deduced type: " +++ showDynType dynFlow)
			>>|				return {flowType = flowType, dynFlow = dynFlow}

	errorRaised :: [FlowType] String -> Task DynFlow
	errorRaised flowType s
		=					showMessage s >>| return {flow & flowType = flowType}					

checkFlows :: [FlowType] -> Task Dynamic  
checkFlows [] 		= throw "Cannot apply empty flow."
checkFlows flows 	= mapMonad translate flows >>= \dyns -> return (applyFlows (hd dyns) (tl dyns))
where
	mapMonad :: (!FlowType -> Task Dynamic) [FlowType] -> Task [Dynamic]	// leaving out the type crashes the compiler !!!
	mapMonad fun [] 	= return []
	mapMonad fun [d:ds] = fun d >>= \nd -> mapMonad fun ds >>= \nds -> return [nd:nds] 

	translate :: !FlowType -> Task Dynamic
	translate (Editor prompt)		= return (dynamic (edit prompt):: A.a: a -> Task a | iTask a)
	where
		edit ::  !String a -> Task a | iTask a
		edit prompt v = updateInformation prompt v

	translate (DisplayIt prompt)	= return (dynamic (display prompt):: A.a: a -> Task Void | iTask a)
	where
		display ::  !String a -> Task Void | iTask a
		display prompt v = showMessageAbout prompt v

	translate Return			  	= return (dynamic (\v -> return v) :: A.a: a -> Task a | iTask a)

	translate (Or (left, right))	= checkFlows left >>= \leftflow -> checkFlows right >>= \rightflow -> checkOr leftflow rightflow
	where
		checkOr :: Dynamic Dynamic -> Task Dynamic
		checkOr (T ta :: T (Task a) a) (T tb :: T (Task a) a)  
			= return (dynamic T (ta -||- tb) :: T (Task a) a)
		checkOr (T ta :: T (a -> Task a) a) (T tb :: T (a -> Task a) a)  
			= return (dynamic T (\a -> ta a -||- tb a) :: T (a -> Task a) a)
		checkOr (ta :: A.a: a -> Task a | iTask a) (tb :: A.a: a -> Task a | iTask a)  
			= return (dynamic (\a -> ta a -||- tb a) :: A.a: a -> Task a | iTask a)
		checkOr d1 d2
			= throw (dynErrorMess2 "Or" d1 d2)

	translate (And (left, right))	= checkFlows left >>= \leftflow -> checkFlows right >>= \rightflow -> checkAnd leftflow rightflow
	where
		checkAnd :: Dynamic Dynamic -> Task Dynamic
		checkAnd (T ta :: T (Task a) a) (T tb :: T (Task b) b)  
			= return (dynamic T (ta -&&- tb) :: T (Task (a,b)) (a,b))
		checkAnd d1 d2
			= throw (dynErrorMess2 "And" d1 d2)
	
	translate (Assign assignInfo flow)	= checkFlows flow >>= assignTask assignInfo
	where
		assignTask :: !AssignInfo !Dynamic -> Task Dynamic
		assignTask info (e :: A.a: a -> Task a | iTask a) 		
			= return (dynamic (apply info e) :: A.a: a -> Task a | iTask a)
		where
			apply :: !AssignInfo !(A.a: a -> Task a | iTask a) b -> Task b | iTask b
			apply info e v = assign info.idOfUser NormalPriority Nothing (e v <<@ info.taskName)
		assignTask info d 		
			= throw (dynErrorMess "Assign" d)

	translate First				  	= return (dynamic fst :: A.a b: (a,b) -> a)
	translate Second			  	= return (dynamic snd :: A.a b: (a,b) -> b)
	translate (FormFromStore name) 	= findValue name
	translate (FlowFromStore name) 	= findFlow name

findValue :: String -> Task Dynamic
findValue name
	= 						dbReadAll
		>>= \all ->			examine [this \\ this <- all | this.dynFormFlowName == name]
where
	examine [] 		= throw ("Cannot find form value with name " +++ name)
	examine [f:fs] 	= case f.dynFormFlow of
						(Form form) = return form.dynForm
						_ 			= throw ("Type Error: Value expected, flow found named " +++ name)

findFlow :: String -> Task Dynamic
findFlow name
	= 						dbReadAll
		>>= \all ->			examine [this \\ this <- all | this.dynFormFlowName == name]
where
	examine [] 		= throw ("Cannot find form value with name " +++ name)
	examine [f:fs] 	= case f.dynFormFlow of
						(Flow form) = return form.dynFlow
						_ 			= throw ("Type Error: Value expected, flow found named " +++ name)

applyFlows :: Dynamic [Dynamic] -> Dynamic  
applyFlows dyn [] = dyn
applyFlows (T v :: T a a)  [edit :: A.b: b -> Task b | iTask b : dyns]				// edit value
	= applyFlows (dynamic T (edit v) :: T (Task a) a) dyns


applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task b | iTask b ): dyns]		// >>=
	= applyFlows (dynamic T (t >>= btb) :: T (Task a) a) dyns

//applyFlows (T t :: T (Task a) a)  [(atb :: T (a -> Task b) b): dyns]
//	= applyFlows (dynamic T (t >>= atb) :: T (Task b) b) dyns

applyFlows (T ta :: T (Task a) a)  [(T tb :: T (Task b) b): dyns]					// >>|
	= applyFlows (dynamic T (ta >>| tb) :: T (Task b) b) dyns


applyFlows (T2 v :: T2 (c,d) c d )  [(fst ::  A.a b: (a,b) -> a) : dyns]			// first 
	= applyFlows (dynamic T (fst v) :: T c c) dyns
applyFlows (T2 v :: T2 (c,d) c d)  [(snd ::  A.a b: (a,b) -> b) : dyns]				// second
	= applyFlows (dynamic T (snd v) :: T d d) dyns
applyFlows (T2 v :: T2 (c,d) c d) dyns												// T2 -> T!
	= applyFlows (dynamic T v :: T (c,d) (c,d)) dyns

applyFlows (x :: a)  [(f :: a -> b): dyns]											// common dyn apply
	= applyFlows (dynamic f x :: b) dyns

applyFlows d ds
	= dynamic "Could not parse defined dynamic flow."

// ------------

startFlow
	= 						getCurrentUser
		>>= \me ->			readFlow 
		>>= \mkdynFlow ->	if (isNothing mkdynFlow) (return Void) (evalFlow me ((fromJust mkdynFlow).dynFlow)) 
where
	evalFlow me (T t:: T (Task a) a)	= spawnProcess me.userId True (t <<@ "dynamic flow")>>| return Void
	evalFlow me (T v:: T a b)			= showMessage (showDynValType "Result" (dynamic v :: a))
	evalFlow me (T2 v:: T2 a b c)		= showMessage (showDynValType "Result" (dynamic v :: a))
	evalFlow me d						= showMessage (dynErrorMess "Eval" d) 

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
dynErrorMess s d1 = s +++ ": Type Error: " +++ showDynType d1

dynError2 s d1 d2 = return (dynamic T (dynErrorMess2 s d1 d2):: T String String)
dynErrorMess2 s d1 d2 = s +++ ": Cannot Unify: " +++ showDynType d1 +++ " with "  +++ showDynType d2

