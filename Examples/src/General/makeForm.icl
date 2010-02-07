module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
from	EstherBackend import toStringDynamic
				
derive gPrint 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, EditorInfo, AssignInfo, DynFormFlowStore, Elem
derive gParse 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, EditorInfo, AssignInfo, DynFormFlowStore, Elem
derive gUpdate 		DynFormFlow, DynForm, DynFlow, FormType, FlowType, EditorInfo, AssignInfo, DynFormFlowStore, Elem
derive gVisualize 	DynFormFlow, DynForm, DynFlow, FormType, FlowType, EditorInfo, AssignInfo, DynFormFlowStore

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

:: DynFormFlow	= 	NoDynFormFlow
				| 	Form DynForm
				|	Flow DynFlow
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
				| 	Option 	!FormType
				| 	Labeled !(!String, !FormType)
				| 	Notes 	
				| 	Date 		
				| 	Time 		
				| 	Document 	
				| 	GoogleMap 
:: FlowType		= 	FormFromStore 	!String
				|	FlowFromStore 	!String
				|	Editor 			!EditorInfo
				| 	DisplayIt 		!EditorInfo
				| 	Return
				| 	First
				| 	Second
				| 	Or  ![FlowType] ![FlowType]
				| 	And ![FlowType] ![FlowType]
:: EditorInfo	= 	{ prompt  	:: !String
			  		, assignTo 	:: !Maybe !AssignInfo
			  		}
:: AssignInfo	= 	{ idOfUser	:: !Int
			  		, taskName 	:: !String
			  		}
:: DynFormFlowStore
				= 	{ dynFormFlowName 	:: !String
					, dynFormFlowType	:: !String
			  		, dynFormFlow 		:: !DynFormFlow
			  		, formDBRef 		:: !DBRef !DynFormFlowStore
			  		}

emptyForm 		= Form 
					{ formType = []
					, dynForm = dynamic T "Dynamic Form not defined!" :: T String String
					}

emptyFlow 		= Flow 
					{ flowType = []
					, dynFlow = dynamic T "Dynamic Flow not defined!" :: T String String
					}
// ------------

test =  looping NoDynFormFlow


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

looping :: DynFormFlow -> Task Void
looping NoDynFormFlow
	=	enterChoice "Welcome to the Dynamic Form Editor, make a choice ..." [DynFormFlowRead, FormTypeNew, FlowNew, Exit]
		>>= \choice ->	case choice of
							DynFormFlowRead	-> readFormflow  emptyForm		>>=		looping

							FormTypeNew 	-> editFormTypes emptyForm 		>>=  	looping 
							FlowNew			-> makeFlow 	 emptyFlow		>>= 	looping 
							Exit			-> return Void
looping f=:(Form form)
	=	enterChoice "Dynamic * FORM * Editor, make a choice ..." [FormTypeEdit, FormNew, FormEdit, DynFormFlowStore
																 ,DynFormFlowRead, FormTypeNew, FlowNew, Exit]
		>>= \choice ->	case choice of
							FormTypeEdit 	-> editFormTypes f 				>>=  	looping 
							FormNew			-> makeForm f					>>= 	looping 
							FormEdit		-> editForm f					>>= 	looping 

							DynFormFlowStore -> storeFormflow f				>>=		looping
							DynFormFlowRead	-> readFormflow f				>>=		looping

							FormTypeNew 	-> editFormTypes emptyForm 		>>=  	looping 
							FlowNew			-> makeFlow 	 emptyFlow		>>= 	looping 
							Exit			-> return Void

looping f=:(Flow flow)
	=	enterChoice "Dynamic + FLOW * Editor, make a choice ..." [FlowEdit, DynFormFlowStore
																 ,DynFormFlowRead, FormTypeNew, FlowNew, FlowEdit, FlowStart, Exit]
		>>= \choice ->	case choice of
							FlowEdit		-> makeFlow f					>>= 	looping 

							DynFormFlowStore -> storeFormflow f				>>=		looping
							DynFormFlowRead	-> readFormflow f				>>=		looping

							FormTypeNew 	-> editFormTypes emptyForm 		>>=  	looping 
							FlowNew			-> makeFlow 	 emptyFlow		>>= 	looping 
							FlowEdit		-> makeFlow 	 f				>>= 	looping 
							Exit			-> return Void

							FlowStart		-> startFlow f					>>=		looping 
// ------------

editFormTypes :: DynFormFlow -> Task DynFormFlow
editFormTypes (Form form)
	= 						updateInformation "Construct a form by chosing the types of the form fields:" form.formType
		>>= \formType -> 	if (isEmpty formType) 
								(return NoDynFormFlow) 
								(makeDynForm formType >>= \dynForm ->
								 editForm (Form {form & formType = formType, dynForm = dynForm})
								 )
editFormTypes dynFormFlow	= return dynFormFlow

makeDynForm :: [FormType] -> Task Dynamic
makeDynForm bs = convertFormTypes bs >>= return o tupling
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

		returnLabel s (T v :: T a a) = return (dynamic T (Static s,v) :: T (Static String,a) (Static String,a))

// ------------

makeForm :: DynFormFlow -> Task DynFormFlow
makeForm (Form form) 
	= 						makeDynForm form.formType 
		>>= \dynForm ->		editForm (Form {form & dynForm = dynForm})
makeForm dynFormFlow	= return dynFormFlow

// ------------

editForm ::  DynFormFlow -> Task DynFormFlow
editForm (Form form=:{dynForm = (T v :: T a a)})	
	= 			updateInformation "Initialize form where needed ..." v
		>>= 	returnShow form.dynForm 		
where
	returnShow :: Dynamic a -> Task DynFormFlow | iTask a
	returnShow d=:(T v :: T a^ b) nv = return (Form {form & dynForm = dynamic T nv :: T a^ a^})

editForm dynFormFlow	= return dynFormFlow

// ------------

instance DB DynFormFlowStore where
	databaseId	:: DBid [DynFormFlowStore]
	databaseId = mkDBid "FormStore"
	
	getItemId	:: DynFormFlowStore -> DBRef DynFormFlowStore
	getItemId a = a.formDBRef

	setItemId	:: (DBRef DynFormFlowStore) DynFormFlowStore -> DynFormFlowStore
	setItemId dbref a = {a & formDBRef = dbref}

storeFormflow dynFormFlow
	=						enterInformation "Define name to store:"
		>>= \extname ->		dbReadAll
		>>= \all ->			if	(isMember extname [this.dynFormFlowName \\ this <- all])
								(			requestConfirmation ("Name already exists, do you want to overwrite?")
								 >>= \ok -> if ok (updateItem all extname) (storeFormflow dynFormFlow)
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
			>>|				return dynFormFlow

		storeItem name
			=				dbCreateItem
			>>= \oform ->	dbUpdateItem {oform & dynFormFlowName = name, dynFormFlowType = type, dynFormFlow = dynFormFlow}
			>>|				return dynFormFlow

readFormflow  dynFormFlow
	=						dbReadAll
		>>= \all ->			case all of
							 [] ->					updateInformation "No stored definitions can be found." Void
							 		>>|				return NoDynFormFlow
							 all ->					enterChoice "Choose definition you want to use:" [showName this \\ this <- all]
									>>= \choice ->	return (hd [this.dynFormFlow \\ this <- all | showName this == choice])
where
	showName this = this.dynFormFlowName +++ " :: " +++ this.dynFormFlowType

// ------------

makeFlow (Flow flow)
	=						updateInformation "Construct a flow:" flow.flowType
		>>= \flowType ->	try (checkIt flowType) (errorRaised flowType)
where
	checkIt flowType 
		=					checkFlows flowType
			>>= \dynFlow ->	showMessage ("Deduced type: " +++ showDynType dynFlow)
			>>|				return (Flow {flow & flowType = flowType, dynFlow = dynFlow})

	errorRaised :: [FlowType] String -> Task DynFormFlow
	errorRaised flowType s
		=					showMessage s >>| return (Flow {flow & flowType = flowType})					

checkFlows :: [FlowType] -> Task Dynamic  
checkFlows [] 		= throw "Cannot apply empty flow."
checkFlows flows 	= mapMonad translate flows >>= \dyns -> return (applyFlows (hd dyns) (tl dyns))
where
	mapMonad fun [] 	= return []
	mapMonad fun [d:ds] = fun d >>= \nd -> mapMonad fun ds >>= \nds -> return [nd:nds] 

	translate :: FlowType -> Task Dynamic
	translate (FormFromStore name) 	= findValue name
	translate (FlowFromStore name) 	= findFlow name
	translate (Editor editorInfo) 	= return (dynamic (\v -> assignTask editorInfo v) :: A.a: a -> Task a | iTask a)
	where
		assignTask info=:{assignTo = Just some} v 		
			= assign some.idOfUser NormalPriority Nothing (updateInformation info.prompt v <<@ some.taskName)
		assignTask info v 		
			= updateInformation info.prompt v
	translate Return			  	= return (dynamic (\v -> return v) :: A.a: a -> Task a | iTask a)
	translate First				  	= return (dynamic fst :: A.a b: (a,b) -> a)
	translate Second			  	= return (dynamic snd :: A.a b: (a,b) -> b)
//	translate (Or left right)		= checkFlows left >>= \leftflow -> checkFlows right >>= \rightflow -> checkOr leftflow rightflow
//	translate (And left right)		= checkFlows left >>= \leftflow -> checkFlows right >>= \rightflow -> checkAnd leftflow rightflow

//	checkOr (T ta :: T (Task a) a) (T tb :: T (Task a) a)  
//		= return (dynamic T (ta -||- tb) :: T (Task a) a)
//	checkOr (T ta :: T a b) (T tb :: T c d)  
//		= throw "Or: Cannot unify "

//	checkAnd (T ta :: T (Task a) a) (T tb :: T (Task b) b)  
//		= return (dynamic T (ta -&&- tb) :: T (Task (a,b)) (a,b))

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
applyFlows (T v :: T a a)  [edit :: A.b: b -> Task b | iTask b : dyns]
	= applyFlows (dynamic T (edit v) :: T (Task a) a) dyns
applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task b | iTask b ): dyns]
	= applyFlows (dynamic T (t >>= btb) :: T (Task a) a) dyns
applyFlows (T ta :: T (Task a) a)  [(T tb :: T (Task b) b): dyns]
	= applyFlows (dynamic T (ta >>| tb) :: T (Task b) b) dyns
applyFlows (x :: a)  [(f :: a -> b): dyns]
	= applyFlows (dynamic f x :: b) dyns

//applyFlows (T v :: T (c,d) (c,d))  [(fst ::  A.a b: (a,b) -> a) : dyns]
//	= applyFlows (dynamic T (fst v) :: T c c) dyns

applyFlows d ds
	= dynamic "Could not parse defined dynamic flow."

// ------------

startFlow f=:(Flow flow) 
	= 				getCurrentUser
		>>= \me ->	evalFlow me flow.dynFlow
		>>|			return f
where
	evalFlow me (T t:: T (Task a) a)	= spawnProcess me.userId True t >>| return Void
	evalFlow me d=:(T v:: T a a)		= showMessage (showDynValType "Result" (dynamic v :: a))
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
dynErrorMess s d1 = s +++ ", Type Error: " +++ showDynType d1

dynError2 s d1 d2 = return (dynamic T (dynErrorMess2 s d1 d2):: T String String)
dynErrorMess2 s d1 d2 = s +++ ", Cannot Unify: " +++ showDynType d1 +++ " with "  +++ showDynType d2

// ****************

/*
:: T2 a b = T2 a & TC b

f :: Dynamic -> Dynamic
f (T2 (x,y) :: T2 (a,b) (a,b)) = dynamic x :: T2 a a

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
*/
