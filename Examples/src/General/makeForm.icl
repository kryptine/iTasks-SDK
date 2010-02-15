module makeForm
 
import 	iTasks, CommonDomain, GeoDomain
from 	StdFunc import o
from	EstherBackend import toStringDynamic
from	StdMisc import abort
				
derive gPrint 		Form, Flow, FormShape, FlowShape, AssignInfo, FormStore, FlowStore, Tup
derive gParse 		Form, Flow, FormShape, FlowShape, AssignInfo, FormStore, FlowStore, Tup
derive gUpdate 		Form, Flow, FormShape, FlowShape, AssignInfo, FormStore, FlowStore, Tup
derive gVisualize 	Form, Flow, FormShape, FlowShape, AssignInfo, FormStore, FlowStore

derive bimap Maybe, (,)

Start :: *World -> *World 
Start w = startEngine dynEditor w

dynEditor :: [Workflow]
dynEditor
= 	[ workflow "Interactive Workflows/1. FORM Editor" 		noForm
	, workflow "Interactive Workflows/2. FLOW Editor" 		noFlow
	, workflow "Interactive Workflows/3. Show Definitions"	showAll
	, workflow "Interactive Workflows/4. Run FLOW"			loopStart
  	]

:: T  a b	= T  !a & iTask b
//:: T2 a b c	= T2 !a & iTask b & iTask c

:: Form			= 	{ formShape :: ![FormShape]
			  		, formDyn	:: !Dynamic
			  		}
:: Flow			= 	{ flowShape :: ![FlowShape]
			  		, flowDyn 	:: !Dynamic
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
:: FlowShape	= 	Editor 			!String	
				| 	DisplayIt		!String 		
				| 	Return
				|	Assign			!AssignInfo ![FlowShape]
				| 	Or  !([FlowShape], ![FlowShape])
				| 	And !([FlowShape], ![FlowShape])
				|	FormFromStore 	!String
				|	FlowFromStore 	!String
				| 	First
				| 	Second
:: AssignInfo	= 	{ idOfUser	:: !Int
			  		, taskName 	:: !String
			  		}
:: FormStore	= 	{ formName 	:: !String
					, formType	:: !String
			  		, form 		:: !Form
			  		, formDBRef :: !DBRef !FormStore
			  		}
:: FlowStore	= 	{ flowName 	:: !String
					, flowType	:: !String
			  		, flow 		:: !Flow
			  		, flowDBRef :: !DBRef !FlowStore
			  		}
emptyForm 		= 	{ formShape = []
					, formDyn = dynamic T Void :: T Void Void
					}
emptyFlow 		= 	{ flowShape = []
					, flowDyn = dynamic T  Void :: T Void Void
					}
// ------------

undef = undef 

New 		:== ActionLabel "New"
Exit 		:== ActionLabel "Exit"
Read 		:== ActionLabel "Read"
ReadForm	:== ActionLabel "Read Form"
ReadShape	:== ActionLabel "Read Shape"
Store 		:== ActionLabel "Store"
StartFlow 	:== ActionLabel "Start Flow"
Check 		:== ActionLabel "Type Check"
Refresh		:== ActionLabel "Refresh"

// 1. FORM Editor

noForm :: Task !Void
noForm 
	=					showMessageA "FORM Editor, Welcome..." [New, ReadShape, ReadForm, Exit]
		>>= \choice -> 	case choice of
						 	New			-> newFormName emptyForm >>= editFormShape
						 	ReadForm	-> readForm >>= editForm	
						 	ReadShape	-> readForm >>= editFormShape	
						 	Exit		-> return Void

editFormShape :: !(!String, !Form) -> Task Void 
editFormShape (name, form)
	=					updateInformationA ("FORM SHAPE Editor of form *" +++ name +++ "* :") [New, ReadShape, Exit] [ActionNext] form.formShape 
	 >>= \(choice,formShape) -> 
			case choice of
				ActionNext	-> 					shapeToForm formShape 
								>>= \formDyn -> editForm (name, {formShape = formShape, formDyn = formDyn})
				New			-> newFormName emptyForm >>= editFormShape
				ReadShape	-> readForm >>= editFormShape
				_			-> return Void

editForm :: !(!String, !Form) -> Task Void 
editForm (name,form=:{formDyn = (T v :: T a a)})
	=		updateInformationA ("FORM Editor of form *" +++ name +++ "* :") [ActionPrevious] [New, ReadForm, Store, Exit] v 
	 	>>= editForm2 
where
	editForm2 :: (Action,a) -> Task Void | iTask a
	editForm2 (choice,nv) 
	# form2 = {form & formDyn = dynamic T nv :: T a^ a^} 
	= case choice of
			ActionPrevious	-> editFormShape (name, form)
			New				-> newFormName emptyForm >>= editForm
			ReadForm		-> readForm	>>= editForm
			Store			-> storeForm (name,form2) >>= editForm
			_				-> return Void

// 2. FLOW Editor

noFlow :: Task Void
noFlow 
	=					showMessageA "FLOW Editor, Welcome..." [New, Read, Exit]
		>>= \choice -> 	case choice of
						 	New		-> newFlowName emptyFlow >>= editFlowShape
						 	Read	-> readFlow >>= editFlowShape	
						 	_		-> return Void

editFlowShape :: !(!String, !Flow) -> Task Void 
editFlowShape (name, flow)
	=					updateInformationA ("FLOW Editor of flow *" +++ name +++ "* :") [New, Exit, Read] [Check, ActionNext] flow.flowShape 
	 >>= \(choice,flowShape) -> 
			case choice of
				New			-> newFlowName emptyFlow >>= editFlowShape
				Read		-> readFlow >>= editFlowShape
				Check		-> try (checkIt flowShape) (errorRaised flowShape) >>= \flow -> editFlowShape (name, flow)
				ActionNext	-> try  (checkIt flowShape 		   >>= \flow -> finalizeFlow (name,flow)) 
									(\s -> errorRaised flowShape s >>= \flow -> editFlowShape (name, flow))
				_			-> return Void

where
	checkIt :: ![FlowShape] -> Task Flow
	checkIt flowShape 
		=					checkFlows flowShape
			>>= \flowDyn ->	if (validType flowDyn) 
								(return {flowShape = flowShape, flowDyn = flowDyn}) 
								(throw (dynErrorMess "not a legal workflow, " flowDyn))
			>>|				return {flowShape = flowShape, flowDyn = flowDyn}

	validType :: Dynamic -> Bool
	validType (T x :: T (Task a) a) 											= True
	validType (T x :: T (a -> Task a) a) 										= True
	validType (T x :: T (a -> Task b) b) 										= True
	validType (T x :: T (a -> Task b) (a,b)) 									= True
	validType (f :: A.a: a -> Task a | iTask a) 								= True
	validType (f :: A.a: a -> Task (t a) 	 	| iTask a)						= True
	validType (f :: A.a: a -> Task (t a a) 	 	| iTask a)						= True
	validType (f :: A.a b: a -> Task (t a b) 	| iTask a & iTask b ) 			= True
//	validType (f :: A.a b: a -> Task (t a b) 	| iTask (a,b) ) 				= True
	validType (f :: A.a: a -> Task (t a a a) 	| iTask a)						= True
	validType (f :: A.a b c: a -> Task (t a b c)| iTask a & iTask b & iTask c) 	= True
	validType d																	= False
	
	errorRaised :: [FlowShape] String -> Task Flow
	errorRaised flowShape s
		=					showMessage ("Type Error: " +++ s) >>| return {flow & flowShape = flowShape}					

finalizeFlow :: !(!String, !Flow) -> Task Void 
finalizeFlow (name, flow)
	=					showMessageA ("You may now store flow *" +++ name +++ "* :: " +++ showDynType flow.flowDyn) [ActionPrevious, Store, Exit]
	 >>= \(choice) -> 
			case choice of
				New				-> newFlowName emptyFlow >>= editFlowShape
				Read			-> readFlow >>= editFlowShape
				ActionPrevious	-> editFlowShape (name, flow)
				Store			-> storeFlow (name, flow) >>= editFlowShape
				_				-> return Void

// ------------

loopStart :: Task Void
loopStart
	=	enterInformationA "Press start to run a stored workflow..." [] [StartFlow, Exit]
		>>= \(choice,Void) ->	
			case choice of
				StartFlow	-> startFlow	>>| loopStart 
				Exit		-> return Void

startFlow :: Task Void
startFlow
	= 						getCurrentUser
		>>= \me ->			readFlow 
		>>= \(_,flowDyn) ->	evalFlow me flowDyn.flowDyn 
where
	evalFlow me (T t:: T (Task a) a)	= spawnProcess me.userId True (t <<@ "dynamic flow")>>| return Void
	evalFlow me flow=:(t :: A.a: a -> Task a | iTask a)
										= 						readForm
											>>= \(name,_) -> findValue name
											>>= \dyn -> 	 evalFlow me (applyFlows dyn [flow]) 	
	evalFlow me (T v:: T a b)			= showMessage (showDynValType "Result" (dynamic v :: a))
	evalFlow me d						= showMessage (dynErrorMess "Eval" d) 


findFlow2 ::  Task !(!String, !Flow)
findFlow2   
	=						readAllFlows
		>>= \all ->			let names = [showName this \\ this <- all] in
								case names of
								 [] ->					updateInformation "No Flows stored !" Void
								 		>>|				return ("Temp", emptyFlow)
								 names ->				enterChoice "Choose Flow you want to use:" names
										>>= \choice ->	return (hd [(this.flowName, this.flow) \\ this <- all | showName this == choice])
where
	showName this = this.flowName +++ " :: " +++ this.flowType




// ------------

shapeToForm :: [FormShape] -> Task Dynamic
shapeToForm bs = convertFormShapes bs >>= return o tupling
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
		
// ------------

showAll :: Task Void
showAll
	=						readAllForms
		>>= \allForms 	->	readAllFlows
		>>= \allFlows	->	showMessageAboutA "Stored definitions:" [Refresh, Exit] (myForm allForms ++ myFlows allFlows) 
		>>= \choice		->	case choice of
								Refresh	->	showAll
								_		->	return Void
where
	myForm allForms 	= ["Stored Forms:", "" 		: [form.formName +++ " :: " +++ form.formType \\ form <- allForms]]
	myFlows allFlows 	= ["Stored Workflows:", ""  : [flow.flowName +++ " :: " +++ flow.flowType \\ flow <- allFlows]]

// ------------


checkFlows :: [FlowShape] -> Task Dynamic  
checkFlows [] 		= throw "Cannot apply empty flow."
checkFlows flows 	= mapMonad translate flows >>= \[d:ds] -> return (applyFlows d ds)
where
	mapMonad :: (!FlowShape -> Task Dynamic) [FlowShape] -> Task [Dynamic]	// leaving out the type crashes the compiler !!!
	mapMonad fun [] 	= return []
	mapMonad fun [d:ds] = fun d >>= \nd -> mapMonad fun ds >>= \nds -> return [nd:nds] 

	translate :: !FlowShape -> Task Dynamic
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
		checkAnd (T ta :: T (a -> Task b) b) (T tb :: T (a -> Task c) c)  
			= return (dynamic T (\a -> ta a -&&- tb a) :: T (a -> Task (b,c)) (b,c))
		checkAnd (ta :: A.a: a -> Task a | iTask a) (tb :: A.a: a -> Task a | iTask a)  
			= return (dynamic (\a -> ta a -&&- tb a) :: A.a: a -> Task (a,a) | iTask a )
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
	= 					readAllForms
		>>= \all ->		examine [this.form.formDyn \\ this <- all | this.formName == name]
where
	examine [] 							= throw ("Cannot find Form with name " +++ name)
	examine [form =: (T v :: T a a) :_] = return (dynamic T (return v) :: T (Task a) a )		// turn value into task as well
	examine [form:_]					= throw (dynErrorMess "Form has ilegal type:" form )

findFlow :: String -> Task Dynamic
findFlow name
	= 					readAllFlows
		>>= \all ->		examine [this.flow \\ this <- all | this.flowName == name]
where
	examine [] 			= throw ("Cannot find Flow with name " +++ name)
	examine [flow:_] 	= return flow.flowDyn

applyFlows :: Dynamic [Dynamic] -> Dynamic  
applyFlows dyn [] = dyn
applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task b | iTask b ): dyns]		// ta >>= edit
	= applyFlows (dynamic T (t >>= btb) :: T (Task a) a) dyns
applyFlows (T t :: T (Task a) a)  [(T btb :: T (a -> Task b) b ): dyns]				// ta >>= atb
	= applyFlows (dynamic T (t >>= btb) :: T (Task b) b) dyns
applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task Void | iTask b ): dyns]	// ta >>= show
	= applyFlows (dynamic T (t >>= btb) :: T (Task Void) Void) dyns

applyFlows (T ta :: T (Task a) a)  [(T tb :: T (Task b) b): dyns]					// ta >>| tb
	= applyFlows (dynamic T (ta >>| tb) :: T (Task b) b) dyns

applyFlows (x :: a)  [(f :: a -> b): dyns]											// common dyn apply
	= applyFlows (dynamic f x :: b) dyns

applyFlows d ds
	= dynamic "Could not parse defined dynamic flow."
/*
applyFlows (T2 v :: T2 (c,d) c d )  [(fst ::  A.a b: (a,b) -> a) : dyns]			// first 
	= applyFlows (dynamic T (fst v) :: T c c) dyns
applyFlows (T2 v :: T2 (c,d) c d)  [(snd ::  A.a b: (a,b) -> b) : dyns]				// second
	= applyFlows (dynamic T (snd v) :: T d d) dyns
applyFlows (T2 v :: T2 (c,d) c d) dyns												// T2 -> T!
	= applyFlows (dynamic T v :: T (c,d) (c,d)) dyns
*/

// ------------

instance DB FormStore where
	databaseId	:: DBid [FormStore]
	databaseId = mkDBid "FormStore"
	
	getItemId	:: FormStore -> DBRef FormStore
	getItemId a = a.formDBRef

	setItemId	:: (DBRef FormStore) FormStore -> FormStore
	setItemId dbref a = {a & formDBRef = dbref}

instance DB FlowStore where
	databaseId	:: DBid [FlowStore]
	databaseId = mkDBid "FlowStore"
	
	getItemId	:: FlowStore -> DBRef FlowStore
	getItemId a = a.flowDBRef

	setItemId	:: (DBRef FlowStore) FlowStore -> FlowStore
	setItemId dbref a = {a & flowDBRef = dbref}

readAllForms :: Task [FormStore]
readAllForms = dbReadAll

readAllFlows :: Task [FlowStore]
readAllFlows = dbReadAll

newFormName :: !Form -> Task !(!String, !Form)
newFormName form
	=						enterInformation "Give name of new Form:" 
		>>= \name ->		readAllForms
		>>= \allForms ->	case [this \\ this <- allForms | this.formName == name] of
								[] -> 					    dbCreateItem 
											>>= \item -> 	dbUpdateItem {item & form = form, formType = showDynType form.formDyn, formName = name}
											>>|				return (name,form) 
								found ->	requestConfirmation ("Name already exists, do you want to overwrite" +++ (hd found).formType)
								 			>>= \ok -> if ok (return (name,form)) (newFormName form)

newFlowName :: !Flow -> Task !(!String, !Flow)
newFlowName flow
	=						enterInformation "Give name of new flow:" 
		>>= \name ->		readAllFlows
		>>= \allFlows ->	case [this \\ this <- allFlows | this.flowName == name] of
								[] -> 						dbCreateItem 
											>>= \item -> 	dbUpdateItem {item & flow = flow, flowType = showDynType flow.flowDyn, flowName = name} 
											>>|				return (name,flow) 
								found ->	requestConfirmation ("Name already exists, do you want to overwrite" +++ (hd found).flowType )
								 			>>= \ok -> if ok (return (name,flow)) (newFlowName flow)
readForm :: Task !(!String, !Form)
readForm   
	=						readAllForms
		>>= \all ->			let names = [showName this \\ this <- all] in
								case names of
								 [] ->					updateInformation "No Forms stored !" Void
								 		>>|				return ("Temp", emptyForm)
								 names ->				enterChoice "Choose Form you want to use:" names
										>>= \choice ->	return (hd [(this.formName, this.form) \\ this <- all | showName this == choice])
where
	showName this = this.formName +++ " :: " +++ this.formType

readFlow ::  Task !(!String, !Flow)
readFlow   
	=						readAllFlows
		>>= \all ->			let names = [showName this \\ this <- all] in
								case names of
								 [] ->					updateInformation "No Flows stored !" Void
								 		>>|				return ("Temp", emptyFlow)
								 names ->				enterChoice "Choose Flow you want to use:" names
										>>= \choice ->	return (hd [(this.flowName, this.flow) \\ this <- all | showName this == choice])
where
	showName this = this.flowName +++ " :: " +++ this.flowType

newName fun f 
	=		enterInformation "Type in another name " >>= \name -> fun (name, f)

storeForm :: !(String, !Form) -> Task !(!String, !Form) // item assumed to be in store
storeForm (name, form)
	=						readAllForms
		>>= \all ->			return (hd [this \\ this <- all | this.formName == name])
		>>= \formStore ->	dbUpdateItem {formStore & formType = showDynType form.formDyn, form = form}
		>>|					return (name,form)

storeFlow :: !(String, !Flow) -> Task !(!String, !Flow) // item assumed to be in store
storeFlow (name, flow)
	=						readAllFlows
		>>= \all ->			return (hd [this \\ this <- all | this.flowName == name])
		>>= \flowStore -> 	dbUpdateItem {flowStore & flowName = name, flowType = showDynType flow.flowDyn, flow = flow}
		>>|					return (name,flow)

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

