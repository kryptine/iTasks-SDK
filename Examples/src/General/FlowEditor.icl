implementation module FlowEditor
 
import 	iTasks
import	FlowData, FormFlowStorage, TaskContainer

from 	StdFunc import o
				
flowEditor :: Workflow
flowEditor = workflow "Interactive Workflows/Flow Editor" noFlow

emptyFlow 		= 	{ flowShape = []
					, flowDyn = dynamic T  Void :: T Void Void
					}

// ****************************

New 		:== ActionLabel "New"
Exit 		:== ActionLabel "Exit"
Read 		:== ActionLabel "Read"
Store 		:== ActionLabel "Store"
Check 		:== ActionLabel "Type Check"
Refresh		:== ActionLabel "Refresh"

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
								(throw (typeErrorMess "not a legal workflow, " flowDyn))
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

// ****************************

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
			= throw (typeErrorMess2 "Or" d1 d2)

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
			= throw (typeErrorMess2 "And" d1 d2)
	
	translate (Assign assignInfo flow)	= checkFlows flow >>= assignTask assignInfo
	where
		assignTask :: !AssignInfo !Dynamic -> Task Dynamic
		assignTask info (e :: A.a: a -> Task a | iTask a) 		
			= return (dynamic (apply info e) :: A.a: a -> Task a | iTask a)
		where
			apply :: !AssignInfo !(A.a: a -> Task a | iTask a) b -> Task b | iTask b
			apply info e v = assign info.nameOfUser NormalPriority Nothing (e v <<@ info.taskName)
		assignTask info d 		
			= throw (typeErrorMess "Assign" d)

	translate First				  	= return (dynamic fst :: A.a b: (a,b) -> a)
	translate Second			  	= return (dynamic snd :: A.a b: (a,b) -> b)
	translate (FormFromStore name) 	= findValue name
	translate (FlowFromStore name) 	= findFlow name

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

applyFlows (ta :: A.a: a -> Task a | iTask a)  [(tb:: A.b: b -> Task b | iTask b): dyns] // ta >>= tb
	= applyFlows (dynamic \a -> ta a >>= tb :: A.a: a -> Task a | iTask a) dyns

applyFlows (x :: a)  [(f :: a -> b): dyns]											// common dyn apply
	= applyFlows (dynamic f x :: b) dyns

applyFlows d ds
	= dynamic "Could not parse defined dynamic flow."




