implementation module FlowData
 
import 	iTasks, TaskContainer
from FormFlowStorage import findValue, findFlow

				
derive gPrint 		Flow, FlowShape, AssignInfo, CleanExpr
derive gParse 		Flow, FlowShape, AssignInfo, CleanExpr
derive gUpdate 		Flow, FlowShape, AssignInfo, CleanExpr
derive gVisualize 	Flow, FlowShape, AssignInfo, CleanExpr

derive bimap	Maybe, (,)

:: Flow			= 	{ flowShape :: ![FlowShape]
			  		, flowDyn 	:: !Dynamic
			  		}
:: FlowShape	= 	Editor 			!String	
				| 	DisplayIt		!String 		
				| 	Return
				|	Assign			!AssignInfo ![FlowShape]
				| 	Or  !([FlowShape], ![FlowShape])
				| 	And !([FlowShape], ![FlowShape])
				|	FormFromStore 	!String
				|	FlowFromStore 	!String
				|	CleanExpr		!CleanExpr
				| 	First
				| 	Second
:: AssignInfo	= 	{ nameOfUser:: !String
			  		, taskName 	:: !String
			  		}
:: CleanExpr	=	CI Int
				|	CR Real
				|	CB Bool
				|	CS String
				|	VoidVal
				|	CE String

emptyFlow :: Flow
emptyFlow 		= 	{ flowShape = []
					, flowDyn = dynamic "Flow not initialized" :: String
					}

flowShapeToFlow :: ![FlowShape] -> Task Flow
flowShapeToFlow flowShape 
	=					flowShapeToFlowDyn flowShape
		>>= \flowDyn ->	if (validTask flowDyn || validTaskFun flowDyn) 
							(return {flowShape = flowShape, flowDyn = flowDyn}) 
							(throw (typeErrorMess "not a legal workflow, " flowDyn))
		>>|				return {flowShape = flowShape, flowDyn = flowDyn}

		
flowShapeToFlowDyn :: ![FlowShape] -> Task Dynamic  
flowShapeToFlowDyn [] 		= throw "A list in a flow has to be non empty."
flowShapeToFlowDyn flows 	= mapMonad translate flows >>= \[d:ds] -> return (applyFlows d ds)
where
	mapMonad :: (!FlowShape -> Task Dynamic) [FlowShape] -> Task [Dynamic]	// leaving out the type crashes the compiler !!!
	mapMonad fun [] 	= return []
	mapMonad fun [d:ds] = fun d >>= \nd -> mapMonad fun ds >>= \nds -> return [nd:nds] 

translate :: !FlowShape -> Task Dynamic
translate (Editor prompt)		= return (dynamic DynTaskFun (updateInformation prompt):: A.a: DynTaskFun a a | iTask a)
translate (DisplayIt prompt)	= return (dynamic DynTaskFun (showMessageAbout prompt):: A.a: DynTaskFun a Void | iTask a)
translate Return			  	= return (dynamic DynTaskFun (\v -> return v) :: A.a: DynTaskFun a a | iTask a)

translate (Or (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkOr leftflow rightflow
where
	checkOr :: Dynamic Dynamic -> Task Dynamic
	checkOr (DynTaskFun ta1 :: DynTaskFun a b) 	(DynTaskFun ta2 :: DynTaskFun a b)  = return (dynamic DynTaskFun (\a -> ta1 a -||- ta2 a) :: DynTaskFun a b)
	checkOr d1 d2																	= throw (typeErrorMess2 "Or" d1 d2)

translate (And (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkAnd leftflow rightflow
where
	checkAnd :: Dynamic Dynamic -> Task Dynamic					
	checkAnd (DynTaskFun tab :: DynTaskFun a b) (DynTaskFun tac :: DynTaskFun a c)  = return (dynamic DynTaskFun (\a -> tab a -&&- tac a) :: DynTaskFun a (b,c)) 
	checkAnd (DynTaskFun tab :: A.a: DynTaskFun a a) (DynTaskFun tac :: A.b: DynTaskFun b b)  = return (dynamic DynTaskFun (\a -> tab a -&&- tac a) :: A.a: DynTaskFun a (a,a)) 
	checkAnd (DynTaskFun tab :: A.a: DynTaskFun a c) (DynTaskFun tac :: A.b: DynTaskFun b d)  = return (dynamic DynTaskFun (\a -> tab a -&&- tac a) :: A.a: DynTaskFun a (c,d)) 
	checkAnd d1 d2																	= throw (typeErrorMess2 "And" d1 d2)

translate (Assign info flow)	= flowShapeToFlowDyn flow >>= assignTask info
where
	assignTask :: !AssignInfo !Dynamic -> Task Dynamic
	assignTask info (e :: A.a: a -> Task a | iTask a) 	= return (dynamic (\v -> assign info.nameOfUser NormalPriority Nothing (e v <<@ info.taskName)) :: A.a: a -> Task a | iTask a)
	assignTask info d 									= throw (typeErrorMess "Assign" d)

translate First				  	= return (dynamic DynTaskFun (\(a,b) -> return a) :: A.a b: DynTaskFun (a,b) a |iTask a)
translate Second			  	= return (dynamic DynTaskFun (\(a,b) -> return b) :: A.a b: DynTaskFun (a,b) b |iTask b)
translate (FormFromStore name) 	= findValue name
translate (FlowFromStore name) 	= findFlow name

translate (CleanExpr VoidVal )		= return (dynamic DynTask (return Void) 	:: DynTask Void) 
translate (CleanExpr (CI i))		= return (dynamic DynTask (return i) 		:: DynTask Int) 
translate (CleanExpr (CR r))		= return (dynamic DynTask (return r) 		:: DynTask Real) 
translate (CleanExpr (CB b))		= return (dynamic DynTask (return b) 		:: DynTask Bool) 
translate (CleanExpr (CS s))		= return (dynamic DynTask (return s) 		:: DynTask String) 
translate (CleanExpr (CE s))		= return (dynamic "Not implemented")

applyDynFlows :: ![Dynamic] -> Dynamic 
applyDynFlows [] 		= dynamic "Cannot apply empty list of flows"
applyDynFlows [h:tl] 	= applyFlows h tl

undef = undef

applyFlows :: Dynamic [Dynamic] -> Dynamic 
applyFlows dyn [] = dyn

applyFlows (DynTaskVal a::DynTaskVal a)	[(DynTaskFun tab :: DynTaskFun a b): dyns]					= applyFlows (dynamic DynTask (tab a) :: DynTask b) dyns
applyFlows (DynTaskVal a::DynTaskVal a)	[(DynTaskFun tab :: A.c: DynTaskFun c b | iTask c): dyns] 	= applyFlows (dynamic DynTask (tab a) :: DynTask b) dyns
applyFlows (DynTaskVal a::DynTaskVal a)	[(DynTaskFun tab :: A.c: DynTaskFun c c | iTask c): dyns] 	= applyFlows (dynamic DynTask (tab a) :: DynTask a) dyns

applyFlows (DynTask ta::DynTask a)  	[(DynTaskFun tab :: DynTaskFun a b): dyns]					= applyFlows (dynamic DynTask (ta >>= tab) :: DynTask b) dyns
applyFlows (DynTask ta::DynTask a)  	[(DynTaskFun tab :: A.c: DynTaskFun c b | iTask c): dyns]	= applyFlows (dynamic DynTask (ta >>= tab) :: DynTask b) dyns
applyFlows (DynTask ta::DynTask a)  	[(DynTaskFun tab :: A.c: DynTaskFun c c | iTask c): dyns]	= applyFlows (dynamic DynTask (ta >>= tab) :: DynTask a) dyns

applyFlows (DynTaskFun tab::DynTaskFun a b)	[(DynTaskFun tbc :: DynTaskFun b c): dyns]					= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) :: DynTaskFun a c) dyns
applyFlows (DynTaskFun tab::DynTaskFun a b)	[(DynTaskFun tbc :: A.c: DynTaskFun c d | iTask c): dyns]	= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) :: DynTaskFun a d) dyns
applyFlows (DynTaskFun tab::DynTaskFun a b)	[(DynTaskFun tbc :: A.c: DynTaskFun c c | iTask c): dyns]	= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) :: DynTaskFun a b) dyns

//applyFlows (DynTaskFun tab :: A.a: DynTaskFun a b | iTask a) [(DynTaskFun tbc :: A.c: DynTaskFun c d | iTask c): dyns]	= undef
//																									= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) :: A.b: DynTaskFun b d | iTask b) dyns

//applyFlows (DynTaskFun tab :: A.a: DynTaskFun a a | iTask a) [(DynTaskFun tbc :: A.c: DynTaskFun c c | iTask c): dyns]	= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) :: A.a: DynTaskFun a a | iTask a) dyns
//applyFlows (DynTaskFun tab :: A.a: DynTaskFun a b | iTask a) [(DynTaskFun tbc :: DynTaskFun b c): dyns]	= applyFlows (dynamic DynTaskFun (\a -> tab a >>= tbc) ::  DynTaskFun b c) dyns

applyFlows d [d1:_]																		= dynamic  (typeErrorMess2 "Cannot apply" d d1)

				
/*
translate :: !FlowShape -> Task Dynamic
translate (Editor prompt)		= return (dynamic (updateInformation prompt):: A.a: a -> Task a | iTask a)
translate (DisplayIt prompt)	= return (dynamic (showMessageAbout prompt)::  A.a: a -> Task Void | iTask a)
translate Return			  	= return (dynamic (\v -> return v) :: A.a: a -> Task a | iTask a)

translate (Or (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkOr leftflow rightflow
where
	checkOr :: Dynamic Dynamic -> Task Dynamic
	checkOr (T ta :: T (a -> Task a) a) 		(T tb :: T (a -> Task a) a)  		= return (dynamic T (\a -> ta a -||- tb a) :: T (a -> Task a) a)
	checkOr (ta :: A.a: a -> Task a | iTask a) 	(tb :: A.b: b -> Task b | iTask b)	= return (dynamic (\a -> ta a -||- tb a)   :: A.a: a -> Task a | iTask a)
	checkOr (T ta :: T (a -> Task a) a) 		(tb :: A.b: b -> Task b | iTask b)  = return (dynamic T (\a -> ta a -||- tb a) :: T (a -> Task a) a)
	checkOr (tb :: A.b: b -> Task b | iTask b)	(T ta :: T (a -> Task a) a)  		= return (dynamic T (\a -> ta a -||- tb a) :: T (a -> Task a) a)
	checkOr d1 d2																	= throw (typeErrorMess2 "Or" d1 d2)

translate (And (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkAnd leftflow rightflow
where
	checkAnd :: Dynamic Dynamic -> Task Dynamic					
	checkAnd (T ta :: T (a -> Task b) b) 		(T tb :: T (a -> Task c) c)  		= return (dynamic T (T (\a -> ta a -&&- tb a)) :: T (T (a -> Task (b,c)) b) c)
	checkAnd (ta :: A.a: a -> Task a | iTask a) (tb :: A.a: a -> Task a | iTask a)  = return (dynamic (\a -> ta a -&&- tb a) :: A.a: a -> Task (a,a) | iTask a )
	checkAnd d1 d2																	= throw (typeErrorMess2 "And" d1 d2)

translate (Assign info flow)	= flowShapeToFlowDyn flow >>= assignTask info
where
	assignTask :: !AssignInfo !Dynamic -> Task Dynamic
	assignTask info (e :: A.a: a -> Task a | iTask a) 	= return (dynamic (\v -> assign info.nameOfUser NormalPriority Nothing (e v <<@ info.taskName)) :: A.a: a -> Task a | iTask a)
	assignTask info d 									= throw (typeErrorMess "Assign" d)

translate First				  	= return (dynamic \(a,b) -> return a :: A.a b: (a,b) -> Task a |iTask a)
translate Second			  	= return (dynamic \(a,b) -> return b :: A.a b: (a,b) -> Task b |iTask b)
translate (FormFromStore name) 	= findValue name
translate (FlowFromStore name) 	= findFlow name

translate (CleanExpr VoidVal )		= return (dynamic T (return Void) 	:: T (Task Void) Void) 
translate (CleanExpr (CI i))		= return (dynamic T (return i) 		:: T (Task Int) Int) 
translate (CleanExpr (CR r))		= return (dynamic T (return r) 		:: T (Task Real) Real) 
translate (CleanExpr (CB b))		= return (dynamic T (return b) 		:: T (Task Bool) Bool) 
translate (CleanExpr (CS s))		= return (dynamic T (return s) 		:: T (Task String) String) 
translate (CleanExpr (CE s))		= return (dynamic "Not implemented")

applyDynFlows :: ![Dynamic] -> Dynamic 
applyDynFlows [] 		= dynamic "Cannot apply empty list of flows"
applyDynFlows [h:tl] 	= applyFlows h tl

applyFlows :: Dynamic [Dynamic] -> Dynamic 
applyFlows dyn [] = dyn

applyFlows (T ta :: T (Task a) a)  [(btb :: A.b: b -> Task b | iTask b): dyns]				= applyFlows (dynamic T (ta >>= btb) :: T (Task a) a) dyns
// applyFlows (T ta :: T (Task a) a)  [(btb :: A.b: b -> Task (t b b) | iTask b): dyns]		= applyFlows (dynamic T (ta >>= btb) :: T (Task (t a a)) a) dyns

//JOHN: dit geeft een overloading error, stand alone werkt het wel..

applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Void  | iTask b): dyns]			= applyFlows (dynamic T (t >>= btb) :: T (Task Void)   Void) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Int   | iTask b): dyns]			= applyFlows (dynamic T (t >>= btb) :: T (Task Int)    Int) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Real  | iTask b): dyns]			= applyFlows (dynamic T (t >>= btb) :: T (Task Real)   Real) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Bool  | iTask b): dyns]			= applyFlows (dynamic T (t >>= btb) :: T (Task Bool)   Bool) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task String| iTask b): dyns]			= applyFlows (dynamic T (t >>= btb) :: T (Task String) String) dyns

applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Void  ): dyns]					= applyFlows (dynamic T (t >>= btb) :: T (Task Void)   Void) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Int   ): dyns]					= applyFlows (dynamic T (t >>= btb) :: T (Task Int)    Int) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Real  ): dyns]					= applyFlows (dynamic T (t >>= btb) :: T (Task Real)   Real) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task Bool  ): dyns]					= applyFlows (dynamic T (t >>= btb) :: T (Task Bool)   Bool) dyns
applyFlows (T t :: T (Task a) a)   [(btb :: A.b: b -> Task String): dyns]					= applyFlows (dynamic T (t >>= btb) :: T (Task String) String) dyns

applyFlows (T t :: T (Task a) a)  [(T btb :: T (a -> Task b) b ): dyns]						= applyFlows (dynamic T (t >>= btb) :: T (Task b) b) dyns
//applyFlows (T t :: T (Task a) a)  [(T btb :: T (a -> Task (t b b)) b ): dyns]				= applyFlows (dynamic T (t >>= btb) :: T (Task (t b b)) b) dyns
//applyFlows (T t :: T (Task a) a)  [(T btb :: T (T (a -> Task (t b c)) b) c ): dyns]		= applyFlows (dynamic T (t >>= btb) :: T (T (Task (t b c)) b) c) dyns

applyFlows (ta :: A.a: a -> Task a | iTask a)  [(tb:: A.b: b -> Task b | iTask b): dyns]	= applyFlows (dynamic \a -> ta a >>= tb :: A.a: a -> Task a | iTask a) dyns
applyFlows (ta :: A.a: a -> Task a | iTask a)  [(tb:: A.b: b -> Task Void | iTask b): dyns]	= applyFlows (dynamic \a -> ta a >>= tb :: A.a: a -> Task Void | iTask a) dyns

applyFlows d [d1:_]																			= dynamic  (typeErrorMess2 "Cannot apply" d d1)

*/
