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
					, flowDyn = dynamic T  Void :: T Void Void
					}

flowShapeToFlow :: ![FlowShape] -> Task Flow
flowShapeToFlow flowShape 
	=					flowShapeToFlowDyn flowShape
		>>= \flowDyn ->	if (validType flowDyn) 
							(return {flowShape = flowShape, flowDyn = flowDyn}) 
							(throw (typeErrorMess "not a legal workflow, " flowDyn))
		>>|				return {flowShape = flowShape, flowDyn = flowDyn}
where
	validType :: Dynamic -> Bool
	validType (T x :: T (Task a) a) 												= True
	validType (T x :: T (a -> Task a) a) 											= True
	validType (T x :: T (a -> Task b) b) 											= True

	validType (f :: A.a: 		a -> Task a 		| iTask a) 						= True
	validType (f :: A.a: 		a -> Task (t a) 	| iTask a)						= True
	validType (f :: A.a: 		a -> Task (t a a) 	| iTask a)						= True
	validType (f :: A.a b: 		a -> Task (t a b) 	| iTask a & iTask b ) 			= True
	validType (f :: A.a: 		a -> Task (t a a a) | iTask a)						= True
	validType (f :: A.a b c: 	a -> Task (t a b c)	| iTask a & iTask b & iTask c) 	= True

	validType (f :: A.a: 		a -> Task Void) 									= True
	validType (f :: A.a: 		a -> Task Int) 										= True
	validType (f :: A.a: 		a -> Task Real) 									= True
	validType (f :: A.a: 		a -> Task Bool) 									= True
	validType (f :: A.a: 		a -> Task String) 									= True

	validType (f :: A.a: (Task a) -> Task a | iTask a) 								= True

	validType d																		= False
	
		
flowShapeToFlowDyn :: ![FlowShape] -> Task Dynamic  
flowShapeToFlowDyn [] 		= throw ("Flow list has to contain at least one element.")
flowShapeToFlowDyn flows 	= mapMonad translate flows >>= \[d:ds] -> return (applyFlows d ds)
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

translate (Or (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkOr leftflow rightflow
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

translate (And (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkAnd leftflow rightflow
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

translate (Assign info [])
	=	return (dynamic (\t -> assign info.nameOfUser NormalPriority Nothing (t <<@ info.taskName)) :: A.a: (Task a) -> Task a | iTask a)
translate (Assign info flow)	= flowShapeToFlowDyn flow >>= assignTask info
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

translate (CleanExpr (CI i))		= return (dynamic T (return i) :: T (Task Int) Int) 
translate (CleanExpr (CR r))		= return (dynamic T (return r) :: T (Task Real) Real) 
translate (CleanExpr (CB b))		= return (dynamic T (return b) :: T (Task Bool) Bool) 
translate (CleanExpr (CS s))		= return (dynamic T (return s) :: T (Task String) String) 
translate (CleanExpr VoidVal )	= return (dynamic T (return Void) :: T (Task Void) Void) 
translate (CleanExpr (CE s))		= interpret s

applyDynFlows :: ![Dynamic] -> Dynamic 
applyDynFlows [] 		= dynamic "Cannot apply empty list of flows"
applyDynFlows [h:tl] 	= applyFlows h tl

applyFlows :: Dynamic [Dynamic] -> Dynamic  
applyFlows dyn [] = dyn

applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task b | iTask b ): dyns]		
	= applyFlows (dynamic T (t >>= btb) :: T (Task a) a) dyns

applyFlows (T t :: T (Task a) a)  [(T btb :: T (a -> Task b) b ): dyns]				
	= applyFlows (dynamic T (t >>= btb) :: T (Task b) b) dyns

applyFlows (T t :: T (Task a) a)  [(btb :: A.b: b -> Task Void | iTask b ): dyns]	
	= applyFlows (dynamic T (t >>= btb) :: T (Task Void) Void) dyns

applyFlows (T ta :: T (Task a) a)  [(T tb :: T (Task b) b): dyns]					
	= applyFlows (dynamic T (ta >>| tb) :: T (Task b) b) dyns

applyFlows (ta :: A.a: a -> Task a | iTask a)  [(tb:: A.b: b -> Task b | iTask b): dyns]
	= applyFlows (dynamic \a -> ta a >>= tb :: A.a: a -> Task a | iTask a) dyns

//applyFlows (ta :: A.a: a -> Task a | iTask a)  [(tb:: A.b: b -> Task Void): dyns]
//	= applyFlows (dynamic \a -> ta a >>= tb :: A.c: c -> Task Void) dyns

applyFlows (tt :: A.a: (Task a) -> Task a | iTask a)  [(T ta :: T (Task a) a): dyns] 
	= applyFlows (dynamic T (tt ta) :: T (Task a) a) dyns

applyFlows (x :: a)  [(f :: a -> b): dyns]											// common dyn apply
	= applyFlows (dynamic f x :: b) dyns

applyFlows d [d1:_]
	= dynamic  (typeErrorMess2 "Cannot apply" d d1)


// ******************

interpret s = return (dynamic "Not implemented")
				









