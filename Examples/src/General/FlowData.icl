implementation module FlowData
 
import 	iTasks, TaskContainer
from FormFlowStorage import findValue, findFlow

// import get_dictionary

				
derive gPrint 		Flow, FlowShape, AssignInfo, CleanExpr
derive gParse 		Flow, FlowShape, AssignInfo, CleanExpr
derive gUpdate 		Flow, FlowShape, AssignInfo, CleanExpr
derive gVisualize 	Flow, FlowShape, AssignInfo, CleanExpr
derive gError	 	Flow, FlowShape, AssignInfo, CleanExpr
derive gHint	 	Flow, FlowShape, AssignInfo, CleanExpr


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
:: AssignInfo	= 	{ nameOfUser:: !UserName
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
	mapMonad :: (FlowShape -> Task Dynamic) [FlowShape] -> Task [Dynamic]	
	mapMonad fun [] 	= return []
	mapMonad fun [d:ds] = fun d >>= \nd -> mapMonad fun ds >>= \nds -> return [nd:nds] 

translate :: !FlowShape -> Task Dynamic
translate (Editor prompt)		= return (dynamic DF0 (updateInformation prompt)	:: A.a: DF0 a a 	| iTask a)

translate (DisplayIt prompt)	= return (dynamic DF0 (showMessageAbout prompt)		:: A.a: DF0 a Void 	| iTask a)
translate Return			  	= return (dynamic DF0 (\v -> return v) 				:: A.a: DF0 a a 	| iTask a)

translate (Or (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkOr leftflow rightflow
where
	checkOr :: Dynamic Dynamic -> Task Dynamic
	checkOr (ta1 :: DF0 a b | iTask a) (ta2 :: DF0 a b | iTask a)  
		= return (dynamic (case (ta1,ta2) of
								(DF0 ta1, DF0 ta2) -> DF0 (\a -> ta1 a -||- ta2 a)) :: DF0 a b | iTask a)
	checkOr d1 d2
		= throw (typeErrorMess2 "Or" d1 d2)

translate (And (left, right))	= flowShapeToFlowDyn left >>= \leftflow -> flowShapeToFlowDyn right >>= \rightflow -> checkAnd leftflow rightflow
where
	checkAnd :: Dynamic Dynamic -> Task Dynamic					
	checkAnd (atb :: DF0 a b | iTask a) (tac :: DF0 a c | iTask a)  
		= return (dynamic (case (atb,tac) of
								(DF0 atb, DF0 tac) -> DF0 (\a -> atb a -&&- tac a)) :: DF0 a (b,c) | iTask a)
	checkAnd d1 d2	
		= throw (typeErrorMess2 "And" d1 d2)

translate (Assign info flow)	= flowShapeToFlowDyn flow >>= assignTask info
where
	assignTask :: !AssignInfo !Dynamic -> Task Dynamic
	assignTask info (e :: DF0 a b | iTask a) 	
		= return (dynamic (case e of
							(DF0 e) ->  DF0 (\v -> assign info.nameOfUser NormalPriority Nothing (e v <<@ info.taskName))) :: DF0 a b | iTask a)
	assignTask info d 									
		= throw (typeErrorMess "Assign" d)

translate First	
	= return (dynamic DF0 (\(a,b) -> return a) :: A.a b: DF0 (a,b) a | iTask a)
translate Second
	= return (dynamic DF0 (\(a,b) -> return b) :: A.a b: DF0 (a,b) b | iTask b)

translate (FormFromStore name) 	= findValue name
translate (FlowFromStore name) 	= findFlow name

translate (CleanExpr VoidVal )	= return (dynamic DT (return Void) 	:: DT Void) 
translate (CleanExpr (CI i))	= return (dynamic DT (return i)		:: DT Int) 
translate (CleanExpr (CR r))	= return (dynamic DT (return r)		:: DT Real) 
translate (CleanExpr (CB b))	= return (dynamic DT (return b)		:: DT Bool) 
translate (CleanExpr (CS s))	= return (dynamic DT (return s)		:: DT String) 
translate (CleanExpr (CE s))	= return (dynamic "Not implemented")

applyDynFlows :: ![Dynamic] -> Dynamic 
applyDynFlows [] 		= dynamic "Cannot apply empty list of flows"
applyDynFlows [h:tl] 	= applyFlows h tl


applyFlows :: Dynamic [Dynamic] -> Dynamic 

// first simplify unnecessary context restrictions

applyFlows (atb:: DF0 a b | iTask a & iTask a) dyns 			
	=  applyFlows (dynamic atb :: DF0 a b | iTask a ) dyns 						
applyFlows dyn [] 
	= dyn

// dynamic apply on flow: val |> a -> Tb 

applyFlows (DV0 a::DV0 a) [(DF0 atb :: DF0 a b): dyns] 						// a |> atb -> atb a 			
	= applyFlows (dynamic DT (atb a) :: DT b) dyns

applyFlows (DV0 a::DV0 a) [(DF0 atb :: DF0 a b | iTask a): dyns] 			// a |> atb | iT a -> atb a		
	= applyFlows (dynamic DT (atb a) :: DT b) dyns

// dynamic apply on flow: task |> a -> Tb 

applyFlows (DT ta::DT a) [(atb :: DF0 a b): dyns]							// ta |> atb -> ta >>= atb				
	= applyFlows (dynamic (case atb of
				(DF0 atb) -> DT (ta >>= atb)) :: DT b) dyns

applyFlows (DT ta::DT a) [(atb :: DF0 a b | iTask a): dyns]					// ta |> atb | iT a -> ta >>= atb					
	= applyFlows (dynamic (case atb of
				(DF0 atb) -> DT (ta >>= atb)) :: DT b) dyns

applyFlows (DT ta::DT a) [(atb :: DF0 a b | iTask b): dyns]					// ta |> atb | iT b -> ta >>= atb					
	= applyFlows (dynamic (case atb of
				(DF0 atb) -> DT (ta >>= atb)) :: DT b | iTask b) dyns

// dynamic apply on flow: a -> Tb |> b -> Tc 

applyFlows (atb:: DF0 a b) [(btc :: DF0 b c): dyns]							// ftaskb |> ftaskc -> ftaskb >>= ftaskc	
	=  applyFlows (dynamic (case atb of
								(DF0 atb)->
										case btc of
												(DF0 btc)-> DF0 (\a -> atb a >>= btc)) :: DF0 a c) dyns 

applyFlows (atb:: DF0 a b | iTask a) [(btc :: DF0 b c): dyns]				// ftaskb |> ftaskc -> ftaskb >>= ftaskc	
	=  applyFlows (dynamic (case atb of
								(DF0 atb)->
										case btc of
												(DF0 btc)-> DF0 (\a -> atb a >>= btc)) :: DF0 a c | iTask a ) dyns 

applyFlows (atb:: DF0 a b | iTask a) [(btc :: DF0 b c | iTask b): dyns]		// ftaskb |> ftaskc -> ftaskb >>= ftaskc	
	=  applyFlows (dynamic (case atb of
								(DF0 atb)->
										case btc of
												(DF0 btc)-> DF0 (\a -> atb a >>= btc)) :: DF0 a c | iTask a ) dyns 


applyFlows (atb:: DF0 a b | iTask a) [(btc :: DF0 b c | iTask c): dyns]		// ftaskb |> ftaskc -> ftaskb >>= ftaskc	
	=  applyFlows (dynamic (case atb of
								(DF0 atb)->
										case btc of
												(DF0 btc)-> DF0 (\a -> atb a >>= btc)) :: DF0 a c | iTask a & iTask c) dyns 

applyFlows d [d1:_]																		
	= dynamic  (typeErrorMess2 "Cannot apply" d d1)


				
