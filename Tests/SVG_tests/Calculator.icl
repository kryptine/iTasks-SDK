module Calculator

import iTasks
import MultiUser

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow  "original calculator"  "calculator"       calculator
		 							, workflow  "graphical calculator" "svg calculator"   calculator2
									] world
	
// original calculator

:: CalculatorState = 	{ result	:: Int
						, digits	:: Int 
						}

derive class iTask CalculatorState

initState 				= {result = 0, digits = 0}
updateDigit digit st 	= {st & digits = st.digits*10 + digit}
applyOperator op st 	= {result = op st.result st.digits, digits = 0}

calculator :: Task Int
calculator = calc initState

calc st
= 		viewInformation "Calculator" [ViewWith Display] st
	>>* [ OnAction (Action (toString i) []) (always (calc (updateDigit i st))) 
		\\ i <- [0..9] 
		] ++
		[ OnAction (Action eventName []) (always (calc (applyOperator operator st)))
		\\ (eventName,operator) <- calcFunctions 
		]

calcFunctions = [("+",(+)),("-",(-)),("*",(*)),("/",(/)),("=",(+))]

// calculator with actions shifted into state

import iTasks.API.Extensions.SVG.SVGlet

calculator2 :: Task Int
calculator2 = calc2 initActionState

initActionState :: (ActionState Char CalculatorState) 
initActionState = {ActionState|state = initState, action = Nothing}

updateDigit2 :: Char CalculatorState -> CalculatorState
updateDigit2 digit st=:{digits} = {st & digits = digits*10 + toInt (digit - '0')}

calcFunctions2 :: [(Char,Int Int -> Int)]
calcFunctions2 = [('+',(+)),('-',(-)),('*',(*)),('/',(/)),('=',(+))]

applyOperator2 :: Char CalculatorState -> CalculatorState
applyOperator2 a st  = {result = (findOperator a) st.result st.digits, digits = 0}
where
	findOperator a = hd [op \\ (c,op) <- calcFunctions2 | c == a]

calc2 :: (ActionState Char CalculatorState) -> Task Int
calc2 st 
= 	(updateInformation "Calculator" view st
	>>* [ OnValue (ifAction isDigit    updateDigit2  calc2) 
		, OnValue (ifAction isOperator applyOperator2 calc2)
		, OnAction ActionContinue (hasValue (\st -> return st.ActionState.state.result))
	    ])
	>>= viewInformation "result" []
where
	isOperator c = isMember c (map fst calcFunctions2)

view = []

view = [] // svg image has to be defined here 


























	
	
