module Calculator

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin

Start :: *World -> *World
Start world = startTask [ workflow  "calculator" "calculator"          	      calculator
						, workflow "Manage users"  "Manage system users..."   manageUsers
						, workflow "test"  "test"   calculator2
						] world

startTask taskList world
	= startEngine [ publish "/" (WebApp []) (\_-> browseExamples taskList)
				  ] world
where
	browseExamples taskList = forever (
		 	enterInformation "Enter your credentials and login or press continue to remain anonymous" []
		>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey (unmodified KEY_ENTER)]) (hasValue (browseAuthenticated taskList))
			] )
	
	browseAuthenticated taskList {Credentials|username,password}
		= authenticateUser username password
		>>= \mbUser -> case mbUser of
			Just user 	= workAs user (manageWorklist taskList)
			Nothing		= viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void
	
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

:: ActionState a s  = 	{ state		:: s
						, action	:: Maybe a
						}

derive class iTask ActionState

ifAction :: (a -> Bool) (a s -> s) ((ActionState a s) -> Task b) (TaskValue (ActionState a s)) -> Maybe (Task b)
ifAction pred astos stotaskb (Value {ActionState|state=s,action=Just a} _) 
    | pred a 	= Just (stotaskb {ActionState|state = astos a s, action = Nothing})
    | otherwise = Nothing
ifAction _ _ _ _ = Nothing

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
= 	(updateInformation "Calculator" [] st
	>>* [ OnValue (ifAction isDigit    updateDigit2  calc2) 
		, OnValue (ifAction isOperator applyOperator2 calc2)
		, OnAction ActionContinue (hasValue (\st -> return st.ActionState.state.result))
	    ])
	>>= viewInformation "result" []
where
	isOperator c = isMember c (map fst calcFunctions2)




test = updateImageState "test" Nothing field


/*
mkboard :: Bool TicTacToe2 -> Image TicTacToe2
mkboard turn ttt=:{board2,turn2}
	= grid (Rows 3) (LeftToRight,TopToBottom) [] [] 
	       [ mkTile i j (turn == turn2) cell \\ row <- board2 & i <- [0..2], cell <- row & j <- [0..2] ]
	       Nothing
*/
import iTasks.API.Extensions.SVG.SVGlet

field :: (Maybe String) -> Image (Maybe String)
field _ = grid (Rows 3) (LeftToRight, TopToBottom) [] []
												  
												 [button i \\ i <- [1..9]]
												 Nothing

button i = rect (PxSpan buttonL) (PxSpan buttonH) <@< { onclick = \_ -> Just (toString i)} 
												  <@< {strokewidth = px 1.0} 
												  <@< {fill = toSVGColor "none"}
where
	buttonL = 30.0
	buttonH = 15.0


























	
	
