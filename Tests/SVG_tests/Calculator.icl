module Calculator

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin

Start :: *World -> *World
Start world = startTask [ workflow  "calculator" "calculator"          	      calculator
						, workflow "Manage users"  "Manage system users..."   manageUsers
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
	
// calculator

:: CalculatorState = {result :: Int, input :: Int }

derive class iTask CalculatorState

initState 				= {result = 0, input = 0}
updateDigit digit st 	= calc {st & input = st.input*10 + digit}
applyOperator op st 	= calc {result = op st.result st.input, input = 0}

calculator :: Task Int
calculator = calc initState

calc st
= 		viewInformation "Calculator" [ViewWith Display] st
	>>* [ OnAction (Action (toString i) []) (always (updateDigit i st)) 
		\\ i <- [0..9] 
		] ++
		[ OnAction (Action eventName []) (always (applyOperator operator st))
		\\ (eventName,operator) <- [("+",(+)),("-",(-)),("*",(*)),("/",(/)),("=",(+))] 
		]

	
	
