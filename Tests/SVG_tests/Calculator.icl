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

:: CalculatorState = { display :: Int, n :: Int }

derive class iTask CalculatorState

calculator :: Task Int
calculator = calc initSt
where
	calc st
	= 		viewInformation "Calculator" [ViewWith Display] st
		>>* [ OnAction (Action "7" []) (always (updateDigit 7 st)) 
			, OnAction (Action "8" []) (always (updateDigit 8 st))
			, OnAction (Action "9" []) (always (updateDigit 9 st))
			, OnAction (Action "4" []) (always (updateDigit 4 st)) 
			, OnAction (Action "5" []) (always (updateDigit 5 st))
			, OnAction (Action "6" []) (always (updateDigit 6 st))
			, OnAction (Action "1" []) (always (updateDigit 1 st)) 
			, OnAction (Action "2" []) (always (updateDigit 2 st))
			, OnAction (Action "3" []) (always (updateDigit 3 st)) 
			, OnAction (Action "0" []) (always (updateDigit 0 st))
			, OnAction (Action "+" []) (always (apply (+) st))
			, OnAction (Action "-" []) (always (apply (-) st))
			, OnAction (Action "*" []) (always (apply (*) st))
			, OnAction (Action "/" []) (always (apply (/) st))
			]
	where
		updateDigit n st = calc {st & n = st.n*10 + n}
	
		apply op st = calc {display = op st.display st.n, n = 0}

	initSt = { display = 0, n = 0}