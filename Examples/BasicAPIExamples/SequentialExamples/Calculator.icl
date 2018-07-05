implementation module BasicAPIExamples.SequentialExamples.Calculator

// a tiny calculator

import iTasks

wf :: String -> Workflow
wf a = workflow a "Calculator" calculator

Start :: *World -> *World
Start world
	= startEngine calculator world

:: CalculatorState = { display :: Int, n :: Int}

derive class iTask CalculatorState

calculator :: Task Int
calculator = calc initSt
where
	calc st
	= 		viewInformation "Calculator" [] st
		>>* [ OnAction (Action "7") (always (updateDigit 7 st))
			, OnAction (Action "8") (always (updateDigit 8 st))
			, OnAction (Action "9") (always (updateDigit 9 st))
			, OnAction (Action "4") (always (updateDigit 4 st))
			, OnAction (Action "5") (always (updateDigit 5 st))
			, OnAction (Action "6") (always (updateDigit 6 st))
			, OnAction (Action "1") (always (updateDigit 1 st))
			, OnAction (Action "2") (always (updateDigit 2 st))
			, OnAction (Action "3") (always (updateDigit 3 st))
			, OnAction (Action "0") (always (updateDigit 0 st))
			, OnAction (Action "+") (always (apply (+) st))
			, OnAction (Action "-") (always (apply (-) st))
			, OnAction (Action "*") (always (apply (*) st))
			, OnAction (Action "/") (always (apply (/) st))
			, OnAction (Action "c") (always (calc initSt))
			]
	where
		updateDigit n st = calc {st & n = st.n*10 + n}

		apply op st = calc {display = op st.display st.n, n = 0}

initSt = { display = 0, n = 0}
