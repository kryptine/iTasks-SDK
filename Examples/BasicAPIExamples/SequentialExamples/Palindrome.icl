implementation module BasicAPIExamples.SequentialExamples.Palindrome

// Enter a palindrome

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter a palindrome" palindrome

Start :: *World -> *World
Start world
	= startEngine palindrome world

palindrome :: Task (Maybe String)
palindrome
	=   	enterInformation "Enter a palindrome" []
	>>* 	[ OnAction  ActionOk     (ifValue palindrome (\v -> return (Just v)))
            , OnAction  ActionCancel (always (return Nothing))
            ]
    >>=		viewInformation "Result is:" []
    >>=		return
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s
