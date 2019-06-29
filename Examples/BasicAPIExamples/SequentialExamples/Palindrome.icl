implementation module BasicAPIExamples.SequentialExamples.Palindrome

// Enter a palindrome

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter a palindrome" palindrome

main :: Task ()
main = palindrome @! ()

palindrome :: Task (Maybe String)
palindrome
	=   	Hint "Enter a palindrome" @>> enterInformation []
	>>* 	[ OnAction  ActionOk     (ifValue palindrome (\v -> return (Just v)))
            , OnAction  ActionCancel (always (return Nothing))
            ]
    >>=		\result -> Hint "Result is:" @>> viewInformation [] result
    >>=		return
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s
