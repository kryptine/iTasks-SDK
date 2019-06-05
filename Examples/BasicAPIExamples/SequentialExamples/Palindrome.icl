implementation module BasicAPIExamples.SequentialExamples.Palindrome

// Enter a palindrome

import iTasks

wf :: String -> Workflow
wf a = workflow a "Enter a palindrome" palindrome

main :: Task ()
main = palindrome @! ()

palindrome :: Task (Maybe String)
palindrome
	=   	enterInformation [EnterWithHint "Enter a palindrome"]
	>>* 	[ OnAction  ActionOk     (ifValue palindrome (\v -> return (Just v)))
            , OnAction  ActionCancel (always (return Nothing))
            ]
    >>=		viewInformation [ViewWithHint "Result is:"]
    >>=		return
where
	palindrome s = lc == reverse lc
	where lc :: [Char]
		  lc = fromString s
