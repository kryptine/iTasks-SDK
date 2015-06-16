module examples
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin
 
Start :: *World -> *World
Start world 
    = startEngine [ publish "/"      (WebApp []) (\_-> myExamples)
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          ] world
 
 
 
myExamples :: Task Void
myExamples = manageWorklist [workflow "palindrome" "accepts palindrome string " palindrome]
 
 
palindrome :: Task (Maybe String)
palindrome 
	=   		enterInformation "Enter a palindrome" []
    	>>* 	[ OnAction  ActionOk     (ifValue isPalindrome  (\v -> return (Just v)))
               	, OnAction  ActionCancel (always	            (return Nothing))
               	]
where
	isPalindrome s = l == reverse l where l = [e \\ e <-: s]

