module examples
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin
 
Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> loginAndManageWorkList "welcome to my examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world
 
 
 
myExamples :: [Workflow]
myExamples = 	[	workflow "palindrome" 					"accepts palindrome string " 	palindrome
				,	workflow "create list of persons"		"one by one" 					person1by1
				,	workflow "create list of palindromes"	"one by one" 					palindrome1by1
				,	workflow "delegate  list of persons"	"delegate the creation" 		(delegate person1by1)
				,	workflow "two google maps workers"		"one to update and one to monitor" twoGoogleMaps


				,	workflow "Manage users"	"Manage system users..." 		manageUsers
			 	]
 
 
palindrome :: Task (Maybe String)
palindrome 
	=   		enterInformation "Enter a palindrome" []
    	>>* 	[ OnAction  ActionOk     (ifValue isPalindrome  (\v -> return (Just v)))
               	, OnAction  ActionCancel (always	            (return Nothing))
               	]
where
	isPalindrome s = l == reverse l where l = [e \\ e <-: s]
	

:: Person 	=  { name :: String, gender :: Gender, dateOfBirth :: Date}
:: Gender 	= Male | Female

derive class iTask Person, Gender


myTask :: Task Int
myTask = enterInformation "Enter an integer" []


person1by1 :: Task [Person] 
person1by1 = add1by1 []

palindrome1by1 :: Task [Maybe String]
palindrome1by1 = add1by1 []

add1by1 :: [a] -> Task [a]	| iTask a
add1by1 list_so_far
	= 		enterInformation "Add an element" []
		    -|| 
		    viewInformation "List so far.." [] list_so_far
		>>* [ OnAction  (Action "Add" [])      (hasValue  (\elem   -> add1by1 [elem : list_so_far])) 
		    ,  OnAction  (Action "Finish" [])  (always (return list_so_far))
		    ,  OnAction  ActionCancel 	      (always (return []))
	        ]

delegate :: (Task a) -> Task a | iTask a
delegate task
	=					enterChoiceWithShared "Select someone to delegate the task to:" [] users
		>>= \user -> 	user @: (task >>= return)
		>>= \result ->	viewInformation "The result is:" [] result


import iTasks.API.Extensions.GIS.GoogleMap

twoGoogleMaps ::  Task (GoogleMap, Note)
twoGoogleMaps 
	=					enterChoiceWithShared "Select someone to enter information:" [] users
		>>= \worker -> 	enterChoiceWithShared "Select someone to view what is entered:" [] users
		>>= \viewer -> 	twoTasks worker viewer 
	


twoTasks :: User User -> Task a  | iTask a
twoTasks user1 user2
= withShared defaultValue 
	(\share -> ((user1, "Enter something") @: updateSharedInformation  ("Update, viewer is " <+++ user2) [] share)
		        -||  
		       ((user2, "View something") @: viewSharedInformation    ("Viewer, creator is " <+++ user1)  [] share) 
    )



