module examples
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin
 
Start :: *World -> *World
Start world 
    = startEngine 
    	[ publish "/"      (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "welcome to my examples" myExamples)
        , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
        ] world
 
 
 
myExamples :: [Workflow]
myExamples = 	[	workflow "palindrome" 					"accepts palindrome string " 	palindrome
				,	workflow "create list of persons"		"one by one" 					person1by1
				,	workflow "create list of palindromes"	"one by one" 					palindrome1by1
				,	workflow "delegate  list of persons"	"delegate the creation" 		(delegate person1by1)
				,	workflow "monitor a list maker"			"one user creates a list and another can monitor this" monitorPersons

				,	workflow "monitor a GoogleMap browser"	"one user browses and another can monitor this" monitorGoogleMap
				,	workflow "chat 1"							"two users can chat with each other" chat1
				,	workflow "chat 2"							"two users can chat with each other, and point to interesting points on a map" chat2


				,	workflow "Manage users"					"Create users and logins..." 		manageUsers
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
		>>= \user -> 	(user,"delegated task") @: (task >>= return)
		>>= \result ->	viewInformation "The result is:" [] result


monitorPersons ::  Task [Person]
monitorPersons =	selectCoWorker "who do you want to monitor ? " >>= monitorWorker 


import iTasks.API.Extensions.GIS.GoogleMap

monitorGoogleMap ::  Task (GoogleMap, Note)
monitorGoogleMap =	selectCoWorker "who do you want to monitor ?" >>= monitorWorker 

selectCoWorker :: String -> Task (User,User)
selectCoWorker  prompt
	=						get currentUser
		>>= \me -> 			enterChoiceWithShared prompt [] users
		>>= \colleague ->	return (me,colleague)


monitorWorker :: (User, User) -> Task a  | iTask a
monitorWorker (me,worker)
	=	withShared defaultValue 
		(\share -> ((worker, "Update Information") @: updateSharedInformation  ("Update, viewer is " <+++ me) [] share)
			        -||  
			       ((me, "View Information") @: viewSharedInformation    ("Viewer, worker is " <+++ worker)  [] share) 
	    )

chat1 :: Task (Note,Note)
chat1 = chat
	    
chat2 :: Task ((GoogleMap,Situation),Reaction)
chat2 = chat

:: Situation	= AllIsFine | Fire | WaterProblem | TrafficAccident | OtherIssue Note
:: Reaction		= NoActionProposed | SendFireBrigate | SendPolice | SendSpecialForces | OtherProposal Note

derive class iTask Situation, Reaction

// general chat where one user provides information of arbitrary type a, and the other can respond with arbitrary type b

chat :: Task (a,b) | iTask a & iTask b
chat =							selectCoWorker "with whom do you want to work ? "
		>>= \(me,colleague) ->	withShared defaultValue 
			(\workOfMe ->		withShared defaultValue
			(\workOfColleague ->((me,"chat") @: updateAndView (me,workOfMe) (colleague,workOfColleague))
								-&&-
								((colleague,"chat") @: updateAndView (colleague,workOfColleague) (me,workOfMe))
			))
where
	updateAndView (me,workOfMe) (you,workOfYou)
		= 	viewSharedInformation    ("You are looking at the response of: " <+++ you) [] workOfYou 
			||-
			updateSharedInformation  ("Please enter your information: " <+++ me) [] workOfMe
			
	    
	    
	    



