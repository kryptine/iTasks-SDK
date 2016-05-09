module IncidoneCCC
/**
* This module contains an iTasks program designed
* to support SAR operations of the Netherlands Coast guard. It is loosely based on their
* procedure documentation and observation of, and interviews with, Coast guard officers.
*/
import iTasks, StdMisc, System.Time, Text, Data.Tuple
import iTasks.UI.Layout, iTasks.UI.Definition
import qualified Data.Map as DM

//General configuration
import Incidone.Configuration

//Management of the operational picture
import Incidone.OP.SDSs
import Incidone.OP.ContactManagementTasks
//Management of actions
import Incidone.ActionManagementTasks

//Shared code
import Incidone.Util.TaskPatterns
import Incidone.Util.Notification

//Role-based tasks for the different roles users may have
import Incidone.RoleBased.WatchOfficerTasks
import Incidone.RoleBased.AdministratorTasks
import Incidone.RoleBased.PartnerTasks

//Training, experimentation and exercises
import Incidone.Simulation.TrainingTasks

//Device-based tasks that are accessible through the shared devices in the CCC
import Incidone.DeviceBased.VideoWall
import Incidone.DeviceBased.Tablet


Start :: *World -> *World
Start world = startEngine [publish "/"                 (\_ -> ccPerson)
						  ,publish "/wall"             (\_ -> viewVideoWallContent)
						  ,publish "/wall-control"     (\_ -> selectVideoWallContent)
                          ,publish "/exercise-control" (\_ -> controlExercise)
						  ] world
where
	//Main task for command center operators
	ccPerson :: Task ()
	ccPerson = forever (catchAll (doAuthenticated usersMainTask) (\err -> viewInformation "Error" [] err >>| return ()))

	usersMainTask :: User -> Task ()
	usersMainTask me=:(AuthenticatedUser "admin" _ _)
        = whileAuthenticated me configureIncidone
	usersMainTask me=:(AuthenticatedUser _ roles _) | isMember "wo" roles
        = whileAuthenticated me keepWatch
    usersMainTask me = whileAuthenticated me managePartnerActions

doAuthenticated :: (User -> Task a) -> Task a | iTask a
doAuthenticated task
	= (	enterCredentials
	>>* [OnAction (Action "Login" [ActionIcon "login",ActionKey {key=KEY_ENTER,ctrl=False,shift=False,alt=False}])
			(hasValue (\cred -> verifyCredentials cred >>- executeTask task))
		] ) <<@ Title "Login" <<@ ApplyLayout (beforeStep frameCompact) //Compact layout before login, full screen afterwards
where
	enterCredentials :: Task Credentials
	enterCredentials
		= 	viewInformation () [] (DivTag [ClassAttr "identify-app",StyleAttr "width: 350px; height: 55px; margin-bottom: 5px"] [])
		||-	enterInformation (Title "Log in") []

	verifyCredentials :: Credentials -> Task (Maybe User)
	verifyCredentials credentials=:{Credentials|username,password}
		| username === Username "admin"	
            = get adminPassword >>- \password` -> if (password === password`)
                (return (Just (AuthenticatedUser "admin" [] (Just "Administrator"))))
                (return Nothing)
        | otherwise
            = verifyContactCredentials credentials

	executeTask :: (User -> Task a) (Maybe User) -> Task a | iTask a
	executeTask task (Just user)	= workAs user (task user)
	executeTask task Nothing		= throw "Log in failed"

whileAuthenticated :: User [Workspace -> Task ()] -> Task ()
whileAuthenticated user tasks
    =  (controlDash -|| workOnTasks) <<@ (ArrangeWithSideBar 0 TopSide 30 False)
where
	controlDash = (
		    viewInformation () [] ("Welcome " +++ toString user) 
             -&&-
            viewNotifications
        >>* [OnAction (Action "Log out" [ActionIcon "logout"]) (always (return ()))]
        )  <<@ ApplyLayout layoutControlDash

	workOnTasks = doIndependent tasks <<@ ArrangeWithTabs

	layoutControlDash = sequenceLayouts
		[moveSubAt [0,0] [1]
		,moveSubAt [0,0] [2]
		,removeSubAt [0]
		,layoutChildrenOf [] actionToButton
		,setAttributes ('DM'.unions [directionAttr Horizontal,paddingAttr 2 2 2 250, baseClsAttr "summary-bar"])
		,setNodeType UIPanel
        ]
