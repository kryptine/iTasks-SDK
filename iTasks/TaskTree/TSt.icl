implementation module TSt

import InternaliTasksCommon
import StdEnv, StdMaybe
import HSt

mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
mkTst thisUser itaskstorage threadstorage hst
	=	{ tasknr		= [-1]
		, activated 	= True
		, staticInfo	= initStaticInfo thisUser threadstorage
		, userId		= if (thisUser >= 0) defaultUser thisUser
		, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
		, html 			= BT [] []
		, hst 			= hst
		, options 		= initialOptions thisUser itaskstorage
		}

initStaticInfo :: UserId !Lifespan -> StaticInfo
initStaticInfo thisUser location
	=	{ currentUserId	= thisUser 
		, threadTableLoc= location
		}

initialOptions ::  !UserId !Lifespan  -> Options 
initialOptions thisUser location 
	=	{ tasklife 		= if (thisUser >= 0) location LSSession 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		, trace			= False
		}
	

appTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)
appTaskTSt (Task fn) tst = fn tst