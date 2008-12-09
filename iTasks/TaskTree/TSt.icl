implementation module TSt

import InternaliTasksCommon
import StdEnv, StdMaybe
import HSt

mkTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
mkTst thisUser itaskstorage threadstorage hst
	=	{ tasknr		= [-1]
		, activated 	= True
		, staticInfo	= initStaticInfo thisUser threadstorage
		, userId		= defaultUser
		, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
		, html 			= BT [] []
		, hst 			= hst
		, trace			= False
		, options 		= initialOptions itaskstorage
		}

initStaticInfo :: UserId !Lifespan -> StaticInfo
initStaticInfo thisUser location
	=	{ currentUserId	= thisUser 
		, threadTableLoc= location
		}

initialOptions :: !Lifespan  -> Options 
initialOptions location 
	=	{ tasklife 		= location 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		}

appTaskTSt :: !(Task a) !*TSt -> (!a,!*TSt)
appTaskTSt (Task fn) tst = fn tst