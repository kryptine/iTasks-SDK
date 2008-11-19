implementation module TaskTree

// *********************************************************************************************************************************
// This module contains the functions for calculating the TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import StdEnv
import iDataFormlib
import InternaliTasksCommon, iTasksHtmlSupport
import InternaliTasksThreadHandling
import iTasksProcessHandling

calculateTaskTree :: !UserId !Bool !Bool !Bool !(Task a) !*HSt  
						-> (!Bool,!HtmlTree,!Maybe String,!Maybe [Trace],!Maybe [HtmlTag],!Maybe [HtmlTag],!*HSt) | iData a
calculateTaskTree thisUser traceOn showProcessTable showCurrThreadTable mainTask hst
# (pversion,hst)	 	= setPUserNr thisUser id hst												// fetch global settings of this user
# (sversion,hst)	 	= setSVersionNr thisUser id hst												// fetch version number of session (not needed in new set up?)
# versionconflict		= sversion > 0 && sversion < pversion.versionNr //&& not noNewVersion 		// test if there is a version conflict				
| versionconflict		= (True,BT [] [],Just "Version conflict detected!",Nothing,Nothing,Nothing,hst)				// Yes, return error message

# ((toServer,thrOwner,event,thrinfo,threads),tst=:{activated})	
						=  calculateTasks thisUser pversion mainTask (initTst thisUser TxtFile TxtFile hst)

# (processTable,tst)		
						= if  showProcessTable (showWorkflows activated {tst & activated = activated}) ([],{tst & activated = activated})
# (threadTable,tst=:{html,hst,trace,activated})		
						= if  showCurrThreadTable  (showThreadTable {tst & activated = activated}) ([],{tst & activated = activated})


# newUserVersionNr		= 1 + if (pversion.versionNr > sversion) pversion.versionNr sversion		// increment user querie version number
# (_,hst)				= clearIncPUser thisUser (\_ -> newUserVersionNr) hst						// store in session
# (sversion,hst)	 	= setSVersionNr thisUser (\_ -> newUserVersionNr) hst						// store in persistent memory
# showCompletePage		= IF_Ajax (hd threads == [-1]) True
= (toServer,html,Nothing,trace,if showProcessTable (Just processTable) Nothing,if showCurrThreadTable (Just threadTable) Nothing,hst)
where
	initTst :: !UserId !Lifespan !Lifespan !*HSt -> *TSt
	initTst thisUser itaskstorage threadstorage hst
	=	{ tasknr		= [-1]
		, activated 	= True
		, staticInfo	= initStaticInfo thisUser threadstorage
		, userId		= if (thisUser >= 0) defaultUser thisUser
		, workflowLink	= (0,(defaultUser,0,defaultWorkflowName))
		, html 			= BT [] []
		, trace			= if traceOn (Just []) Nothing
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
	=	{ tasklife 		= if (thisUser >= 0) location Session 
		, taskstorage 	= PlainString
		, taskmode 		= Edit 
		, gc			= Collect
		}