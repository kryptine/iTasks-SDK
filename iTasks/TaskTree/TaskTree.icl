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
import TSt

calculateTaskTree :: !UserId !Bool !Bool !Bool !(Task a) !*HSt  
						-> (!Bool,!HtmlTree,!Maybe String,!Maybe [Trace],!Maybe [HtmlTag],!Maybe [HtmlTag],!*HSt) | iData a
calculateTaskTree thisUser traceOn showProcessTable showCurrThreadTable mainTask hst
# (pversion,hst)	 	= setPUserNr thisUser id hst												// fetch global settings of this user
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{activated})	
						=  calculateTasks thisUser pversion mainTask (mkTst thisUser LSTxtFile LSTxtFile hst)

# (processTable,tst)		
						= if  showProcessTable (showWorkflows activated {tst & activated = activated}) ([],{tst & activated = activated})
# (threadTable,tst=:{html,hst,trace,activated})		
						= if  showCurrThreadTable  (showThreadTable {tst & activated = activated}) ([],{tst & activated = activated})
# showCompletePage		= IF_Ajax (hd threads == [-1]) True
= (toServer,html,Nothing,trace,if showProcessTable (Just processTable) Nothing,if showCurrThreadTable (Just threadTable) Nothing,hst)