implementation module TaskTree

// *********************************************************************************************************************************
// This module contains the functions for calculating the TaskTree
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************

import StdEnv
import iDataFormlib
import InternaliTasksCommon
import InternaliTasksThreadHandling
import iTasksProcessHandling
import TSt

calculateTaskTree :: !Int !Bool !Bool !Bool !(LabeledTask a) !Int !*HSt  
						-> (!Bool,!HtmlTree,!Maybe String,!Maybe [HtmlTag],!Maybe [HtmlTag],!*HSt) | iData a
calculateTaskTree thisUser traceOn showProcessTable showCurrThreadTable mainTask mainUser hst
# (pversion,hst)	 	= setPUserNr thisUser id hst												// fetch global settings of this user
# ((toServer,thrOwner,event,thrinfo,threads),tst=:{activated})	
						=  calculateTasks thisUser pversion mainTask mainUser (mkTst thisUser LSTxtFile LSTxtFile hst)

# (processTable,tst)	= if  showProcessTable (showWorkflows activated {tst & activated = activated}) ([],{tst & activated = activated})
# (threadTable,tst=:{html,hst,activated})		
						= if  showCurrThreadTable  (showThreadTable {tst & activated = activated}) ([],{tst & activated = activated})
# showCompletePage		= IF_Ajax (hd threads == [-1]) True
= (toServer,html,Nothing,if showProcessTable (Just processTable) Nothing,if showCurrThreadTable (Just threadTable) Nothing,hst)