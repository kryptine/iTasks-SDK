implementation module WorkListHandler

import StdEnv
import Http
import Text
import JSON
import iDataHandler
import TaskTree, TaskTreeFilters

:: WorkListItem = { taskid	:: String //Task id of the work item
				  , for		:: String //Label of the user who issued the work
				  , subject	:: String //Short description of the work
				  }

derive JSONEncode WorkListItem

handleWorkListRequest :: !(Task a) *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkListRequest mainTask hst
# thisUser = 0
# (toServer, htmlTree, maybeError, hst)	= calculateTaskTree thisUser mainTask hst // Calculate the TaskTree given the id of the current user
# worklist									= [{taskid = toString taskLabel, for = toString fromUser, subject = workflowLabel +++ " " +++ taskLabel}				
										  	  \\ (fromUser,(toUser,tasknr,processNr,workflowLabel,taskLabel)) <- collectTaskList thisUser thisUser htmlTree
										   	  | toUser == thisUser]

= ({http_emptyResponse & rsp_data = toJSON worklist}, hst)




