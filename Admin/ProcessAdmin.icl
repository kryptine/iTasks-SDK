implementation module ProcessAdmin

import iTasks
import StdMisc
from TaskTree import :: TaskProperties(..), :: TaskSystemProperties(..), :: TaskManagerProperties(..), :: TaskWorkerProperties(..), :: TaskProgress(..)

processAdmin :: [Workflow]
processAdmin
	= [	{Workflow
		| name = "Admin/Manage processes"
		, label = "Manage processes"
		, roles	= []
		, mainTask = processAdminMainTask
		}]
		
processAdminMainTask :: Task Void
processAdminMainTask
= (	stopOrRefresh
	-||-
	(	
							getProcesses [Active,Suspended] True
		>>= \processes ->	case processes of
								[]	= return True
								_	= gridChooseTask processes header visualizeProcess processTasks
	)
  ) <! id <<@ TTVertical
  >>| return Void

where
	header :: [[HtmlTag]]
	header = [[Text h] \\ h <- ["Id","Label","Owner","Delegator"]]
	
	visualizeProcess :: Process -> [HtmlTag]
	visualizeProcess {Process|processId, properties = {systemProps,managerProps}}
		= [Text (toString processId)
		  ,Text systemProps.subject
		  ,Text (toString (fst managerProps.worker))
		  ,Text (toString (snd systemProps.manager))
		  ]
	
	processTasks :: [(Process -> String, Process -> Task Bool)]
 	processTasks = [(\proc -> if (isActive proc) "Suspend" "Activate",	toggleProcess)
 				   ,(\_ -> "Inspect",									\p -> ok >>| return True)
 				   ,(\_ -> "Kill",										killProcess)
 				   ]

	isActive {status = Active } = True
	isActive _					= False
 	
stopOrRefresh :: Task Bool
stopOrRefresh = button "Reload process list" False -||- button "I am done" True <<@ TTHorizontal

toggleProcess :: Process -> Task Bool
toggleProcess process=:{Process|processId,status}
	= case status of
		Active		= setProcessStatus Suspended processId
		Suspended	= setProcessStatus Active processId		
	  >>| return False


killProcess :: Process -> Task Bool
killProcess process=:{Process|processId}
	=	setProcessStatus Deleted processId
	>>|	return False

gridChooseTask :: [a] [[HtmlTag]] (a -> [HtmlTag]) [(a -> String, a -> Task Bool)] -> Task Bool | iTask a
gridChooseTask xs header rowVisualizeFun rowTasks = abort "TODO: REMOVE gridChooseTask"