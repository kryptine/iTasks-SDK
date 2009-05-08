implementation module ProcessAdmin

import iTasks

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
							getProcesses [Active,Suspended]
		>>= \processes ->	case processes of
								[]	= return True
								_	= gridChooseTask processes header visualizeProcess processTasks
	)
  ) <! id <<@ TTVertical
  >>| return Void

where
	header :: [[HtmlTag]]
	header = [[Text h] \\ h <- ["Id","Label","Owner","Delegator"]]
	
	visualizeProcess :: Process -> [[HtmlTag]]
	visualizeProcess {Process|id,label,owner,delegator}
		= [[Text (toString id)]
		  ,[Text label]
		  ,[Text (toString owner)]
		  ,[Text (toString delegator)]
		  ]
	
	processTasks :: [(Process -> String, Process -> Task Bool)]
 	processTasks = [(\proc -> if (isActive proc) "Suspend" "Activate",	toggleProcess)
 				   ,(\_ -> "Inspect",									inspectProcess)
 				   ,(\_ -> "Kill",										killProcess)
 				   ]

	isActive {status = Active } = True
	isActive _					= False
 	
stopOrRefresh :: Task Bool
stopOrRefresh = button "Reload process list" False -||- button "I am done" True <<@ TTHorizontal

toggleProcess :: Process -> Task Bool
toggleProcess process=:{Process|id,status}
	= case status of
		Active		= setProcessStatus Suspended id
		Suspended	= setProcessStatus Active id		
	  >>| return False

inspectProcess :: Process -> Task Bool
inspectProcess process = yes

killProcess :: Process -> Task Bool
killProcess process=:{Process|id}
	=	setProcessStatus Deleted id
	>>|	return False

gridChooseTask :: [a] [[HtmlTag]] (a -> [[HtmlTag]]) [(a -> String, a -> Task Bool)] -> Task Bool | iData a
gridChooseTask xs header rowVisualizeFun rowTasks
	=				orTasks [("row-" +++ toString i, row x) \\ x <- xs & i <- [1..]] <<@ TTCustom (toTable header)	//Show the grid
	>>= \task -> 	task																																	//Execute the chosen task
where

	row x = parallel "row-content" (\list -> length list == 1) (\_ [index] -> (snd (rowTasks !! index)) x) [("row-buttons", selectWithButtons [labelFun x \\ (labelFun,_) <- rowTasks])]
				<<@ TTCustom (toRow (toCells (rowVisualizeFun x)))
				
	toCells html = [TdTag [] cell \\ cell <- html]
	toRow info [tasks] = [TrTag [] (info ++ [TdTag [] tasks])]
	toTable header rows = [TableTag [ClassAttr "debug-table"] [TrTag [] ([ThTag [] cell \\ cell <- header ] ++ [ThTag [] [RawText "&nbsp;"]]):flatten rows]]  