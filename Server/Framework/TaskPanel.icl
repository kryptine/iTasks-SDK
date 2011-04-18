implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util, TUIDiff

buildTaskPanel :: !UITreeContainer -> TaskPanel
buildTaskPanel cont=:(TTContainer menu tree controlTask) = case tree of
	TTFinishedTask _ _ False
		= TaskDone
	TTFinishedTask _ _ True
		= buildResultPanel cont
	TTInteractiveTask _ _ _
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu controlTask)
			, updates 		= Nothing	
			, menu			= menu
			}
	TTParallelTask _ _
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu controlTask)
			, updates 		= Nothing	
			, menu			= menu
			}
where
	buildTaskPanel` :: !UITree ![TUIDef] !Bool -> TUIDef
	buildTaskPanel` tree menu controlTask = case tree of
		TTInteractiveTask {TaskInfo|title,description,interactiveLayout=l=:TIInteractiveLayoutMerger layout} type (editor,mbContext,buttons)
			= layout	{ TUIInteractive
						| title			= title
						, description	= htmlDisplay Nothing description
						, mbContext		= mbContext
						, editor		= editor
						, buttons		= buttons
						, type			= type
						, isControlTask	= controlTask
						}
		TTParallelTask {TaskInfo|title,description,parallelLayout=l=:TIParallelLayoutMerger layout} containers
			= layout	{ TUIParallel
						| title			= title
						, description	= htmlDisplay Nothing description
						, items			= map buildParallelElement containers
						}
	where
		buildParallelElement :: !UIParallelTreeContainer -> TUIDef
		buildParallelElement (TTParallelContainer _ type tree controlTask) = case type of
			TTWindow _ menu	= buildTaskPanel` tree menu controlTask
			TTDialog _		= buildTaskPanel` tree [] controlTask
			TTInBody		= buildTaskPanel` tree [] controlTask
									
buildResultPanel :: !UITreeContainer -> TaskPanel
buildResultPanel tree = case tree of 
	TTContainer _ (TTFinishedTask ti result _) _
		= (TTCInteractiveContainer	{ TTCInteractiveContainer
									| xtype 		= "itasks.ttc.interactive"
									, content 		= Just (content ti result)
									, updates 		= Nothing	
									, menu			= []
									})
	_
		= TaskNotDone
where
	content {TaskInfo|title,description,resultLayout=l=:TIResultLayoutMerger layout} result
		= layout	{ TUIResult
					| title			= title
					, description	= htmlDisplay Nothing description
					, result		= htmlDisplay Nothing (toString result)
					}
		
diffTaskPanels :: !TaskPanel !TaskPanel -> TaskPanel
diffTaskPanels (TTCInteractiveContainer old) (TTCInteractiveContainer new)
	= TTCInteractiveContainer {new & content = Nothing, updates = Just (diffEditorDefinitions (fromJust old.TTCInteractiveContainer.content) (fromJust new.TTCInteractiveContainer.content))}
diffTaskPanels _ new
	= new
