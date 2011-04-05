implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util, TUIDiff

buildTaskPanel :: !UITreeContainer -> TaskPanel
buildTaskPanel cont=:(TTContainer menu tree controlTask) = buildTaskPanel` tree menu controlTask
where
	buildTaskPanel` :: !UITree ![TUIDef] !Bool -> TaskPanel
	buildTaskPanel` tree menu controlTask
		= case tree of
			TTFinishedTask _ _ False
				= TaskDone
			TTFinishedTask _ _ True
				= buildResultPanel cont
			TTInteractiveTask ti type tui
				= TTCInteractiveContainer
					{ TTCInteractiveContainer
					| xtype 		= "itasks.ttc.interactive"
					, subject		= ti.TaskInfo.subject
					, description	= ti.TaskInfo.description
					, content 		= Just tui
					, updates 		= Nothing	
					, menu			= menu
					, formWidth		= ti.TaskInfo.formWidth
					, type			= if controlTask Control type
					}
			TTParallelTask ti containers
				= TTCParallelContainer
					{ TTCParallelContainer 
					| xtype			= "itasks.ttc.parallel"
					, subject		= ti.TaskInfo.subject
					, description	= ti.TaskInfo.description
					, content		= map buildParallelElement containers
					, menu			= menu
					}
	where
		buildParallelElement :: !UIParallelTreeContainer -> TaskPanel
		buildParallelElement (TTParallelContainer _ type tree controlTask) = case type of
			TTWindow _ menu	= buildTaskPanel` tree menu controlTask
			TTDialog _		= buildTaskPanel` tree [] controlTask
			TTInBody		= buildTaskPanel` tree [] controlTask
									
buildResultPanel :: !UITreeContainer -> TaskPanel
buildResultPanel tree = case tree of 
	TTContainer _ (TTFinishedTask ti result _) _
		= (TTCResultContainer {TTCResultContainer
								| xtype 	= "itasks.ttc.result"
								, subject	= ti.TaskInfo.subject
								, result	= toString result
								})
	_
		= TaskNotDone
		
diffTaskPanels :: !TaskPanel !TaskPanel -> TaskPanel
diffTaskPanels (TTCInteractiveContainer old) (TTCInteractiveContainer new)
	= TTCInteractiveContainer {new & content = Nothing, updates = Just (diffEditorDefinitions (fromJust old.TTCInteractiveContainer.content) (fromJust new.TTCInteractiveContainer.content))}
diffTaskPanels (TTCParallelContainer old) (TTCParallelContainer new)
	= TTCParallelContainer {TTCParallelContainer|new & content = map (uncurry diffTaskPanels) (zip2 old.TTCParallelContainer.content new.TTCParallelContainer.content)}
diffTaskPanels _ new
	= new
