implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util, TUIDiff

buildTaskPanel :: !UITreeContainer -> TaskPanel
buildTaskPanel cont=:(TTContainer menu tree) = case tree of
	TTFinishedTask _ _ False
		= TaskDone
	TTFinishedTask _ _ True
		= buildResultPanel cont
	TTInteractiveTask _ _
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu)
			, updates 		= Nothing	
			, menu			= menu
			}
	TTParallelTask _ _
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu)
			, updates 		= Nothing	
			, menu			= menu
			}
where
	buildTaskPanel` :: !UITree ![TUIDef] -> TUIDef
	buildTaskPanel` tree menu = case tree of
		TTInteractiveTask {TaskInfo|title,description,type,isControlTask,localInteraction,interactiveLayout=l=:TIInteractiveLayoutMerger layout} (editor,buttons)
			= layout	{ TUIInteractive
						| title				= title
						, description		= htmlDisplay Nothing description
						, editorParts		= editor
						, buttons			= buttons
						, type				= type
						, isControlTask		= isControlTask
						, localInteraction	= localInteraction
						}
		TTParallelTask {TaskInfo|title,description,parallelLayout=l=:TIParallelLayoutMerger layout} containers
			= layout	{ TUIParallel
						| title			= title
						, description	= htmlDisplay Nothing description
						, items			= map buildParallelElement containers
						}
	where
		buildParallelElement :: !UIParallelTreeContainer -> TUIDef
		buildParallelElement (TTParallelContainer _ type tree) = case type of
			TTWindow _ menu	= buildTaskPanel` tree menu
			TTDialog _		= buildTaskPanel` tree []
			TTInBody		= buildTaskPanel` tree []
									
buildResultPanel :: !UITreeContainer -> TaskPanel
buildResultPanel tree = case tree of 
	TTContainer _ (TTFinishedTask ti result _)
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
