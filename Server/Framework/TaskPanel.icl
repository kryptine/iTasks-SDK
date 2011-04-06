implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc
import JSON, HTML, TSt, TUIDefinition, Map, Util, TUIDiff

buildTaskPanel :: !UITreeContainer -> TaskPanel
buildTaskPanel cont=:(TTContainer menu tree controlTask) = case tree of
	TTFinishedTask _ _ False
		= TaskDone
	TTFinishedTask _ _ True
		= buildResultPanel cont
	TTInteractiveTask ti type tui
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu controlTask)
			, updates 		= Nothing	
			, menu			= menu
			, type			= if controlTask Control type
			}
	TTParallelTask ti containers
		= TTCInteractiveContainer
			{ TTCInteractiveContainer
			| xtype 		= "itasks.ttc.interactive"
			, content 		= Just (buildTaskPanel` tree menu controlTask)
			, updates 		= Nothing	
			, menu			= menu
			, type			= if controlTask Control Parallel
			}
where
	buildTaskPanel` :: !UITree ![TUIDef] !Bool -> TUIDef
	buildTaskPanel` tree menu controlTask
		= case tree of
			TTInteractiveTask ti type tui
				# restrictedWidth = case ti.TaskInfo.formWidth of
					Just FWFullWidth	= False
					_					= True
				= TUIContainer
					{ TUIContainer
					| simpleContainer
						[ TUIContainer {TUIContainer | simpleContainer [htmlDisplay Nothing ti.TaskInfo.subject]		& cls = Just "TTCSubject"}
						, TUIContainer {TUIContainer | simpleContainer [htmlDisplay Nothing ti.TaskInfo.description]	& cls = Just "TTCDescription"}
						, TUIContainer {TUIContainer | simpleContainer [tui]											& cls = Just "TTCPanel"}
						]
					& restrictedWidth = restrictedWidth
					}
			TTParallelTask ti containers
				# restrictedWidth = case ti.TaskInfo.formWidth of
					Just FWFullWidth	= False
					_					= True
				= TUIContainer
					{ TUIContainer
					| simpleContainer (map buildParallelElement containers)
					& restrictedWidth = restrictedWidth
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
diffTaskPanels _ new
	= new
