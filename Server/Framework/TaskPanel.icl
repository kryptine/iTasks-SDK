implementation module TaskPanel

import StdList, StdMisc, StdTuple, StdEnum, StdBool, StdFunc, StdOrdList
import JSON, HTML, TSt, TUIDefinition, Map, Util

derive JSONEncode TTCInteractiveContainer, FormContent, InteractiveTaskType, TTCResultContainer, TTCParallelContainer

//JSON specialization for TaskPanel: Ignore the union constructor
JSONEncode{|TaskPanel|} (TaskDone)					= [JSONString "done"]
JSONEncode{|TaskPanel|} (TaskRedundant)				= [JSONString "redundant"]
JSONEncode{|TaskPanel|} (TaskNotDone)				= [JSONString "notdone"]
JSONEncode{|TaskPanel|} (TTCInteractiveContainer x)	= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCResultContainer x)		= JSONEncode{|*|} x
JSONEncode{|TaskPanel|} (TTCParallelContainer x)	= JSONEncode{|*|} x

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
			TTInteractiveTask ti type (Definition def buttons)
				= TTCInteractiveContainer
					{ TTCInteractiveContainer
					| xtype 		= "itasks.ttc.interactive"
					, id 			= "taskform-" +++ ti.TaskInfo.taskId
					, taskId 		= ti.TaskInfo.taskId
					, subject		= ti.TaskInfo.subject
					, description	= ti.TaskInfo.description
					, content 		= Just {form = def, buttons = buttons}
					, updates 		= Nothing	
					, menu			= menu
					, formWidth		= ti.TaskInfo.formWidth
					, type			= if controlTask Control type
					}
			TTInteractiveTask ti type (Updates upd buttons)
				= TTCInteractiveContainer
					{ TTCInteractiveContainer 
					| xtype 		= "itasks.ttc.interactive"
					, id 			= "taskform-" +++ ti.TaskInfo.taskId
					, taskId 		= ti.TaskInfo.taskId
					, subject		= ti.TaskInfo.subject
					, description	= ti.TaskInfo.description
					, content 		= Nothing
					, updates 		= Just (upd ++ [TUIReplaceButtons buttons])
					, menu			= menu
					, formWidth		= Nothing
					, type			= if controlTask Control type
					}
			TTParallelTask ti containers
				= TTCParallelContainer
					{ TTCParallelContainer 
					| xtype			= "itasks.ttc.parallel"
					, taskId		= ti.TaskInfo.taskId
					, subject		= ti.TaskInfo.subject
					, description	= ti.TaskInfo.description
					, content		= map buildParallelElement (sortBy (\(TTParallelContainer idx0 _ _ _) (TTParallelContainer idx1 _ _ _) -> idx0 < idx1) containers)
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
								, id 		= "taskform-" +++ ti.TaskInfo.taskId
								, taskId	= ti.TaskInfo.taskId
								, subject	= ti.TaskInfo.subject
								, result	= toString result
								})
	_
		= TaskNotDone

//Specialized JSON encoding of TUI definitions

//JSON Encoding of TUI definitions is directly encoded as JSON data.
derive JSONEncode TUIButton, TUIUpdate, TUIMenuButton, TUIMenu, TUIMenuItem, Key, Hotkey
derive JSONEncode TUIBasicControl, TUICurrencyControl, TUIDocumentControl, TUIConstructorControl
derive JSONEncode TUIButtonControl, TUIListItemControl, TUIChoiceControl, TUIAppletControl, TUIORYXControl
derive JSONEncode TUIStaticContainer, TUIRecordContainer, TUIListContainer, TUIHtmlContainer, TUIGridControl, TUIGridColumn, TUITreeControl, TUITree

//TODO: Separate control elements from form-widgets
JSONEncode{|TUIDef|} (TUIButton r)				= addXType "itasks.ttc.Button" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIMenuButton r)			= addXType "button" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuItem r)			= addXType "itasks.ttc.MenuItem" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIMenuSeparator)			= [JSONRaw "{\"xtype\":\"menuseparator\"}"]
JSONEncode{|TUIDef|} (TUICustom r)				= JSONEncode{|*|} r

JSONEncode{|TUIDef|} (TUIStringControl r)		= addXType "itasks.tui.String" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICharControl r)			= addXType "itasks.tui.Char" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIIntControl r)			= addXType "itasks.tui.Int" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRealControl r)			= addXType "itasks.tui.Real" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIBoolControl r)			= addXType "itasks.tui.Bool" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIChoiceControl r)		= addXType "itasks.tui.Choice" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUINoteControl r)			= addXType "itasks.tui.Note" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDateControl r)			= addXType "itasks.tui.Date" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITimeControl r)			= addXType "itasks.tui.Time" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHiddenControl r)		= addXType "itasks.tui.Hidden" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIFormButtonControl r)	= addXType "itasks.tui.FormButton" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUICurrencyControl r)		= addXType "itasks.tui.Currency" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIUserControl r)			= addXType "itasks.tui.Username" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIPasswordControl r)		= addXType "itasks.tui.Password" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIDocumentControl r)	 	= addXType "itasks.tui.Document" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIConstructorControl r)	= addXType "itasks.tui.Constructor" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListItemControl r) 	= addXType "itasks.tui.list.Item" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIAppletControl r)		= addXType "itasks.tui.Applet" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIORYXControl r)			= addXType "itasks.tui.Oryx" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIGridControl r)			= addXType "itasks.tui.Grid" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUITreeControl r)			= addXType "itasks.tui.Tree" (JSONEncode{|*|} r)

JSONEncode{|TUIDef|} (TUIStaticContainer r)		= addXType "itasks.tui.Static" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIRecordContainer r)		= addXType "itasks.tui.Record" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIListContainer r) 		= addXType "itasks.tui.List" (JSONEncode{|*|} r)
JSONEncode{|TUIDef|} (TUIHtmlContainer r)		= addXType "itasks.tui.Html" (JSONEncode{|*|} r)

addXType :: !String ![JSONNode] -> [JSONNode]
addXType xtype [JSONObject fields: xs]	= [JSONObject [("xtype", JSONString xtype):fields] : xs]
addXType xtype nodes					= nodes
