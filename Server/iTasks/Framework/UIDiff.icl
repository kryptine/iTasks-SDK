implementation module iTasks.Framework.UIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
import Text, Text.JSON, Data.Map
import iTasks.Framework.Util, iTasks.Framework.UIDefinition
from iTasks.Framework.Task import :: Event(..), :: EventNo

:: DiffResult
	= DiffImpossible
	| DiffPossible [UIUpdate]

:: UIDiffers	:== Map (String,String) (JSONNode JSONNode -> Maybe JSONNode)

derive gEq UISizeOpts, UISide, UISize, UIMinSize, UISideSizes, UIViewOpts, UISliderOpts, UIProgressOpts, UIButtonOpts
derive gEq UIGridOpts, UITreeOpts, UITreeNode, UIMenuButtonOpts, UIMenuItem, UIActionOpts
derive gEq UILabelOpts, UIIconOpts
derive gEq UIViewport, UIWindow, UIControl, UIItemsOpts, UIWindowOpts, UIFieldSetOpts, UIPanelOpts, UIViewportOpts, UIChoiceOpts, UIEditOpts, UIVAlign, UIHAlign, UIDirection, UITabSetOpts, UITab, UITabOpts
derive gEq UIDef, UIControlStack, UISubUI, UISubUIStack, UIAction
derive gEq UITaskletOpts, UITaskletPHOpts, UIEditletOpts

derive JSONEncode UITreeNode, UIActionOpts, UISizeOpts, UISideSizes, UIMinSize, UISize

//TODO Make a good diffViewports function that considers also the other parts of a viewport
diffUIDefinitions :: !UIDef !UIDef !Event !UIDiffers -> [UIUpdate]	
diffUIDefinitions (UIFinal (UIViewport iOpts1 opts1)) (UIFinal (UIViewport iOpts2 opts2)) event differs
	=	diffItems [] event differs iOpts1.UIItemsOpts.items iOpts2.UIItemsOpts.items
	++	diffAllWindows event differs opts1.UIViewportOpts.windows opts2.UIViewportOpts.windows
	++	(case (diffHotkeys (fromMaybe [] opts1.UIViewportOpts.hotkeys) (fromMaybe [] opts2.UIViewportOpts.hotkeys)) of [] = []; ops = [UIUpdate [] ops])
	++	if (opts1.UIViewportOpts.title === opts2.UIViewportOpts.title) [] [UIUpdate [] [("setTitle",[toJSON opts2.UIViewportOpts.title])]]

diffUIDefinitions d1 d2 event differs
	= diffItems [] event differs (uiDefControls d1) (uiDefControls d2) 

//Compare controls
diffControls :: !UIPath !Event !UIDiffers !UIControl !UIControl -> DiffResult
diffControls path event differs c1 c2
	# parts = case (c1,c2) of
		(UIViewString sOpts1 vOpts1, UIViewString sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewHtml sOpts1 vOpts1, UIViewHtml sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewDocument sOpts1 vOpts1, UIViewDocument sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewCheckbox sOpts1 vOpts1, UIViewCheckbox sOpts2 vOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2]
		(UIViewSlider sOpts1 vOpts1 opts1, UIViewSlider sOpts2 vOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2,diffOpts opts1 opts2]
		(UIViewProgress sOpts1 vOpts1 opts1, UIViewProgress sOpts2 vOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffViewOpts path vOpts1 vOpts2,diffOpts opts1 opts2]
		(UIEditString sOpts1 eOpts1, UIEditString sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditNote sOpts1 eOpts1, UIEditNote sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditPassword sOpts1 eOpts1, UIEditPassword sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditInt sOpts1 eOpts1, UIEditInt sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditDecimal sOpts1 eOpts1, UIEditDecimal sOpts2 eOpts2)	
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditCheckbox sOpts1 eOpts1, UIEditCheckbox sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditSlider sOpts1 eOpts1 opts1, UIEditSlider sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIEditDate sOpts1 eOpts1, UIEditDate sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditTime sOpts1 eOpts1, UIEditTime sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditDocument sOpts1 eOpts1, UIEditDocument sOpts2 eOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2]
		(UIEditButton sOpts1 eOpts1 opts1, UIEditButton sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIDropdown sOpts1 cOpts1, UIDropdown sOpts2 cOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2]
		(UIGrid sOpts1 cOpts1 opts1, UIGrid sOpts2 cOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2,diffOpts opts1 opts2]
		(UITree sOpts1 cOpts1 opts1, UITree sOpts2 cOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2,diffOpts opts1 opts2]
		(UIActionButton sOpts1 aOpts1 opts1, UIActionButton sOpts2 aOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffActionOpts path aOpts1 aOpts2,diffButtonOpts path opts1 opts2]
		(UIMenuButton sOpts1 opts1, UIMenuButton sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UILabel sOpts1 opts1, UILabel sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UIIcon sOpts1 opts1, UIIcon sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffIconOpts path opts1 opts2]
		// Tasklet on the right hand side:
		// check their instance id. Different: replace, Equals: update (mostly taskId)
		(UITasklet sOpts1 opts1, UITasklet sOpts2 opts2)
			| opts1.UITaskletOpts.taskId == opts2.UITaskletOpts.taskId
				= [DiffPossible [UIUpdate path [("selfUpdate",[encodeUIControl
					(UITaskletPH sOpts2 {UITaskletPHOpts|taskId = opts2.UITaskletOpts.taskId, updateVal = Nothing})])]]]
				= [DiffImpossible]
		(UITaskletPH sOpts1 opts1, UITasklet sOpts2 opts2)
			| opts1.UITaskletPHOpts.taskId == opts2.UITaskletOpts.taskId		
				= [DiffPossible [UIUpdate path [("selfUpdate",[encodeUIControl
					(UITaskletPH sOpts2 {UITaskletPHOpts|taskId = opts2.UITaskletOpts.taskId, updateVal = Nothing})])]]]
				= [DiffImpossible]
		// Placeholder on the right hand side: update
		(UITaskletPH sOpts1 opts1, UITaskletPH sOpts2 opts2)
			| opts1.UITaskletPHOpts.taskId == opts2.UITaskletPHOpts.taskId		
				= [DiffPossible [UIUpdate path [("selfUpdate",[encodeUIControl (UITaskletPH sOpts2 opts2)])]]]
				= [DiffImpossible]
		(UITasklet sOpts1 opts1, UITaskletPH sOpts2 opts2)
			| opts1.UITaskletOpts.taskId == opts2.UITaskletPHOpts.taskId		
				= [DiffPossible [UIUpdate path [("selfUpdate",[encodeUIControl (UITaskletPH sOpts2 opts2)])]]]
				= [DiffImpossible]
		// Editlets have custom diff functions which are passed in separately
		(UIEditlet sOpts1 opts1, UIEditlet sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2, diffEditletOpts path differs opts1 opts2]
		(UIContainer sOpts1 iOpts1, UIContainer sOpts2 iOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event differs iOpts1 iOpts2]
		(UIPanel sOpts1 iOpts1 opts1, UIPanel sOpts2 iOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event differs iOpts1 iOpts2, diffPanelOpts path event differs opts1 opts2]
		(UIFieldSet sOpts1 iOpts1 opts1, UIFieldSet sOpts2 iOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event differs iOpts1 iOpts2, diffOpts opts1 opts2]
		(UITabSet sOpts1 opts1, UITabSet sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2, diffTabSetOpts path event differs opts1 opts2]
		(_,_)
			= [DiffImpossible]		
	= DiffPossible (replaceControlIfImpossible path c2 parts)

//As a first step, only do diffs for value changes, all other diffs trigger replacements...
diffSizeOpts :: UIPath UISizeOpts UISizeOpts -> DiffResult
diffSizeOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible

diffViewOpts :: UIPath (UIViewOpts a) (UIViewOpts a) -> DiffResult | gEq{|*|} a & encodeUIValue a
diffViewOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffPossible [UIUpdate path [("setValue",[encodeUIValue opts2.UIViewOpts.value])]]

diffEditOpts :: UIPath Event UIEditOpts UIEditOpts -> DiffResult
diffEditOpts path event opts1 opts2
	| isEmpty taskIdUpd && isEmpty editorIdUpd
		= DiffPossible (flatten [taskIdUpd,editorIdUpd,valueUpd])
	| otherwise
		= DiffImpossible
where
	taskIdUpd	= if (opts1.UIEditOpts.taskId == opts2.UIEditOpts.taskId) [] [UIUpdate path [("setTaskId",[toJSON opts2.UIEditOpts.taskId])]]
	editorIdUpd = if (opts1.UIEditOpts.editorId == opts2.UIEditOpts.editorId) [] [UIUpdate path [("setEditorId",[toJSON opts2.UIEditOpts.editorId])]]
	valueUpd
		| eventMatch opts2 event
			= if (eventValue event === opts2.UIEditOpts.value)  [] [UIUpdate path [("setEditorValue",[toJSON opts2.UIEditOpts.value])]]
		| otherwise
			= if (opts1.UIEditOpts.value === opts2.UIEditOpts.value) [] [UIUpdate path [("setEditorValue",[toJSON opts2.UIEditOpts.value])]]

	eventMatch {UIEditOpts|taskId,editorId} (EditEvent _ matchTask matchEditor _) = (taskId == toString matchTask) && (editorId == matchEditor)
	eventMatch _ _ = False
	
	eventValue (EditEvent _ _ _ value) = Just value
	
diffEditletOpts :: UIPath UIDiffers UIEditletOpts UIEditletOpts -> DiffResult
diffEditletOpts path differs opts1 opts2
	| opts1.UIEditletOpts.value === opts2.UIEditletOpts.value	= DiffPossible []
	//Check if we have a local diff function for this editor...
	| otherwise
		= case get (opts2.UIEditletOpts.taskId,opts2.UIEditletOpts.editorId) differs of
			Just differ = case differ opts1.UIEditletOpts.value opts2.UIEditletOpts.value of
				Just diff	= DiffPossible [UIUpdate path [("applyDiff",[diff])]]
				_			= DiffPossible []
			_
				= DiffImpossible

diffChoiceOpts :: UIPath (UIChoiceOpts a) (UIChoiceOpts a) -> DiffResult | gEq{|*|} a & JSONEncode{|*|} a
diffChoiceOpts path opts1 opts2
	| opts1.UIChoiceOpts.taskId <> opts2.UIChoiceOpts.taskId		= DiffImpossible
	| opts1.UIChoiceOpts.editorId <> opts2.UIChoiceOpts.editorId	= DiffImpossible 
	| opts1.UIChoiceOpts.options =!= opts2.UIChoiceOpts.options		= DiffImpossible
	= DiffPossible valueDiff //(valueDiff ++ optionDiff)
where
	valueDiff	= if (opts1.UIChoiceOpts.value === opts2.UIChoiceOpts.value) [] [UIUpdate path [("setValue",[toJSON opts2.UIChoiceOpts.value,JSONBool True])]]
	optionDiff	= if (opts1.UIChoiceOpts.options === opts2.UIChoiceOpts.options) [] [UIUpdate path [("setOptions",[toJSON opts2.UIChoiceOpts.options])]]

diffActionOpts :: UIPath UIActionOpts UIActionOpts -> DiffResult
diffActionOpts path opts1 opts2 = DiffPossible (flatten [taskIdUpd,actionIdUpd])
where
	taskIdUpd	= if (opts1.UIActionOpts.taskId == opts2.UIActionOpts.taskId) [] [UIUpdate path [("setTaskId",[toJSON opts2.UIActionOpts.taskId])]]
	actionIdUpd	= if (opts1.UIActionOpts.actionId == opts2.UIActionOpts.actionId) [] [UIUpdate path [("setActionId",[toJSON opts2.UIActionOpts.actionId])]]

diffItemsOpts :: UIPath !Event UIDiffers UIItemsOpts UIItemsOpts -> DiffResult
diffItemsOpts path event differs opts1 opts2
	| opts1.UIItemsOpts.direction =!= opts2.UIItemsOpts.direction	= DiffImpossible
	| opts1.UIItemsOpts.halign =!= opts2.UIItemsOpts.halign			= DiffImpossible
	| opts1.UIItemsOpts.valign =!= opts2.UIItemsOpts.valign			= DiffImpossible
	| opts1.UIItemsOpts.padding =!= opts2.UIItemsOpts.padding		= DiffImpossible
	| opts1.UIItemsOpts.baseCls =!= opts2.UIItemsOpts.baseCls       = DiffImpossible
	| opts1.UIItemsOpts.bodyCls =!= opts2.UIItemsOpts.bodyCls       = DiffImpossible
	| otherwise
		= DiffPossible (diffItems path event differs opts1.UIItemsOpts.items opts2.UIItemsOpts.items)

diffTabSetOpts :: UIPath !Event UIDiffers UITabSetOpts UITabSetOpts -> DiffResult
diffTabSetOpts path event differs opts1 opts2
	# (tabUpdates,replacedTabs) = diffTabItemsSet path event differs opts1.UITabSetOpts.items opts2.UITabSetOpts.items 
	= DiffPossible (tabUpdates ++ activeTabUpd replacedTabs)
where
	activeTabUpd replacedTabs
	| opts1.UITabSetOpts.activeTab =!= opts2.UITabSetOpts.activeTab || maybe False (\i -> isMember i replacedTabs) opts2.UITabSetOpts.activeTab
		= [UIUpdate path [("setActiveTab",[toJSON opts2.UITabSetOpts.activeTab,JSONBool True])]]
		= []

diffOpts :: a a -> DiffResult | gEq{|*|} a	//Very crude, but always working fallback diff
diffOpts opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible

//Let the client figure out the update if there are differences
selfUpdate :: UIPath UIControl UIControl -> DiffResult
selfUpdate path c1 c2
	| c1 === c2	= DiffPossible []
				= DiffPossible [UIUpdate path [("selfUpdate",[encodeUIControl c2])]]

//Group multiple operations in a single component update
diffMultiProperties :: UIPath [[UIUpdateOperation]] -> [UIUpdate]
diffMultiProperties path ops = case flatten ops of
	[]	= []
	ops	= [UIUpdate path ops]

//Specialized diffs for the control specific options
diffPanelOpts :: UIPath Event UIDiffers UIPanelOpts UIPanelOpts -> DiffResult
diffPanelOpts path event differs opts1 opts2
	| impossible	= DiffImpossible
					= case flatten [titleUpd,hotkeyUpd] of
						[]	= DiffPossible menusUpd
						ops	= DiffPossible [UIUpdate path ops:menusUpd]
where
	impossible	=  opts1.UIPanelOpts.frame <> opts2.UIPanelOpts.frame
				|| opts1.UIPanelOpts.iconCls <> opts2.UIPanelOpts.iconCls
				|| (isJust opts1.UIPanelOpts.tbar && isNothing opts2.UIPanelOpts.tbar)	//Can only update menu items, not create a menubar suddenly
				|| (isNothing opts1.UIPanelOpts.tbar && isJust opts2.UIPanelOpts.tbar)
				
	titleUpd	= if (opts1.UIPanelOpts.title == opts2.UIPanelOpts.title) [] [("setTitle",[toJSON opts2.UIPanelOpts.title])]
	hotkeyUpd	= diffHotkeys (fromMaybe [] opts1.UIPanelOpts.hotkeys) (fromMaybe [] opts2.UIPanelOpts.hotkeys)
	menusUpd	= diffItems [MenuStep:path] event differs (fromMaybe [] opts1.UIPanelOpts.tbar) (fromMaybe [] opts2.UIPanelOpts.tbar)

diffWindowOpts :: UIPath Event UIDiffers UIWindowOpts UIWindowOpts -> DiffResult
diffWindowOpts path event differs opts1 opts2 
	| impossible	= DiffImpossible
					= case flatten [titleUpd,hotkeyUpd] of
						[]	= DiffPossible menusUpd
						ops	= DiffPossible [UIUpdate path ops:menusUpd]
where
	impossible	=  opts1.UIWindowOpts.focusTaskId	=!= opts2.UIWindowOpts.focusTaskId //TODO Make more possible on client
				|| opts1.UIWindowOpts.closeTaskId	=!= opts2.UIWindowOpts.closeTaskId
				|| opts1.UIWindowOpts.iconCls		=!= opts2.UIWindowOpts.iconCls

	titleUpd	= if (opts1.UIWindowOpts.title == opts2.UIWindowOpts.title) [] [("setTitle",[toJSON opts2.UIWindowOpts.title])]
	hotkeyUpd	= diffHotkeys (fromMaybe [] opts1.UIWindowOpts.hotkeys) (fromMaybe [] opts2.UIWindowOpts.hotkeys)
	menusUpd	= diffItems [MenuStep:path] event differs (fromMaybe [] opts1.UIWindowOpts.tbar) (fromMaybe [] opts2.UIWindowOpts.tbar)

diffButtonOpts :: UIPath UIButtonOpts UIButtonOpts -> DiffResult
diffButtonOpts path b1 b2 = DiffPossible (diffMultiProperties path [textUpd,iconUpd,enabledUpd])
where	
	textUpd = if (b1.UIButtonOpts.text === b2.UIButtonOpts.text) [] [("setText",[toJSON b2.UIButtonOpts.text])]
	iconUpd = if (b1.UIButtonOpts.iconCls === b2.UIButtonOpts.iconCls) [] [("setIconCls",[toJSON b2.UIButtonOpts.iconCls])]
	enabledUpd = if (b1.UIButtonOpts.disabled === b2.UIButtonOpts.disabled) [] [("setDisabled",[toJSON b2.UIButtonOpts.disabled])]

diffIconOpts :: UIPath UIIconOpts UIIconOpts -> DiffResult
diffIconOpts path i1 i2 = DiffPossible (diffMultiProperties path [iconUpd,tooltipUpd])
where
	iconUpd = if (i1.UIIconOpts.iconCls === i2.UIIconOpts.iconCls) [] [("setIconCls",[toJSON i2.UIIconOpts.iconCls])]
	tooltipUpd = if (i1.UIIconOpts.tooltip === i2.UIIconOpts.tooltip) [] [("setTooltip",[toJSON i2.UIIconOpts.tooltip])]

diffTabOpts :: UIPath Event UIDiffers UITabOpts UITabOpts -> DiffResult
diffTabOpts path event differs t1 t2
	| (isJust t1.UITabOpts.tbar && isNothing t1.UITabOpts.tbar)	//Can only update menu items, not create a menubar suddenly
		|| (isNothing t1.UITabOpts.tbar && isJust t2.UITabOpts.tbar)	
		|| (isJust t1.UITabOpts.closeTaskId && isNothing t2.UITabOpts.closeTaskId) //We cannot make a non-closable tab closable or vice-versa
		|| (isNothing t1.UITabOpts.closeTaskId && isJust t2.UITabOpts.closeTaskId) 
		= DiffImpossible
	| otherwise
		= DiffPossible ((diffMultiProperties path [titleUpd,focusUpd,closeUpd,iconUpd,hotkeyUpd]) ++ menusUpd)
where
	titleUpd = if (t1.UITabOpts.title === t2.UITabOpts.title) [] [("setTitle",[toJSON t2.UITabOpts.title])]
	focusUpd = if (t1.UITabOpts.focusTaskId === t2.UITabOpts.focusTaskId) [] [("setFocusTaskId",[toJSON t2.UITabOpts.focusTaskId])]
	closeUpd = if (t1.UITabOpts.closeTaskId === t2.UITabOpts.closeTaskId) [] [("setCloseTaskId",[toJSON t2.UITabOpts.closeTaskId])]
	iconUpd = if (t1.UITabOpts.iconCls === t2.UITabOpts.iconCls) [] [("setIconCls",[toJSON t2.UITabOpts.iconCls])]
	menusUpd = diffItems [MenuStep:path] event differs (fromMaybe [] t1.UITabOpts.tbar) (fromMaybe [] t2.UITabOpts.tbar)
	hotkeyUpd = diffHotkeys (fromMaybe [] t1.UITabOpts.hotkeys) (fromMaybe [] t2.UITabOpts.hotkeys)

diffItems :: UIPath Event UIDiffers [UIControl] [UIControl] -> [UIUpdate]
diffItems path event differs items1 items2 = diff path event 0 items1 items2
where
	diff path event i [] []
		= []
	diff path event i items1 [] //Less items in new than old (remove starting with the last item)
		= [UIUpdate path [("remove",[toJSON n])] \\ n <- reverse [i.. i + length items1 - 1 ]] 
	diff path event i [] items2 //More items in new than old
		= [UIUpdate path [("add",[toJSON n,encodeUIControl def])] \\ n <- [i..] & def <- items2]	
	diff path event i [c1:c1s] [c2:c2s] //Compare side by side
		=	replaceControlIfImpossible [ItemStep i:path] c2 [diffControls [ItemStep i:path] event differs c1 c2]
		++  diff path event (i + 1) c1s c2s

diffTabItemsSet :: UIPath Event UIDiffers [UITab] [UITab] -> ([UIUpdate],[Int])
diffTabItemsSet path event differs items1 items2 = diff path event 0 items1 items2
where
	diff path event i [] []
		= ([],[])
	diff path event i items1 [] 
		= ([UIUpdate path [("remove",[toJSON n])] \\ n <- reverse [i.. i + length items1 - 1 ]], [])
	diff path event i [] items2 //More items in new than old
		= ([UIUpdate path [("add",[toJSON n,encodeUITab def])] \\ n <- [i..] & def <- items2], [])	
	diff path event i [c1:c1s] [c2:c2s] //Compare side by side
		# (tabUpdates,replaced) = diffTabs [ItemStep i:path] event differs c1 c2
		# (restUpdates,replacedTabs) = diff path event (i + 1) c1s c2s
		= (tabUpdates ++ restUpdates,replacedTabs ++ if replaced [i] [])

diffTabs :: UIPath Event UIDiffers UITab UITab -> ([UIUpdate],Bool)
diffTabs path event differs t1=:(UITab iOpts1 opts1) t2=:(UITab iOpts2 opts2)
	# parts = [diffItemsOpts path event differs iOpts1 iOpts2, diffTabOpts path event differs opts1 opts2]
	| allDiffsPossible parts
		= (flatten [d \\ DiffPossible d <- parts],False)
	| otherwise
        = ([UIUpdate parentPath [("replace",[toJSON parentIndex,encodeUITab t2])]],True)
where
	[ItemStep parentIndex:parentPath] = path

diffAllWindows :: Event UIDiffers [UIWindow] [UIWindow] -> [UIUpdate]
diffAllWindows event differs windows1 windows2 = diff event 0 windows1 windows2
where
	diff event i [] []
		= []
	diff event i windows1 [] //Less windows
		= [UIUpdate [] [("removeWindow",[toJSON n])] \\ n <- reverse [i.. i + length windows1 - 1 ]] 
	diff event i [] items2 //More windows
		= [UIUpdate [] [("addWindow",[toJSON n,encodeUIWindow def])] \\ n <- [i..] & def <- windows2]	
	diff event i [w1:w1s] [w2:w2s] //Compare side by side (TODO: Make more granular)
		= diffWindows [WindowStep i] event differs w1 w2 ++ diff event (i + 1) w1s w2s
		++  diff event (i + 1) w1s w2s

diffWindows :: UIPath Event UIDiffers UIWindow UIWindow -> [UIUpdate]
diffWindows path event differs w1=:(UIWindow sOpts1 iOpts1 opts1) w2=:(UIWindow sOpts2 iOpts2 opts2)
	= replaceWindowIfImpossible path w2 [diffSizeOpts path sOpts1 sOpts2
										,diffItemsOpts path event differs iOpts1 iOpts2
										,diffWindowOpts path event differs opts1 opts2]

diffHotkeys :: [UIKeyAction] [UIKeyAction] -> [UIUpdateOperation]
diffHotkeys keys1 keys2 = if (keys1 === keys2) [] [("setHotkeys",[toJSON keys2])]

//Try to diff a control in parts. If one of the parts is impossible, then return a full replace instruction
replaceControlIfImpossible :: UIPath UIControl [DiffResult] -> [UIUpdate]
replaceControlIfImpossible path fallback parts
	| allDiffsPossible parts	= flatten [d \\DiffPossible d <- parts]
								= [UIUpdate parentPath [("remove",[toJSON parentIndex])
													   ,("add",[toJSON parentIndex,encodeUIControl fallback])]]
where
	[ItemStep parentIndex:parentPath] = path

replaceWindowIfImpossible :: UIPath UIWindow [DiffResult] -> [UIUpdate]
replaceWindowIfImpossible path fallback parts
	| allDiffsPossible parts	= flatten [d \\ DiffPossible d <- parts]
								= [UIUpdate [] [("removeWindow",[toJSON windowIndex])
											   ,("addWindow",[toJSON windowIndex,encodeUIWindow fallback])]]
where
	[WindowStep windowIndex:_]	= path

allDiffsPossible :: [DiffResult] -> Bool
allDiffsPossible [] 					= True
allDiffsPossible [DiffImpossible:_]		= False
allDiffsPossible [(DiffPossible _):ps]	= allDiffsPossible ps

encodeUIUpdates :: ![UIUpdate] -> JSONNode
encodeUIUpdates updates = JSONArray (map encodeUIUpdate updates)

encodeUIUpdate :: !UIUpdate -> JSONNode
encodeUIUpdate (UIUpdate path operations)
	=	JSONObject [("path",encodeUIPath path)
					,("operations", JSONArray [JSONObject [("method",JSONString method),("arguments",JSONArray arguments)]
											  \\ (method,arguments) <- operations])]

encodeUIPath :: UIPath -> JSONNode
encodeUIPath path = JSONArray (enc (reverse path))
where
	enc [] = []
	enc [ItemStep i:ss]		= [JSONInt i:enc ss]
	enc [MenuStep:ss]		= [JSONString "m":enc ss]
	enc [WindowStep i:ss]	= [JSONString "w",JSONInt i:enc ss]
