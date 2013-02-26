implementation module UIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
import Text, Util, UIDefinition
from Task import :: Event(..)

:: DiffResult
	= DiffImpossible
	| DiffPossible [UIUpdate]

derive gEq UISizeOpts, UISide, UISize, UIMinSize, UISideSizes, UIViewOpts, UISliderOpts, UIProgressOpts, UIButtonOpts
derive gEq UIGoogleMapOpts, UIGoogleMapMarker, UIGoogleMapOptions, UICodeOpts, UIGridOpts, UITreeNode, UIMenuButtonOpts, UIMenuItem, UIActionOpts
derive gEq UILabelOpts, UITabOpts, UIIconOpts, UITaskletOpts, UITaskletPHOpts
derive gEq UIViewport, UIWindow, UIControl, UIItemsOpts, UIWindowOpts, UIFieldSetOpts, UIPanelOpts, UIContainerOpts, UIViewportOpts, UIChoiceOpts, UIEditOpts, UIVAlign, UIHAlign, UIDirection
derive gEq UIDef, UIControlSequence, UIActionSet, UIControlGroup, UIAbstractContainer, UIAction

derive JSONEncode UITreeNode, UIActionOpts, UISizeOpts, UISideSizes, UIMinSize, UISize

//TODO Make a good diffViewports function that considers also the other parts of a viewport
diffUIDefinitions :: !UIDef !UIDef !Event -> [UIUpdate]	
diffUIDefinitions (UIFinal (UIViewport iOpts1 opts1)) (UIFinal (UIViewport iOpts2 opts2)) event
	=	diffItems [] event iOpts1.UIItemsOpts.items iOpts2.UIItemsOpts.items
	++	diffAllWindows event opts1.UIViewportOpts.windows opts2.UIViewportOpts.windows
	++	diffHotkeys [] (fromMaybe [] opts1.UIViewportOpts.hotkeys) (fromMaybe [] opts2.UIViewportOpts.hotkeys)

diffUIDefinitions d1 d2 event
	= diffItems [] event (uiDefControls d1) (uiDefControls d2) 

//Compare controls
diffControls :: !UIPath !Event !UIControl !UIControl -> DiffResult
diffControls path event c1 c2
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
		(m1=:UIEditGoogleMap sOpts1 eOpts1 opts1,m2=:UIEditGoogleMap sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts eOpts1 eOpts2, diffOpts opts1 opts2/* selfUpdate path m1 m2 */]
		(UIEditCode sOpts1 eOpts1 opts1, UIEditCode sOpts2 eOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffEditOpts path event eOpts1 eOpts2,diffOpts opts1 opts2]
		(UIDropdown sOpts1 cOpts1, UIDropdown sOpts2 cOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2]
		(UIGrid sOpts1 cOpts1 opts1, UIGrid sOpts2 cOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2,diffOpts opts1 opts2]
		(UITree sOpts1 cOpts1, UITree sOpts2 cOpts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffChoiceOpts path cOpts1 cOpts2]
		(UIActionButton sOpts1 aOpts1 opts1, UIActionButton sOpts2 aOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffActionOpts path aOpts1 aOpts2,diffButtonOpts path opts1 opts2]
		(UIMenuButton sOpts1 opts1, UIMenuButton sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UILabel sOpts1 opts1, UILabel sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffOpts opts1 opts2]
		(UIIcon sOpts1 opts1, UIIcon sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffIconOpts path opts1 opts2]
		(UITab sOpts1 opts1, UITab sOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffTabOpts path opts1 opts2]
		// Tasklet on the right hand side:
		// check their instance id. Different: replace, Equals: update (mostly taskId)
		(UITasklet sOpts1 opts1, UITasklet sOpts2 opts2)
			| opts1.UITaskletOpts.iid == opts2.UITaskletOpts.iid
				= [DiffPossible [UIUpdate path (UISelfUpdate
						(UITaskletPH sOpts2 {UITaskletPHOpts|iid = opts2.UITaskletOpts.iid, taskId = opts2.UITaskletOpts.taskId}))]]
				= [DiffImpossible]

		(UITaskletPH sOpts1 opts1, UITasklet sOpts2 opts2)
			| opts1.UITaskletPHOpts.iid == opts2.UITaskletOpts.iid		
				= [DiffPossible [UIUpdate path (UISelfUpdate 
						(UITaskletPH sOpts2 {UITaskletPHOpts|iid = opts2.UITaskletOpts.iid, taskId = opts2.UITaskletOpts.taskId}))]]
				= [DiffImpossible]
		// Placeholder on the right hand side: update
		(UITaskletPH sOpts1 opts1, UITaskletPH sOpts2 opts2)
			| opts1.UITaskletPHOpts.iid == opts2.UITaskletPHOpts.iid		
				= [DiffPossible [UIUpdate path (UISelfUpdate (UITaskletPH sOpts2 opts2))]]
				= [DiffImpossible]
		(UITasklet sOpts1 opts1, UITaskletPH sOpts2 opts2)
			| opts1.UITaskletOpts.iid == opts2.UITaskletPHOpts.iid		
				= [DiffPossible [UIUpdate path (UISelfUpdate (UITaskletPH sOpts2 opts2))]]
				= [DiffImpossible]

		(UIContainer sOpts1 iOpts1 opts1, UIContainer sOpts2 iOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event iOpts1 iOpts2, diffOpts opts1 opts2]
		(UIPanel sOpts1 iOpts1 opts1, UIPanel sOpts2 iOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event iOpts1 iOpts2, diffPanelOpts path event opts1 opts2]
		(UIFieldSet sOpts1 iOpts1 opts1, UIFieldSet sOpts2 iOpts2 opts2)
			= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event iOpts1 iOpts2, diffOpts opts1 opts2]
		//(UIWindow sOpts1 iOpts1 opts1, UIWindow sOpts2 iOpts2 opts2)
		//	= [diffSizeOpts path sOpts1 sOpts2,diffItemsOpts path event iOpts1 iOpts2, diffOpts opts1 opts2]
		(_,_)
			= [DiffImpossible]		
	= DiffPossible (replaceIfImpossible path c2 parts)

//As a first step, only do diffs for value changes, all other diffs trigger replacements...
diffSizeOpts :: UIPath UISizeOpts UISizeOpts -> DiffResult
diffSizeOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible // DiffPossible [UIResize (toString path) opts2]

diffViewOpts :: UIPath (UIViewOpts a) (UIViewOpts a) -> DiffResult | gEq{|*|} a & encodeUIValue a
diffViewOpts path opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffPossible [UIUpdate path (UISetValue (encodeUIValue opts2.UIViewOpts.value))]

diffEditOpts :: UIPath Event (UIEditOpts a) (UIEditOpts a) -> DiffResult | gEq{|*|} a & encodeUIValue a
diffEditOpts path event opts1 opts2
	| isEmpty taskIdUpd && isEmpty editorIdUpd
		= DiffPossible (foldr (++) [] [taskIdUpd,editorIdUpd,valueUpd])
	| otherwise
		= DiffImpossible
where
	taskIdUpd	= if (opts1.UIEditOpts.taskId == opts2.UIEditOpts.taskId) [] [UIUpdate path (UISetTaskId opts2.UIEditOpts.taskId)]
	editorIdUpd = if (opts1.UIEditOpts.editorId == opts2.UIEditOpts.editorId) [] [UIUpdate path (UISetEditorId opts2.UIEditOpts.editorId)]
	valueUpd
		| eventMatch opts2 event
			# value2 = encodeUIValue opts2.UIEditOpts.value
			= if (eventValue event === value2)  [] [UIUpdate path (UISetValue value2)]
		| otherwise 
			= if (opts1.UIEditOpts.value === opts2.UIEditOpts.value) [] [UIUpdate path (UISetValue (encodeUIValue opts2.UIEditOpts.value))]

	eventMatch {UIEditOpts|taskId,editorId} (EditEvent matchTask matchEditor _) = (taskId == toString matchTask) && (editorId == matchEditor)
	eventMatch _ _ = False
	
	eventValue (EditEvent _ _ value) = value
	
diffChoiceOpts :: UIPath (UIChoiceOpts a) (UIChoiceOpts a) -> DiffResult | gEq{|*|} a & JSONEncode{|*|} a
diffChoiceOpts path opts1 opts2
	| opts1.UIChoiceOpts.taskId <> opts2.UIChoiceOpts.taskId		= DiffImpossible
	| opts1.UIChoiceOpts.editorId <> opts2.UIChoiceOpts.editorId	= DiffImpossible 
	| opts1.UIChoiceOpts.options =!= opts2.UIChoiceOpts.options		= DiffImpossible
	= DiffPossible valueDiff //(valueDiff ++ optionDiff)
where
	valueDiff	= if (opts1.UIChoiceOpts.value === opts2.UIChoiceOpts.value) [] [UIUpdate path (UISetValue (toJSON opts2.UIChoiceOpts.value))]
	optionDiff	= if (opts1.UIChoiceOpts.options === opts2.UIChoiceOpts.options) [] [UIUpdate path (UISetOptions (toJSON opts2.UIChoiceOpts.options))]

diffActionOpts :: UIPath UIActionOpts UIActionOpts -> DiffResult
diffActionOpts path opts1 opts2 = DiffPossible (flatten [taskIdUpd,actionIdUpd])
where
	taskIdUpd	= if (opts1.UIActionOpts.taskId == opts2.UIActionOpts.taskId) [] [UIUpdate path (UISetTaskId opts2.UIActionOpts.taskId)]
	actionIdUpd	= if (opts1.UIActionOpts.actionId == opts2.UIActionOpts.actionId) [] [UIUpdate path (UISetActionId opts2.UIActionOpts.actionId)]

diffItemsOpts :: UIPath !Event UIItemsOpts UIItemsOpts -> DiffResult
diffItemsOpts path event opts1 opts2
	| opts1.UIItemsOpts.direction =!= opts2.UIItemsOpts.direction	= DiffImpossible
	| opts1.UIItemsOpts.halign =!= opts2.UIItemsOpts.halign			= DiffImpossible
	| opts1.UIItemsOpts.valign =!= opts2.UIItemsOpts.valign			= DiffImpossible
	| opts1.UIItemsOpts.padding =!= opts2.UIItemsOpts.padding		= DiffImpossible
	| otherwise
		= DiffPossible (diffItems path event opts1.UIItemsOpts.items opts2.UIItemsOpts.items)

diffOpts :: a a -> DiffResult | gEq{|*|} a	//Very crude, but always working fallback diff
diffOpts opts1 opts2
	| opts1 === opts2	= DiffPossible []
						= DiffImpossible

//Let the client figure out the update if there are differences
selfUpdate :: UIPath UIControl UIControl -> DiffResult
selfUpdate path c1 c2
	| c1 === c2	= DiffPossible []
				= DiffPossible [UIUpdate path (UISelfUpdate c2)]

//Specialized diffs for the control specific options
diffPanelOpts :: UIPath Event UIPanelOpts UIPanelOpts -> DiffResult
diffPanelOpts path event opts1 opts2
	| impossible	= DiffImpossible
					= DiffPossible (flatten [titleUpd,menusUpd,hotkeyUpd])
where
	impossible	=  opts1.UIPanelOpts.frame <> opts2.UIPanelOpts.frame
				|| opts1.UIPanelOpts.iconCls <> opts2.UIPanelOpts.iconCls
				|| opts1.UIPanelOpts.baseCls <> opts2.UIPanelOpts.baseCls
				|| opts1.UIPanelOpts.bodyCls <> opts2.UIPanelOpts.bodyCls
				|| (isJust opts1.UIPanelOpts.tbar && isNothing opts2.UIPanelOpts.tbar)	//Can only update menu items, not create a menubar suddenly
				|| (isNothing opts1.UIPanelOpts.tbar && isJust opts2.UIPanelOpts.tbar)
				
	titleUpd	= if (opts1.UIPanelOpts.title == opts2.UIPanelOpts.title) [] [UIUpdate path (UISetTitle opts2.UIPanelOpts.title)]
	menusUpd	= diffItems [MenuStep:path] event (fromMaybe [] opts1.UIPanelOpts.tbar) (fromMaybe [] opts2.UIPanelOpts.tbar)
	hotkeyUpd	= diffHotkeys path (fromMaybe [] opts1.UIPanelOpts.hotkeys) (fromMaybe [] opts2.UIPanelOpts.hotkeys)

diffButtonOpts :: UIPath UIButtonOpts UIButtonOpts -> DiffResult
diffButtonOpts path b1 b2 = DiffPossible (flatten [textUpd,iconUpd,enabledUpd])
where	
	textUpd = if (b1.UIButtonOpts.text === b2.UIButtonOpts.text) [] [UIUpdate path (UISetText b2.UIButtonOpts.text)]
	iconUpd = if (b1.UIButtonOpts.iconCls === b2.UIButtonOpts.iconCls) [] [UIUpdate path (UISetIconCls b2.UIButtonOpts.iconCls)]
	enabledUpd = if (b1.UIButtonOpts.disabled === b2.UIButtonOpts.disabled) [] [UIUpdate path (UISetEnabled (not b2.UIButtonOpts.disabled))]

diffIconOpts :: UIPath UIIconOpts UIIconOpts -> DiffResult
diffIconOpts path i1 i2 = DiffPossible (flatten [iconUpd,tooltipUpd])
where
	iconUpd = if (i1.UIIconOpts.iconCls === i2.UIIconOpts.iconCls) [] [UIUpdate path (UISetIconCls (Just i2.UIIconOpts.iconCls))]
	tooltipUpd = if (i1.UIIconOpts.tooltip === i2.UIIconOpts.tooltip) [] [UIUpdate path (UISetTooltip i2.UIIconOpts.tooltip)]

diffTabOpts :: UIPath UITabOpts UITabOpts -> DiffResult
diffTabOpts path t1 t2 = DiffPossible (flatten [textUpd,activeUpd,focusUpd,closeUpd,iconUpd])
where
	textUpd = if (t1.UITabOpts.text === t2.UITabOpts.text) [] [UIUpdate path (UISetText (Just t2.UITabOpts.text))]
	activeUpd = if (t1.UITabOpts.active == t2.UITabOpts.active) [] [UIUpdate path (UISetActive t2.UITabOpts.active)]
	focusUpd = if (t1.UITabOpts.focusTaskId === t2.UITabOpts.focusTaskId) [] [UIUpdate path (UISetFocusTaskId t2.UITabOpts.focusTaskId)]
	closeUpd = if (t1.UITabOpts.closeTaskId === t2.UITabOpts.closeTaskId) [] [UIUpdate path (UISetCloseTaskId t2.UITabOpts.closeTaskId)]
	iconUpd = if (t1.UITabOpts.iconCls === t2.UITabOpts.iconCls) [] [UIUpdate path (UISetIconCls t2.UITabOpts.iconCls)]

diffItems :: UIPath Event [UIControl] [UIControl] -> [UIUpdate]
diffItems path event items1 items2 = diff path event 0 items1 items2
where
	diff path event i [] []
		= []
	diff path event i items1 [] //Less items in new than old (remove starting with the last item)
		= [UIUpdate path (UIRemove n) \\ n <- reverse [i.. i + length items1 - 1 ]] 
	diff path event i [] items2 //More items in new than old
		= [UIUpdate path (UIAdd n def) \\ n <- [i..] & def <- items2]	
	diff path event i [c1:c1s] [c2:c2s] //Compare side by side
		=	replaceIfImpossible [ItemStep i:path] c2 [diffControls [ItemStep i:path] event c1 c2]
		++  diff path event (i + 1) c1s c2s

diffAllWindows :: Event [UIWindow] [UIWindow] -> [UIUpdate]
diffAllWindows event windows1 windows2 = diff event 0 windows1 windows2
where
	diff event i [] []
		= []
	diff event i windows1 [] //Less windows
		= [UIUpdate [] (UIRemoveWindow n) \\ n <- reverse [i.. i + length windows1 - 1 ]] 
	diff event i [] items2 //More windows
		= [UIUpdate [] (UIAddWindow n def) \\ n <- [i..] & def <- windows2]	
	diff event i [w1:w1s] [w2:w2s] //Compare side by side (TODO: Make more granular)
		=	if (w1 === w2) [] [UIUpdate [] (UIRemoveWindow i),UIUpdate [] (UIAddWindow i w2)]
		++  diff event (i + 1) w1s w2s

diffWindows :: UIPath Event UIWindow UIWindow -> DiffResult
diffWindows path event w1 w2 = DiffImpossible

diffHotkeys :: UIPath [UIKeyAction] [UIKeyAction] -> [UIUpdate]
diffHotkeys path keys1 keys2 = if (keys1 === keys2) [] [UIUpdate path (UISetHotkeys keys2)]

//Try to diff a control in parts. If one of the parts is impossible, then return a full replace instruction
replaceIfImpossible :: UIPath UIControl [DiffResult] -> [UIUpdate]
replaceIfImpossible path fallback parts
	| allPossible parts		= foldr (++) [] [d \\DiffPossible d <- parts]
							= [UIUpdate parentPath (UIReplace parentIndex fallback)]
where
	[ItemStep parentIndex:parentPath] = path
	
	allPossible [] 						= True
	allPossible [DiffImpossible:_]		= False
	allPossible [(DiffPossible _):ps]	= allPossible ps

encodeUIUpdates :: ![UIUpdate] -> JSONNode
encodeUIUpdates updates = JSONArray (map encodeUIUpdate updates)

encodeUIUpdate :: UIUpdate -> JSONNode
encodeUIUpdate (UIUpdate path operation)
	= let (method,arguments) = encodeUIUpdateOperation operation in
		JSONObject [("path",encodeUIPath path),("method",JSONString method),("arguments",JSONArray arguments)]

encodeUIPath :: UIPath -> JSONNode
encodeUIPath path = JSONArray (enc (reverse path))
where
	enc [] = []
	enc [ItemStep i:ss]		= [JSONInt i:enc ss]
	enc [MenuStep:ss]		= [JSONString "m":enc ss]
	enc [WindowStep i:ss]	= [JSONString "w",JSONInt i:enc ss]

encodeUIUpdateOperation :: UIUpdateOperation -> (!String,![JSONNode])
encodeUIUpdateOperation (UISetValue value)		= ("setValue",		[value])
encodeUIUpdateOperation (UISetOptions value)	= ("setOptions",	[value])
encodeUIUpdateOperation (UISetTaskId id)		= ("setTaskId",		[JSONString id])
encodeUIUpdateOperation (UISetCloseTaskId id)	= ("setCloseTaskId",[toJSON id])
encodeUIUpdateOperation (UISetFocusTaskId id)	= ("setFocusTaskId",[toJSON id])
encodeUIUpdateOperation (UISetEditorId id)		= ("setEditorId",	[JSONString id])
encodeUIUpdateOperation (UISetActionId id)		= ("setActionId",	[JSONString id])
encodeUIUpdateOperation (UISetName name)		= ("setName",		[JSONString name])
encodeUIUpdateOperation (UISetEnabled enabled)	= ("setDisabled",	[JSONBool (not enabled)])
encodeUIUpdateOperation (UISetActive active)	= ("setActive",		[JSONBool active])
encodeUIUpdateOperation (UISetTitle title)		= ("setTitle",		[toJSON title])
encodeUIUpdateOperation (UISetText text)		= ("setText",		[toJSON text])
encodeUIUpdateOperation (UISetIconCls cls)		= ("setIconCls",	[toJSON cls])
encodeUIUpdateOperation (UISetTooltip tip)		= ("setTooltip",	[toJSON tip])
encodeUIUpdateOperation (UISetHotkeys keys)		= ("setHotkeys",	[toJSON keys])
encodeUIUpdateOperation (UIReplace index def)	= ("replace",		[JSONInt index, encodeUIControl def])
encodeUIUpdateOperation (UISelfUpdate def)		= ("selfUpdate",	[encodeUIControl def])
encodeUIUpdateOperation (UIAdd index def)		= ("insert",		[JSONInt index, encodeUIControl def])
encodeUIUpdateOperation (UIRemove index)		= ("remove",		[JSONInt index])
encodeUIUpdateOperation (UIAddWindow index def)	= ("addWindow",		[JSONInt index, encodeUIWindow def])
encodeUIUpdateOperation (UIRemoveWindow index)	= ("removeWindow",	[JSONInt index])
encodeUIUpdateOperation (UIResize opts)			= ("resize",		[toJSON opts])
