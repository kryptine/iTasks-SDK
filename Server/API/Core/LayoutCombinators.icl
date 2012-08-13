implementation module LayoutCombinators

import StdTuple, StdList, StdBool
import Maybe, Text, Tuple, Map, Util, HtmlUtil
import SystemTypes, UIDefinition

from StdFunc import o

from Task import :: TaskCompositionType, :: TaskCompositionType(..)
derive gEq TaskCompositionType

autoLayout :: Layout
autoLayout = layout
where
	layout (DataLayout def)					= autoDataLayout def
	layout (InteractLayout prompt editor)	= autoInteractionLayout prompt editor
	layout (StepLayout def actions)			= autoStepLayout def actions
	layout (ParallelLayout prompt defs)		= autoParallelLayout prompt defs
	layout (FinalLayout def)				= autoFinalLayout def
/**
* The basic data layout groups the controls of a part of a compound datastructure in a fieldset
*/
autoDataLayout :: UIDef -> UIDef
autoDataLayout def=:{UIDef|attributes}
	# def = autoReduce (decorateControls {UIDef|def & attributes = put TYPE_ATTRIBUTE "partial" def.UIDef.attributes})
	//Attributes are discarded so merge with the reduced control
	= {UIDef|def & controls = [(c,mergeAttributes a attributes) \\ (c,a) <- def.UIDef.controls]} 
/**
* The basic interaction layout simply decorates the prompt and merges it with the editor.
*/	
autoInteractionLayout :: UIDef UIDef -> UIDef
autoInteractionLayout prompt editor
	# def = mergeDefs (decoratePrompt prompt) editor
	= {UIDef|def & attributes = put TYPE_ATTRIBUTE "partial" def.UIDef.attributes}

autoStepLayout :: UIDef [UIAction]-> UIDef
autoStepLayout def=:{UIDef|attributes,controls,actions} stepActions
	//If the definition is not already of gorup type, add actions from this step
	//We set the type attribute to "group" because the step combinator binds the controls
	//in the definition together. If the step is made, all these controls will disappear together.
	| not (isGroup def)
		= {UIDef|def & actions = actions ++ stepActions, attributes = put TYPE_ATTRIBUTE "group" def.UIDef.attributes}
	| otherwise
		= def
where
	isGroup {UIDef|attributes} = case get TYPE_ATTRIBUTE attributes of Just "group" = True; _ = False;
/**
* The default parallel composition only merges the prompt of the parallel with
* the definitions of the constituents.
*/
autoParallelLayout :: UIDef [UIDef] -> UIDef
autoParallelLayout prompt defs
	| allPartial defs		//If we are composing partial user interfaces
		= partialMerge prompt defs
	| otherwise
		= groupedMerge prompt defs
where
	allPartial [] = True
	allPartial [d:ds] = case get TYPE_ATTRIBUTE d.UIDef.attributes of Just "partial" = allPartial ds; _ = False;
/**
* Add actions and frame the content
*/
autoFinalLayout :: UIDef -> UIDef	//TODO: Size should be minWidth, but that doesn't seem to work yet...
autoFinalLayout def = appControls finalTouch (autoReduce (decorateControls def))
where
	finalTouch = setWidth (ExactSize 600) o setFramed True
	//Only set width, if it was not explicitly set yet
	setConditionalWidth size = updSizeOpts (\opts -> case opts.UISizeOpts.width of Nothing = {UISizeOpts|opts & width = Just size}; _ = opts)

//Default reduction function that reduces the list of controls to a single control by
//wrapping them in a panel if necessary.
//If possible, actions are converted to menus and buttons and added to this panel
autoReduce :: UIDef -> UIDef
autoReduce def=:{UIDef|attributes,controls,actions}
	# (buttons,actions)	= actionsToButtons actions
	# controls			= addButtons buttons controls
	# (controls,actions) = case controls of
		
		[(UIPanel _ _ _ _,_)]		= (controls,actions)	//Do nothing if the controls already consist of a single panel
		[(c=:(UIContainer _ _ _ _),a)] = case titleAttr of
				Nothing			= (controls,actions)							//If we don't have a title, just leave the container be the single control
				(Just title)	= ([(setTitle title (toPanel c),a)],actions)	//Promote container to panel and set title
		[_] | isNothing titleAttr
			= (controls,actions)
		_					
			# (menus,actions)	= actionsToMenus actions
			= (wrapControls attributes menus controls,actions)
	= {UIDef|def & controls = controls, actions = actions, attributes = attributes}
where
	addButtons [] controls		= controls
	addButtons buttons controls	= controls ++ [(buttonPanel buttons,newMap)]

	titleAttr	= get TITLE_ATTRIBUTE attributes
	
	//Create the panel
	wrapControls attributes menus controls
		= [(UIPanel sizeOpts layoutOpts (map fst controls) panelOpts,newMap)]
	where
		tbar		= case menus of [] = Nothing; _ = Just menus
		
		sizeOpts  = {defaultSizeOpts & width = Just FlexSize, minWidth = Just WrapMin, height = Just WrapSize}
		layoutOpts = {defaultLayoutOpts & padding = Just {top = 5, right = 5, bottom = 5, left = 5}}
		panelOpts = {UIPanelOpts|title = titleAttr,frame = False, tbar 
					,iconCls = iconClsAttr, baseCls = Nothing, bodyCls = Nothing}
		
		iconClsAttr	= fmap (\icon -> "icon-" +++ icon) (get ICON_ATTRIBUTE attributes)

fillReduce :: UIDef -> UIDef
fillReduce def = autoReduce (fillOutControls def)
	
//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: UIDef -> UIDef
decorateControls def=:{UIDef|controls} = {UIDef|def & controls = mapLst decorateControl controls}
where
	mapLst f [] = []
	mapLst f [x] = [f True x]
	mapLst f [x:xs] = [f False x: mapLst f xs]
	
decorateControl :: Bool (!UIControl,!UIAttributes) -> (!UIControl,!UIAttributes)
decorateControl last (control,attributes)
	# mbLabel 	= get LABEL_ATTRIBUTE attributes
	# mbHint 	= get HINT_ATTRIBUTE attributes
	# mbValid	= get VALID_ATTRIBUTE attributes
	# mbError	= get ERROR_ATTRIBUTE attributes
	= case (mbLabel,mbHint,mbValid,mbError) of
		(Nothing,Nothing,Nothing,Nothing)	//Nothing to do
			# control = if last control (setBottomMargin 5 control)
			= (control,attributes)
		_									//Add decoration													
			# control = row (labelCtrl mbLabel ++ [control] ++ iconCtrl mbHint mbValid mbError) 
			# control = if last control (setBottomMargin 5 control)
			# attributes = foldr del attributes [LABEL_ATTRIBUTE,HINT_ATTRIBUTE,VALID_ATTRIBUTE,ERROR_ATTRIBUTE]
			= (control,attributes)
where
	row ctrls				= (setSize FlexSize WrapSize o setDirection Horizontal) (defaultContainer ctrls)
	
	labelCtrl (Just label)	= [setWidth (ExactSize 100) (stringDisplay label)]
	labelCtrl Nothing		= []
	
	iconCtrl (Just msg) _ _	= icon "icon-hint" msg
	iconCtrl _ (Just msg) _	= icon "icon-valid" msg
	iconCtrl _ _ (Just msg)	= icon "icon-invalid" msg
	iconCtrl _ _ _			= []
	
	icon cls tooltip		= [setLeftMargin 5 (UIIcon defaultSizeOpts {UIIconOpts|iconCls = cls, tooltip = Just tooltip})]

//TODO: try to make as many components flex fitting as possible to fill up a container
fillOutControls	:: UIDef -> UIDef
fillOutControls def = def

//Wrap the controls of the prompt in a container with a nice css class and add some bottom margin
decoratePrompt :: UIDef -> UIDef
decoratePrompt def=:{UIDef|controls=[]} = def //If there are no controls in the prompt def, don't create a container
decoratePrompt def=:{UIDef|controls}
	= {UIDef|def & controls = [(container,newMap)]}
where
	container = UIContainer sizeOpts defaultLayoutOpts (map fst controls) containerOpts
	sizeOpts = {defaultSizeOpts & margins = Just {top= 5, right = 0, bottom = 10, left = 0}, width = Just FlexSize, minWidth = Just WrapMin}
	containerOpts = {UIContainerOpts|baseCls=Just "itwc-prompt", bodyCls=Nothing}

//Merge the fragments of a composed interactive task into a single definition
partialMerge :: UIDef [UIDef] -> UIDef
partialMerge prompt defs
	# attributes =  put TYPE_ATTRIBUTE "partial" prompt.UIDef.attributes
	# controls = foldr (++) (decoratePrompt prompt).UIDef.controls [d.UIDef.controls \\ d <- defs]
	# actions = foldr (++) [] [d.UIDef.actions \\ d <- defs]
	//Determine title: if prompt has title use it, else combine titles 
	# attributes = case (get TITLE_ATTRIBUTE attributes, collectTitles defs) of
		(Just _,_) 	= attributes //Title already set, do nothing
		(_,[])		= attributes //None of the parts have a title, do nothing
		(_,titles)	= put TITLE_ATTRIBUTE (join ", " titles) attributes	//Set the joined titles
	= {UIDef|attributes = attributes, controls = controls, actions = actions}
where
	collectTitles defs = [title \\ Just title <- [get TITLE_ATTRIBUTE d.UIDef.attributes \\ d <- defs]]


//Minimal merge, ignore the prompt, only reduce all parts
minimalMerge :: ParallelMerger
minimalMerge = merge
where
	merge _ defs
		# attributes	= put TYPE_ATTRIBUTE "multi" newMap
		# controls		= foldr (++) [] [(autoReduce d).UIDef.controls \\ d <- defs]
		# actions		= foldr (++) [] [d.UIDef.actions \\ d <- defs]
		= {UIDef|attributes = attributes, controls = controls, actions = actions}
	
//Create groups for all definitions in the list
groupedMerge :: ParallelMerger
groupedMerge = merge
where
	merge prompt defs
		# groups		= map (autoReduce o decorateControls) defs
		# attributes	= put TYPE_ATTRIBUTE "multi" prompt.UIDef.attributes
		# controls		= foldr (++) (decoratePrompt prompt).UIDef.controls [d.UIDef.controls \\ d <- groups]
		# actions		= foldr (++) [][d.UIDef.actions \\ d <- groups]
		= {UIDef|attributes = attributes, controls = controls, actions = actions}

sideMerge :: UISide Int ParallelMerger -> ParallelMerger
sideMerge side size restMerge = merge
where
	merge prompt []		= prompt
	merge prompt parts
		# (direction,sidePart,restParts) = case side of
			TopSide		= (Vertical, hd parts,tl parts)
			RightSide	= (Horizontal, last parts,init parts)
			BottomSide	= (Vertical, last parts,init parts)
			LeftSide	= (Horizontal, hd parts, tl parts)
		# restPart		= fillReduce (restMerge prompt restParts)
		# sidePart		= fillReduce sidePart
		# sideUI		= (ifH direction (setWidth (ExactSize size)) (setHeight (ExactSize size))) (fill (uiOf sidePart))
		# restUI		= fill (uiOf restPart)
		# ui			= (fill o setDirection direction) (defaultContainer (ifTL side [sideUI,restUI] [restUI,sideUI]))
		= {UIDef|controls = [(ui,newMap)],actions = sidePart.UIDef.actions ++ restPart.UIDef.actions, attributes = prompt.UIDef.attributes}

	ifTL TopSide a b = a
	ifTL LeftSide a b = a
	ifTL _ a b = b
	
	ifH Horizontal a b = a
	ifH _ a b = b
	
splitMerge :: UIDirection -> ParallelMerger
splitMerge dir = merge
where
	merge prompt parts = prompt
	/*
		# (parts1,parts2)		= splitFun parts
		# side1 = merger1 prompt parts1
		# side2 = merger2 prompt parts2
		# ui1 = defaultContainer (map fst side1.UIDef.controls)
		# ui2 = defaultContainer (map fst side2.UIDef.controls)
		# (uis,dir) = case side of
			TopSide		= ([(fillWidth o fixedHeight size) ui1,fill ui2],Vertical)
			RightSide	= ([fill ui2,(fixedWidth size o fillHeight) ui1],Horizontal)
			BottomSide	= ([fill ui2,(fillWidth o fixedHeight size) ui1],Vertical)
			LeftSide	= ([(fixedWidth size o fillHeight) ui1,fill ui2],Horizontal)
		# ui = (fill o setDirection dir) (defaultContainer uis)
		= {UIDef|attributes = prompt.UIDef.attributes, controls = [(ui,newMap)], actions = side1.UIDef.actions ++ side2.UIDef.actions} 
	*/
tabbedMerge :: ParallelMerger
tabbedMerge = merge
where
	merge prompt defs
		# attributes				= prompt.UIDef.attributes
		# (activeIndex,activeDef)	= findActive defs	
		# (tabBar,actions)			= mkTabs activeIndex defs	
		# tabContent				=  maybe [(defaultPanel [],newMap)] (\d -> (tweakUI (setPadding 0 0 0 0) (autoReduce (tweakAttr (del TITLE_ATTRIBUTE) d))).UIDef.controls) activeDef
		# controls					= (decoratePrompt prompt).UIDef.controls ++ [(tabBar,newMap)] ++ tabContent
		= {UIDef|attributes = attributes,controls = controls, actions = actions}

	findActive defs = find 0 (0,Nothing) defs
	where
		find i bestSoFar [] = bestSoFar
		find i (_,Nothing) [d:ds]	= find (i+1) (i,Just d) ds
		find i bestSoFar=:(_,Just best) [d:ds]	= if (later d best) (find (i+1) (i,Just d) ds) (find (i+1) bestSoFar ds)
		
		later a b = case (get TIME_ATTRIBUTE a.UIDef.attributes,get TIME_ATTRIBUTE b.UIDef.attributes) of
			(Just ta,Just tb)	= ta > tb
			(Just _,Nothing)	= True
			_					= False
		
	mkTabs active defs
		# (tabs,actions) = unzip [mkTab (i == active) d \\ d <- defs & i <- [0..]]
		= (setDirection Horizontal (defaultContainer tabs),foldr (++) [] actions)

	mkTab active def=:{UIDef|attributes,actions}
		# taskId				= get TASK_ATTRIBUTE attributes
		# iconCls				= fmap (\i -> "icon-" +++ i) (get ICON_ATTRIBUTE attributes)
		# text					= fromMaybe "Untitled" (get TITLE_ATTRIBUTE attributes)
		# (closable,actions)	= searchCloseAction taskId actions
		# tabOpts = {text = text ,taskId = taskId,active = active,closable = closable,iconCls=iconCls}
		= (UITab defaultSizeOpts tabOpts, if active actions [])
	
	searchCloseAction match [] = (False,[])
	searchCloseAction (Just match) [{taskId,action=ActionClose,enabled}:as] = (taskId == match && enabled,as)
	searchCloseAction match [a:as] = let (mbtask,as`) = searchCloseAction match as in (mbtask,[a:as`])

hideLayout :: Layout
hideLayout = layout
where
	layout (DataLayout def)					= {UIDef|def & controls = []}
	layout (InteractLayout prompt editor)	= {UIDef|mergeDefs prompt editor & controls = []}
	layout (StepLayout def actions)			= {UIDef|def & controls = [], actions = def.UIDef.actions ++ actions}
	layout (ParallelLayout prompt defs)		= {UIDef|foldr mergeDefs {UIDef|controls=[],actions=[],attributes = newMap} [prompt:defs] & controls = []}
	layout (FinalLayout def)				= {UIDef|def & controls = []}
	
splitLayout :: UIDirection -> Layout
splitLayout dir = layout
where
	layout (ParallelLayout prompt parts)	= splitMerge dir prompt parts
	layout layoutable						= autoLayout layoutable
	
sideLayout :: UISide Int ParallelMerger -> Layout
sideLayout side size restMerge = layout
where
	layout (ParallelLayout prompt parts)	= sideMerge side size restMerge prompt parts
	layout layoutable						= autoLayout layoutable
	
tabbedLayout :: Layout
tabbedLayout = layout
where
	layout (ParallelLayout prompt parts)	= tabbedMerge prompt parts
	layout layoutable						= autoLayout layoutable
	

//Determine the index of the visible part with the highest stack-order attribute
getTopIndex :: [UIDef] -> Int 
getTopIndex parts = find 0 0 0 parts
where
	find maxTop maxIndex i [] = maxIndex
	find maxTop maxIndex i [{UIDef|controls=[]}:ps] = find maxTop maxIndex i ps //Ignore invisible parts 
	find maxTop maxIndex i [{UIDef|attributes}:ps] = case get TIME_ATTRIBUTE attributes of
		Nothing			= find maxTop maxIndex (i + 1) ps
		Just time	
			# time = toInt time
			| time > maxTop	= find time i (i + 1) ps
							= find maxTop maxIndex (i + 1) ps

partLayout :: Int -> Layout
partLayout idx = layout
where
	layout (ParallelLayout prompt parts)	= parts !! idx
	layout layoutable						= autoLayout layoutable

updSizeOpts :: (UISizeOpts -> UISizeOpts) UIControl -> UIControl
updSizeOpts f (UIViewString	sOpts vOpts)			= (UIViewString	(f sOpts) vOpts)
updSizeOpts f (UIViewHtml sOpts vOpts)				= (UIViewHtml (f sOpts) vOpts)
updSizeOpts f (UIViewDocument sOpts vOpts)			= (UIViewDocument (f sOpts) vOpts)
updSizeOpts f (UIViewCheckbox sOpts vOpts)			= (UIViewCheckbox (f sOpts) vOpts)
updSizeOpts f (UIViewSlider sOpts vOpts opts)		= (UIViewSlider (f sOpts) vOpts opts)
updSizeOpts f (UIViewProgress sOpts vOpts opts)		= (UIViewProgress (f sOpts) vOpts opts)
updSizeOpts f (UIEditString	sOpts eOpts)			= (UIEditString	(f sOpts) eOpts)
updSizeOpts f (UIEditNote sOpts eOpts)				= (UIEditNote (f sOpts) eOpts)
updSizeOpts f (UIEditPassword sOpts eOpts)			= (UIEditPassword (f sOpts) eOpts)
updSizeOpts f (UIEditInt sOpts eOpts)				= (UIEditInt (f sOpts) eOpts)
updSizeOpts f (UIEditDecimal sOpts eOpts)			= (UIEditDecimal (f sOpts) eOpts)
updSizeOpts f (UIEditCheckbox sOpts eOpts)			= (UIEditCheckbox (f sOpts) eOpts)
updSizeOpts f (UIEditSlider sOpts eOpts opts)		= (UIEditSlider (f sOpts) eOpts opts)
updSizeOpts f (UIEditDate sOpts eOpts)				= (UIEditDate (f sOpts) eOpts)
updSizeOpts f (UIEditTime sOpts eOpts)				= (UIEditTime (f sOpts) eOpts)
updSizeOpts f (UIEditDocument sOpts eOpts)			= (UIEditDocument (f sOpts) eOpts)
updSizeOpts f (UIEditButton	sOpts eOpts opts)		= (UIEditButton	(f sOpts) eOpts opts)
updSizeOpts f (UIDropdown sOpts cOpts)				= (UIDropdown (f sOpts) cOpts)
updSizeOpts f (UIGrid sOpts cOpts opts)				= (UIGrid (f sOpts) cOpts opts)
updSizeOpts f (UITree sOpts cOpts)					= (UITree (f sOpts) cOpts)
updSizeOpts f (UIActionButton sOpts aOpts opts)		= (UIActionButton (f sOpts) aOpts opts)	
updSizeOpts f (UIMenuButton	sOpts opts)				= (UIMenuButton	(f sOpts) opts)	
updSizeOpts f (UILabel sOpts opts)					= (UILabel (f sOpts) opts)
updSizeOpts f (UIIcon sOpts opts)					= (UIIcon (f sOpts) opts)
updSizeOpts f (UITab sOpts opts)					= (UITab (f sOpts) opts)
updSizeOpts f (UITasklet sOpts opts)				= (UITasklet (f sOpts) opts)
updSizeOpts f (UIContainer sOpts lOpts items opts)	= (UIContainer (f sOpts) lOpts items opts)
updSizeOpts f (UIPanel sOpts lOpts items opts)		= (UIPanel (f sOpts) lOpts items opts)
updSizeOpts f (UIFieldSet sOpts lOpts items opts)	= (UIFieldSet (f sOpts) lOpts items opts)
updSizeOpts f (UIWindow	sOpts lOpts items opts)		= (UIWindow	(f sOpts) lOpts items opts)

setSize :: !UISize !UISize !UIControl -> UIControl
setSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width, height = Just height}) ctrl

setWidth :: !UISize !UIControl -> UIControl
setWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & width = Just width}) ctrl

setHeight :: !UISize !UIControl -> UIControl
setHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & height = Just height}) ctrl

setMinSize :: !UIMinSize !UIMinSize !UIControl -> UIControl
setMinSize width height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width, minHeight = Just height}) ctrl

setMinWidth :: !UIMinSize !UIControl -> UIControl
setMinWidth width ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minWidth = Just width}) ctrl

setMinHeight :: !UIMinSize !UIControl -> UIControl
setMinHeight height ctrl = updSizeOpts (\opts -> {UISizeOpts| opts & minHeight = Just height}) ctrl

fill :: !UIControl -> UIControl
fill ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize, height = Just FlexSize}) ctrl

fillHeight :: !UIControl -> UIControl
fillHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just FlexSize}) ctrl

fillWidth :: !UIControl -> UIControl
fillWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just FlexSize}) ctrl

fixedHeight	:: !Int !UIControl -> UIControl
fixedHeight size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just (ExactSize size)}) ctrl

fixedWidth :: !Int !UIControl -> UIControl
fixedWidth size ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just (ExactSize size)}) ctrl

wrapHeight :: !UIControl -> UIControl
wrapHeight ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & height = Just WrapSize}) ctrl
 
wrapWidth :: !UIControl -> UIControl
wrapWidth ctrl = updSizeOpts (\opts -> {UISizeOpts|opts & width = Just WrapSize}) ctrl

setMargins :: !Int !Int !Int !Int !UIControl -> UIControl
setMargins top right bottom left ctrl
	= updSizeOpts (\opts -> {UISizeOpts|opts & margins = Just {top = top, right = right, bottom = bottom, left = left}}) ctrl

setTopMargin :: !Int !UIControl -> UIControl
setTopMargin top ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = top, right = 0, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & top = top}}

setRightMargin	:: !Int !UIControl -> UIControl
setRightMargin right ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = right, bottom = 0, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & right = right}}
	
setBottomMargin	:: !Int !UIControl -> UIControl
setBottomMargin bottom ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = bottom, left = 0}}
		Just m	= {UISizeOpts|opts & margins = Just {m & bottom = bottom}}
	
setLeftMargin :: !Int !UIControl -> UIControl
setLeftMargin left ctrl = updSizeOpts f ctrl
where
	f opts = case opts.margins of
		Nothing = {UISizeOpts|opts & margins = Just {top = 0, right = 0, bottom = 0, left = left}}
		Just m	= {UISizeOpts|opts & margins = Just {m & left = left}}

setPadding :: !Int !Int !Int !Int !UIControl -> UIControl
setPadding top right bottom left (UIContainer sOpts lOpts items opts)
	= UIContainer sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIPanel sOpts lOpts items opts)
	= UIPanel sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left (UIWindow sOpts lOpts items opts)
	= UIWindow sOpts {UILayoutOpts|lOpts & padding = Just {top=top,right=right,bottom=bottom,left=left}} items opts
setPadding top right bottom left ctrl = ctrl

setTitle :: !String !UIControl -> UIControl
setTitle title (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & title = Just title}
setTitle title (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & title = Just title}
setTitle title (UIFieldSet sOpts lOpts items opts) = UIFieldSet sOpts lOpts items {UIFieldSetOpts|opts & title = title}
setTitle title ctrl = ctrl

setFramed :: !Bool !UIControl -> UIControl
setFramed frame (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & frame = frame}
setFramed frame (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & frame = frame}
setFramed frame ctrl = ctrl

setIconCls :: !String !UIControl -> UIControl
setIconCls iconCls (UIActionButton sOpts aOpts opts) = UIActionButton sOpts aOpts {UIButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIMenuButton sOpts opts) = UIMenuButton sOpts {UIMenuButtonOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIIcon sOpts opts) = UIIcon sOpts  {UIIconOpts|opts & iconCls = iconCls}
setIconCls iconCls (UITab sOpts opts) = UITab sOpts  {UITabOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & iconCls = Just iconCls}
setIconCls iconCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just iconCls}
setIconCls iconCls ctrl = ctrl

setBaseCls :: !String !UIControl -> UIControl
setBaseCls baseCls (UIContainer sOpts lOpts items opts) = UIContainer sOpts lOpts items {UIContainerOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIPanel sOpts lOpts items opts) = UIPanel sOpts lOpts items {UIPanelOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls (UIWindow sOpts lOpts items opts) = UIWindow sOpts lOpts items {UIWindowOpts|opts & baseCls = Just baseCls}
setBaseCls baseCls ctrl = ctrl

setDirection :: !UIDirection !UIControl -> UIControl
setDirection dir (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & direction = dir} items opts
setDirection dir (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & direction = dir} items opts
setDirection dir (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & direction = dir} items opts
setDirection dir ctrl									= ctrl

setHalign :: !UIHAlign !UIControl -> UIControl
setHalign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & halign = align} items opts
setHalign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & halign = align} items opts
setHalign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & halign = align} items opts
setHalign align ctrl									= ctrl

setValign :: !UIVAlign !UIControl -> UIControl
setValign align (UIContainer sOpts lOpts items opts)	= UIContainer sOpts {lOpts & valign = align} items opts
setValign align (UIPanel sOpts lOpts items opts)		= UIPanel sOpts {lOpts & valign = align} items opts
setValign align (UIWindow sOpts lOpts items opts)		= UIWindow sOpts {lOpts & valign = align} items opts
setValign align ctrl									= ctrl

//Container coercion
toPanel	:: !UIControl -> UIControl
//Panels are left untouched
toPanel ctrl=:(UIPanel _ _ _ _)		= ctrl
//Containers are coerced to panels
toPanel ctrl=:(UIContainer sOpts lOpts items {UIContainerOpts|baseCls,bodyCls})
	= UIPanel sOpts lOpts items {UIPanelOpts|title=Nothing,frame=False,tbar=Nothing,iconCls=Nothing,baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a panel instead
toPanel ctrl = defaultPanel [ctrl]

toContainer :: !UIControl -> UIControl
//Containers are left untouched
toContainer ctrl=:(UIContainer _ _ _ _) = ctrl
//Panels can be coerced to containers
toContainer ctrl=:(UIPanel sOpts lOpts items {UIPanelOpts|baseCls,bodyCls})
	= UIContainer sOpts lOpts items {UIContainerOpts|baseCls=baseCls,bodyCls=bodyCls}
//Uncoercable items are wrapped in a container instead
toContainer ctrl = defaultContainer [ctrl]
	
//GUI combinators						
hjoin :: ![UIControl] -> UIControl
hjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Horizontal, halign = AlignLeft, valign = AlignMiddle} items {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}

vjoin :: ![UIControl] -> UIControl
vjoin items = UIContainer defaultSizeOpts {defaultLayoutOpts & direction = Vertical, halign = AlignLeft, valign = AlignTop} items {UIContainerOpts|baseCls=Nothing,bodyCls=Nothing}
						
//Container operations
addItemToUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToUI mbIndex item ctrl = case ctrl of
	UIContainer sOpts lOpts items opts	= UIContainer sOpts lOpts (add mbIndex item items) opts
	UIPanel sOpts lOpts items opts		= UIPanel sOpts lOpts (add mbIndex item items) opts
	UIWindow sOpts lOpts items opts		= UIWindow sOpts lOpts (add mbIndex item items) opts
	_									= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
	

getItemsOfUI :: UIControl -> [UIControl]
getItemsOfUI (UIContainer _ _ items _)	= items
getItemsOfUI (UIPanel _ _ items _)		= items
getItemsOfUI (UIWindow _ _ items _)	= items
getItemsOfUI ctrl						= [ctrl]
	
setItemsOfUI :: [UIControl] UIControl -> UIControl
setItemsOfUI items (UIContainer sOpts lOpts _ opts)	= UIContainer sOpts lOpts items opts
setItemsOfUI items (UIPanel sOpts lOpts _ opts)		= UIPanel sOpts lOpts items opts
setItemsOfUI items (UIWindow sOpts lOpts _ opts)		= UIWindow sOpts lOpts items opts
setItemsOfUI items ctrl								= ctrl

//Container for a set of horizontally layed out buttons
buttonPanel	:: ![UIControl] -> UIControl	
buttonPanel buttons
	= (wrapHeight o fillWidth o setPadding 2 0 2 0 o setDirection Horizontal o setHalign AlignRight) (defaultContainer buttons)

actionsToButtons :: ![UIAction] -> (![UIControl],![UIAction])
actionsToButtons [] = ([],[])
actionsToButtons [a=:{taskId,action,enabled}:as]
	# (buttons,actions)	= actionsToButtons as 
	= case split "/" (actionName action) of
		//Action name consist of only one part -> make a button
		[name]	= ([mkButton taskId action enabled : buttons],actions)
		//Action name consists of multiple parts -> pass through
		_		= (buttons,[a:actions])
where
	mkButton taskId action enabled
		= UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId= actionName action}
			{UIButtonOpts|text = actionName action, iconCls = Just (actionIcon action), disabled = not enabled}
			
actionsToMenus :: ![UIAction] -> (![UIControl],![UIAction])
actionsToMenus actions = makeMenus [] actions
where
	makeMenus :: [UIControl] [UIAction] -> ([UIControl],[UIAction])
	makeMenus menus []	= (menus,[])	
	makeMenus menus [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (split "/" (actionName action)) taskId action enabled menus) as
	
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m=:(UIMenuButton sOpts opts):ms] //Add to existing menu if it exists
		| opts.UIMenuButtonOpts.text == main //Found!
			= [UIMenuButton sOpts {UIMenuButtonOpts|opts & menu = addToItems item taskId action enabled opts.UIMenuButtonOpts.menu}:ms]
		| otherwise
			= [m:addToMenus [main:item] taskId action enabled ms]
			
	addToItems [item:sub] taskId action enabled [] //Create item
		= [createItem item sub taskId action enabled]
	addToItems [item:sub] taskId action enabled [i:is]
		| itemText i == item
			| isEmpty sub	//Duplicate item (just add it)
				= [i,createItem item sub taskId action enabled:is]
			| otherwise		//Add to the found item
				= [addToItem sub taskId action enabled i:is]
		| otherwise
			= [i:addToItems [item:sub] taskId action enabled is]
	addToItems [] _ _ _ _
		= []

	itemText (UIActionMenuItem _ {UIButtonOpts|text})	= text
	itemText (UISubMenuItem {UIMenuButtonOpts|text})		= text
	itemText _					= ""
	
	createButton item sub taskId action enabled
		= UIMenuButton defaultSizeOpts
			{UIMenuButtonOpts
			|text = item
			,iconCls = Just (icon item)
			,disabled	= if (isEmpty sub) (not enabled) False
			,menu = addToItems sub taskId action enabled []
			}
	createItem item [] taskId action enabled //Action item
		= UIActionMenuItem
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=item,iconCls = Just (icon item), disabled = not enabled}
	createItem item sub taskId action enabled //Sub item
		= UISubMenuItem
				{ text = item
				, iconCls = Just (icon item)
				, disabled = False
				, menu = addToItems sub taskId action enabled []
				}
		
	addToItem sub taskId action enabled item=:(UISubMenuItem opts=:{UIMenuButtonOpts|menu})
		= UISubMenuItem {UIMenuButtonOpts|opts & menu = addToItems sub taskId action enabled menu}
	
	icon name = "icon-" +++ (replaceSubString " " "-" (toLowerCase name))

uiOf :: UIDef -> UIControl
uiOf {UIDef|controls} = case controls of
	[]			= stringDisplay "-"
	[(c,_):_]	= c

actionsOf :: UIDef -> [UIAction]
actionsOf {UIDef|actions} = actions

attributesOf :: UIDef -> UIAttributes
attributesOf {UIDef|attributes} = attributes

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2 = foldr (\(k,v) attr -> put k v attr) attr1 (toList attr2)

mergeDefs :: UIDef UIDef -> UIDef
mergeDefs {UIDef|controls=ct1,actions=ac1,attributes=at1} {UIDef|controls=ct2,actions=ac2,attributes=at2}
	= {UIDef|controls = ct1 ++ ct2, actions = ac1 ++ ac2, attributes = mergeAttributes at1 at2}

appControls :: (UIControl -> UIControl) UIDef -> UIDef
appControls f def=:{UIDef|controls} = {UIDef|def & controls = [(f c,a) \\ (c,a) <- controls]}

appDeep	:: [Int] (UIControl -> UIControl) UIControl -> UIControl
appDeep [] f ctrl = f ctrl
appDeep [s:ss] f ctrl = case ctrl of
	(UIContainer sOpts lOpts items cOpts) 	= UIContainer sOpts lOpts (update items) cOpts
	(UIPanel sOpts lOpts items pOpts)		= UIPanel sOpts lOpts (update items) pOpts
	(UIWindow sOpts lOpts items wOpts)		= UIWindow sOpts lOpts (update items) wOpts
	_										= ctrl
where
	update items = [if (i == s) (appDeep ss f item) item \\ item <- items & i <- [0..]]

tweakUI :: (UIControl -> UIControl) UIDef -> UIDef
tweakUI f def=:{UIDef|controls} = {UIDef|def & controls = [(f c,a) \\ (c,a) <- controls]}

tweakAttr :: (UIAttributes -> UIAttributes) UIDef -> UIDef
tweakAttr f def=:{UIDef|attributes} = {UIDef|def & attributes = f attributes}
