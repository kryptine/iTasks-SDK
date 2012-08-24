implementation module UIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
import Text, Util, UIDefinition
from Task import :: Event(..)

:: DiffPath :== [DiffStep] 
:: DiffStep	= ItemStep !Int | MenuStep

instance toString DiffPath
where
	toString path = join "/" (map step (reverse path))
	where
		step (ItemStep i) = toString i
		step (MenuStep) = "m"

diffUIDefinitions :: ![UIControl] ![UIControl] !Event -> [UIUpdate]
diffUIDefinitions old new event = [] //diffEditorDefinitions` [ItemStep 0] event old new

diffEditorDefinitions` :: !DiffPath !Event !UIControl !UIControl -> [UIUpdate]
//diffEditorDefinitions` path event (UIViewString osize oview) (UIViewString nsize nview) = []
//Fallback case, simply replace old by new
diffEditorDefinitions` [ItemStep parentIndex:parentPath] event old new = [UIReplace (toString parentPath) parentIndex new]

//Compare controls
diffControls :: !DiffPath !Event !UIControl !UIControl -> [UIUpdate]
diffControls path event (UIViewString sOpts1 vOpts1)				(UIViewString sOpts2 vOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIViewHtml sOpts1 vOpts1)					(UIViewHtml sOpts2 vOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIViewDocument sOpts1 vOpts1)				(UIViewDocument sOpts2 vOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIViewCheckbox sOpts1 vOpts1)				(UIViewCheckbox sOpts2 vOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIViewSlider sOpts1 vOpts1 opts1)			(UIViewSlider sOpts2 vOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIViewProgress sOpts1 vOpts1 opts1)		(UIViewProgress sOpts2 vOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditString sOpts1 eOpts1)				(UIEditString sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditNote sOpts1 eOpts1)					(UIEditNote sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditPassword sOpts1 eOpts1)				(UIEditPassword sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditInt sOpts1 eOpts1)					(UIEditInt sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditDecimal sOpts1 eOpts1)				(UIEditDecimal sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditCheckbox sOpts1 eOpts1)				(UIEditCheckbox sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditSlider sOpts1 eOpts1 opts1)			(UIEditSlider sOpts2 eOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditDate sOpts1 eOpts1)					(UIEditDate sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditTime sOpts1 eOpts1)					(UIEditTime sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditDocument sOpts1 eOpts1)				(UIEditDocument sOpts2 eOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditButton sOpts1 eOpts1 opts1)			(UIEditButton sOpts2 eOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIEditGoogleMap sOpts1 eOpts1 opts1)		(UIEditGoogleMap sOpts2 eOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIDropdown sOpts1 cOpts1)					(UIDropdown sOpts2 cOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIGrid sOpts1 cOpts1 opts1)				(UIGrid sOpts2 cOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UITree sOpts1 cOpts1)						(UITree sOpts2 cOpts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIActionButton sOpts1 aOpts1 opts1)		(UIActionButton sOpts2 aOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIMenuButton sOpts1 opts1)					(UIMenuButton sOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UILabel sOpts1 opts1)						(UILabel sOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIIcon sOpts1 opts1)						(UIIcon sOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UITab sOpts1 opts1)						(UITab sOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UITasklet sOpts1 opts1)					(UITasklet sOpts2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIContainer sOpts1 lOpts1 items1 opts1)	(UIContainer sOpts2 lOpts2 items2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIPanel sOpts1 lOpts1 items1 opts1)		(UIPanel sOpts2 lOpts2 items2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIFieldSet sOpts1 lOpts1 items1 opts1)		(UIFieldSet sOpts2 lOpts2 items2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UIWindow sOpts1 lOpts1 items1 opts1)		(UIWindow sOpts2 lOpts2 items2 opts2)
	= diffSizeOpts path sOpts1 sOpts2
diffControls path event (UICustom opts1)							(UICustom opts2)							= []

diffSizeOpts :: DiffPath UISizeOpts UISizeOpts -> [UIUpdate]
diffSizeOpts path opts1 opts2 = []

diffViewOpts :: DiffPath (UIViewOpts a) (UIViewOpts a) -> [UIUpdate] | gEq{|*|} a & JSONEncode{|*|} a
diffViewOpts path opts1 opts2 = []

diffEditOpts :: DiffPath (UIEditOpts a) (UIEditOpts a) -> [UIUpdate] | gEq{|*|} a & JSONEncode{|*|} a
diffEditOpts path opts1 opts2 = []

diffChoiceOpts :: DiffPath (UIChoiceOpts a) (UIChoiceOpts a) -> [UIUpdate] | gEq{|*|} a & JSONEncode{|*|} a
diffChoiceOpts path opts1 opts2 = []

/*
where
	diffEditorDefinitions`` :: !(Maybe EditEvent) !UIControlContent !UIControlContent -> Maybe [UIUpdate]
	diffEditorDefinitions`` event old new = case (old,new) of
		// Documents are replaced if their value has changed
		(UIEditControl (UIDocumentControl odoc) oc, UIEditControl (UIDocumentControl ndoc) nc)
			| odoc == ndoc && oc.UIEditControl.taskId == nc.UIEditControl.taskId && oc.UIEditControl.name == nc.UIEditControl.name
				= Just []
			| otherwise
				= Nothing
		(UIEditControl (UIGridControl ogrid) _, UIEditControl (UIGridControl ngrid) _)
			| ogrid =!= ngrid	= Nothing
		(UIEditControl otype oc, UIEditControl ntype nc)
			| otype === ntype
				= Just (valueUpdate path event oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(UIShowControl otype oc, UIShowControl ntype nc)
			| otype === ntype && oc.UIShowControl.value === nc.UIShowControl.value
				= Just []
			| otherwise
				= Nothing
		(UIButton o,UIButton n)
			| o.UIButton.text == n.UIButton.text && o.UIButton.iconCls == n.UIButton.iconCls
				= Just (update (\o n -> o.UIButton.disabled == n.UIButton.disabled) (\b -> Just (not b.UIButton.disabled)) UISetEnabled path o n
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(UIContainer o, UIContainer n)
			|  (o.UIContainer.direction === n.UIContainer.direction
				&& o.UIContainer.halign === n.UIContainer.halign
				&& o.UIContainer.valign === n.UIContainer.valign)
				= Just (diffChildEditorDefinitions path event o.UIContainer.items n.UIContainer.items)
		(UIPanel o, UIPanel n)
			| ( o.UIPanel.direction === n.UIPanel.direction
				&& o.UIPanel.halign === n.UIPanel.halign
				&& o.UIPanel.valign === n.UIPanel.valign
				&& o.UIPanel.frame === n.UIPanel.frame
				&& o.UIPanel.menus === n.UIPanel.menus
				&& (isJust o.UIPanel.iconCls == isJust n.UIPanel.iconCls))
					# titleUpdate	= update (\o n -> o.UIPanel.title === n.UIPanel.title && o.UIPanel.iconCls == n.UIPanel.iconCls) (\{UIPanel|title,iconCls} -> Just (fromMaybe "" title,iconCls)) UISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.UIPanel.items n.UIPanel.items
					# menuUpdates	= []
					//# menuUpdates	= diffUIMenus path o.UIPanel.menus n.UIPanel.menus
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
		(UIWindow o, UIWindow n)
			|  (o.UIWindow.direction === n.UIWindow.direction
				&& o.UIWindow.halign === n.UIWindow.halign
				&& o.UIWindow.valign === n.UIWindow.valign)
				= Just (diffChildEditorDefinitions path event o.UIWindow.items n.UIWindow.items)
		(UIListContainer lcOld, UIListContainer lcNew)	
			= Just (diffChildEditorDefinitions path event (items lcOld) (items lcNew)
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
			where
				items lc = [{content = UIListItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- lc.UIListContainer.items]
		(UIListItem liOld, UIListItem liNew)
			= Just (diffChildEditorDefinitions path event [liOld.UIListItem.items] [liNew.UIListItem.items])
		(UITabContainer tcOld, UITabContainer tcNew)
			# activeTabUpdate	= update (\o n -> o.UITabContainer.active == n.UITabContainer.active) (\{UITabContainer|active} -> Just active) UISetActiveTab path tcOld tcNew
			# itemUpdates 		= diffChildEditorDefinitions path event (items tcOld) (items tcNew)
			= Just (itemUpdates ++ activeTabUpdate)
			where
				items tc = [{content = UITabItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- tc.UITabContainer.items]
		(UITabItem o, UITabItem n)
			| (o.UITabItem.closeAction === n.UITabItem.closeAction //Can't diff the close action for now
				&& o.UITabItem.menus === n.UITabItem.menus)		//Diff of menus is also still impossible
					# titleUpdate	= update (\o n -> o.UITabItem.title == n.UITabItem.title && o.UITabItem.iconCls == n.UITabItem.iconCls) (\{UITabItem|title,iconCls} -> Just (title,iconCls)) UISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.UITabItem.items n.UITabItem.items
					# menuUpdates 	= []
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
			| otherwise
				= Nothing
		(UIIcon o, UIIcon n)
			| o.UIIcon.type == n.UIIcon.type
				&& o.UIIcon.tooltip === n.UIIcon.tooltip
					= Just []	
		// Custom components need to figure out their own update on the client side
		(UICustom oc, UICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [UIUpdate (toString path) newTui]
		// Fallback: always replace
		_	= Nothing
	
	//Determine the updates for child items in containers, lists etc	
	diffChildEditorDefinitions :: DiffPath (Maybe EditEvent) [UIControl] [UIControl] -> [UIUpdate]
	diffChildEditorDefinitions path event old new = diffChildEditorDefinitions` path event 0 old new
	where
		diffChildEditorDefinitions` path event i [] []
			= []
		diffChildEditorDefinitions` path event i old []
			//Less items in new than old (remove starting with the last item)
			= [UIRemove (toString path) n \\ n <- reverse [i.. i + length old - 1 ]] 
		diffChildEditorDefinitions` path event i [] new
			//More items in new than old
			= [UIAdd (toString path) n def \\ n <- [i..] & def <- new] 
		diffChildEditorDefinitions` path event i [o:os] [n:ns] 
			=	(diffEditorDefinitions` [ItemStep i:path] event o n)
			++  (diffChildEditorDefinitions` path event (i + 1) os ns)

//Update the value of a control
valueUpdate path mbEvent old new = update (sameValue mbEvent) (\{UIEditControl|value} -> Just value) UISetValue path old new
where
	sameValue Nothing old new
			= old.UIEditControl.value == new.UIEditControl.value
	sameValue (Just (TaskEvent eTask (eName,eValue))) old new
		| old.UIEditControl.taskId == Just (toString eTask) && old.UIEditControl.name == eName
			= eValue  == new.UIEditControl.value
			= old.UIEditControl.value == new.UIEditControl.value

//Update the task id of a control
taskIdUpdate path old new	= update sameTaskId taskIdOf UISetTaskId path old new

//Update the name of a control
nameUpdate path old new		= update sameName nameOf UISetName path old new

update eqfun accfun consfun path old new
	| not (eqfun old new)	= maybe [] (\prop -> [consfun (toString path) prop]) (accfun new)
	| otherwise				= []

//If the menus are not exactly the same simply replace all of them 
diffUIMenus :: DiffPath [UIMenuButton] [UIMenuButton] -> [UIUpdate]
diffUIMenus path old new
	| old === new	= []
	| otherwise		=  reverse [UIRemove menupath i \\ i <- [0.. (length old - 1)]]
					++ [UIAdd menupath i (tuidef b) \\ i <- [0..] & b <- new]
where
	menupath = toString [MenuStep:path]	
	tuidef b	= {UIControl| content = UIMenuButton b, width = Nothing, height = Nothing, margins = Nothing}
	
sameTaskId :: !UIControlContent !UIControlContent -> Bool
sameTaskId a b = (taskIdOf a) == (taskIdOf b)

sameName :: !UIControlContent !UIControlContent -> Bool
sameName a b = (nameOf a) == (nameOf b)

taskIdOf :: !UIControlContent -> Maybe String
taskIdOf (UIEditControl _ {UIEditControl|taskId})		= taskId
taskIdOf (UIButton {UIButton|taskId})					= taskId
taskIdOf (UIListContainer {UIListContainer|taskId})		= taskId
taskIdOf _												= Nothing

nameOf :: !UIControlContent -> Maybe String
nameOf (UIEditControl _ {UIEditControl|name})			= Just name
nameOf (UIButton {UIButton|name})						= Just name
nameOf (UIListContainer {UIListContainer|name})			= name
nameOf _												= Nothing
*/
encodeUIUpdates :: ![UIUpdate] -> JSONNode
encodeUIUpdates updates = JSONArray (flatten (map encodeUIUpdate updates))

encodeUIUpdate :: UIUpdate -> [JSONNode]
encodeUIUpdate (UISetValue path value)			= [node path "setEditValue"		[value]]
encodeUIUpdate (UISetTaskId path taskId)		= [node path "setTaskId"	 	[JSONString taskId]]
encodeUIUpdate (UISetName path name)			= [node path "setName"			[JSONString name]]
encodeUIUpdate (UISetEnabled path enabled)		= [node path "setDisabled"		[JSONBool (not enabled)]]
encodeUIUpdate (UISetActive path active)		= [node path "setActive"		[JSONBool active]]
encodeUIUpdate (UISetTitle path title)			= [node path "setTitle"			[JSONString title]]
encodeUIUpdate (UIReplace path index def)		= [node path "replace" 			[JSONInt index, encodeUIControl def]]
encodeUIUpdate (UIUpdate path def)				= [node path "update"			[encodeUIControl def]]
encodeUIUpdate (UIAdd path index def)			= [node path "insert"			[JSONInt index, encodeUIControl def]]
encodeUIUpdate (UIRemove path index)			= [node path "remove"			[JSONInt index]]

node path method arguments
	= JSONObject [("path",JSONString path),("method",JSONString method),("arguments",JSONArray arguments)]

