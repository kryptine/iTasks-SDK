implementation module TUIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple, sapldebug
import Text, Util, TUIDefinition
from Task import :: EditEvent(..), :: Event(..)

derive gEq TUIControlType, TUIButtonControl, TUITree, TUIDirection, TUISize, TUIHAlign, TUIVAlign, TUIMinSize, TUIMargins, TUIGridControl, TUISliderControl
derive gEq TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey

:: DiffPath :== [DiffStep] 
:: DiffStep	= ItemStep !Int | MenuStep

instance toString DiffPath
where
	toString path = join "/" (map step (reverse path))
	where
		step (ItemStep i) = toString i
		step (MenuStep) = "m"

diffTUIDefinitions :: !TUIDef !TUIDef !(Maybe EditEvent) -> [TUIUpdate]
diffTUIDefinitions old new event = diffEditorDefinitions` [ItemStep 0] event old new

diffEditorDefinitions` :: !DiffPath !(Maybe EditEvent) !TUIDef !TUIDef -> [TUIUpdate]
diffEditorDefinitions` path event oldTui {content = TUITaskletPlaceholder} = [] // Don't delete the tasklet, do nothing
diffEditorDefinitions` path event oldTui newTui
	| oldTui.margins === newTui.margins
		= case diffEditorDefinitions`` event oldTui.TUIDef.content newTui.TUIDef.content of
			Just diff
				| isFixed oldTui.width && isFixed oldTui.height && isFixed newTui.width && isFixed newTui.height
					//IMPORTANT: TUISetSize only works for fixed sizes
					= [TUISetSize (toString path) newTui.width newTui.height:diff]
				| otherwise
					= diff
			Nothing
				= [TUIReplace (toString ppath) pindex newTui]
	| otherwise
		= [TUIReplace (toString ppath) pindex newTui]
where
	[ItemStep pindex:ppath] = path

	isFixed (Just (Fixed _))	= True
	isFixed _					= False

	diffEditorDefinitions`` :: !(Maybe EditEvent) !TUIDefContent !TUIDefContent -> Maybe [TUIUpdate]
	diffEditorDefinitions`` event old new = case (old,new) of
		// Documents are replaced if their value has changed
		(TUIEditControl (TUIDocumentControl odoc) oc, TUIEditControl (TUIDocumentControl ndoc) nc)
			| odoc == ndoc && oc.TUIEditControl.taskId == nc.TUIEditControl.taskId && oc.TUIEditControl.name == nc.TUIEditControl.name
				= Just []
			| otherwise
				= Nothing
		(TUIEditControl (TUIGridControl ogrid) _, TUIEditControl (TUIGridControl ngrid) _)
			| ogrid =!= ngrid	= Nothing
		(TUIEditControl otype oc, TUIEditControl ntype nc)
			| otype === ntype
				= Just (valueUpdate path event oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIShowControl otype oc, TUIShowControl ntype nc)
			| otype === ntype && oc.TUIShowControl.value === nc.TUIShowControl.value
				= Just []
			| otherwise
				= Nothing
		(TUIButton o,TUIButton n)
			| o.TUIButton.text == n.TUIButton.text && o.TUIButton.iconCls == n.TUIButton.iconCls
				= Just (update (\o n -> o.TUIButton.disabled == n.TUIButton.disabled) (\b -> Just (not b.TUIButton.disabled)) TUISetEnabled path o n
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIContainer o, TUIContainer n)
			|  (o.TUIContainer.direction === n.TUIContainer.direction
				&& o.TUIContainer.halign === n.TUIContainer.halign
				&& o.TUIContainer.valign === n.TUIContainer.valign)
				= Just (diffChildEditorDefinitions path event o.TUIContainer.items n.TUIContainer.items)
		(TUIPanel o, TUIPanel n)
			| ( o.TUIPanel.direction === n.TUIPanel.direction
				&& o.TUIPanel.halign === n.TUIPanel.halign
				&& o.TUIPanel.valign === n.TUIPanel.valign
				&& o.TUIPanel.frame === n.TUIPanel.frame
				&& o.TUIPanel.menus === n.TUIPanel.menus
				&& (isJust o.TUIPanel.iconCls == isJust n.TUIPanel.iconCls))
					# titleUpdate	= update (\o n -> o.TUIPanel.title === n.TUIPanel.title && o.TUIPanel.iconCls == n.TUIPanel.iconCls) (\{TUIPanel|title,iconCls} -> Just (fromMaybe "" title,iconCls)) TUISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.TUIPanel.items n.TUIPanel.items
					# menuUpdates	= []
					//# menuUpdates	= diffTUIMenus path o.TUIPanel.menus n.TUIPanel.menus
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
		(TUIWindow o, TUIWindow n)
			|  (o.TUIWindow.direction === n.TUIWindow.direction
				&& o.TUIWindow.halign === n.TUIWindow.halign
				&& o.TUIWindow.valign === n.TUIWindow.valign)
				= Just (diffChildEditorDefinitions path event o.TUIWindow.items n.TUIWindow.items)
		(TUIListContainer lcOld, TUIListContainer lcNew)	
			= Just (diffChildEditorDefinitions path event (items lcOld) (items lcNew)
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
			where
				items lc = [{content = TUIListItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- lc.TUIListContainer.items]
		(TUIListItem liOld, TUIListItem liNew)
			= Just (diffChildEditorDefinitions path event [liOld.TUIListItem.items] [liNew.TUIListItem.items])
		(TUITabContainer tcOld, TUITabContainer tcNew)
			# activeTabUpdate	= update (\o n -> o.TUITabContainer.active == n.TUITabContainer.active) (\{TUITabContainer|active} -> Just active) TUISetActiveTab path tcOld tcNew
			# itemUpdates 		= diffChildEditorDefinitions path event (items tcOld) (items tcNew)
			= Just (itemUpdates ++ activeTabUpdate)
			where
				items tc = [{content = TUITabItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- tc.TUITabContainer.items]
		(TUITabItem o, TUITabItem n)
			| (o.TUITabItem.closeAction === n.TUITabItem.closeAction //Can't diff the close action for now
				&& o.TUITabItem.menus === n.TUITabItem.menus)		//Diff of menus is also still impossible
					# titleUpdate	= update (\o n -> o.TUITabItem.title == n.TUITabItem.title && o.TUITabItem.iconCls == n.TUITabItem.iconCls) (\{TUITabItem|title,iconCls} -> Just (title,iconCls)) TUISetTitle path o n
					# itemUpdates	= diffChildEditorDefinitions path event o.TUITabItem.items n.TUITabItem.items
					# menuUpdates 	= []
					= Just (titleUpdate ++ itemUpdates ++ menuUpdates)
			| otherwise
				= Nothing
		(TUIIcon o, TUIIcon n)
			| o.TUIIcon.type == n.TUIIcon.type
				&& o.TUIIcon.tooltip === n.TUIIcon.tooltip
					= Just []	
		// Custom components need to figure out their own update on the client side
		(TUICustom oc, TUICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [TUIUpdate (toString path) newTui]
		// Fallback: always replace
		_	= Nothing
	
	//Determine the updates for child items in containers, lists etc	
	diffChildEditorDefinitions :: DiffPath (Maybe EditEvent) [TUIDef] [TUIDef] -> [TUIUpdate]
	diffChildEditorDefinitions path event old new = diffChildEditorDefinitions` path event 0 old new
	where
		diffChildEditorDefinitions` path event i [] []
			= []
		diffChildEditorDefinitions` path event i old []
			//Less items in new than old (remove starting with the last item)
			= [TUIRemove (toString path) n \\ n <- reverse [i.. i + length old - 1 ]] 
		diffChildEditorDefinitions` path event i [] new
			//More items in new than old
			= [TUIAdd (toString path) n def \\ n <- [i..] & def <- new] 
		diffChildEditorDefinitions` path event i [o:os] [n:ns] 
			=	(diffEditorDefinitions` [ItemStep i:path] event o n)
			++  (diffChildEditorDefinitions` path event (i + 1) os ns)

//Update the value of a control
valueUpdate path mbEvent old new = update (sameValue mbEvent) (\{TUIEditControl|value} -> Just value) TUISetValue path old new
where
	sameValue Nothing old new
			= old.TUIEditControl.value == new.TUIEditControl.value
	sameValue (Just (TaskEvent eTask (eName,eValue))) old new
		| old.TUIEditControl.taskId == Just (toString eTask) && old.TUIEditControl.name == eName
			= eValue  == new.TUIEditControl.value
			= old.TUIEditControl.value == new.TUIEditControl.value

//Update the task id of a control
taskIdUpdate path old new	= update sameTaskId taskIdOf TUISetTaskId path old new

//Update the name of a control
nameUpdate path old new		= update sameName nameOf TUISetName path old new

update eqfun accfun consfun path old new
	| not (eqfun old new)	= maybe [] (\prop -> [consfun (toString path) prop]) (accfun new)
	| otherwise				= []

//If the menus are not exactly the same simply replace all of them 
diffTUIMenus :: DiffPath [TUIMenuButton] [TUIMenuButton] -> [TUIUpdate]
diffTUIMenus path old new
	| old === new	= []
	| otherwise		=  reverse [TUIRemove menupath i \\ i <- [0.. (length old - 1)]]
					++ [TUIAdd menupath i (tuidef b) \\ i <- [0..] & b <- new]
where
	menupath = toString [MenuStep:path]	
	tuidef b	= {TUIDef| content = TUIMenuButton b, width = Nothing, height = Nothing, margins = Nothing}
	
sameTaskId :: !TUIDefContent !TUIDefContent -> Bool
sameTaskId a b = (taskIdOf a) == (taskIdOf b)

sameName :: !TUIDefContent !TUIDefContent -> Bool
sameName a b = (nameOf a) == (nameOf b)

taskIdOf :: !TUIDefContent -> Maybe String
taskIdOf (TUIEditControl _ {TUIEditControl|taskId})			= taskId
taskIdOf (TUIButton {TUIButton|taskId})						= taskId
taskIdOf (TUIListContainer {TUIListContainer|taskId})		= taskId
taskIdOf _													= Nothing

nameOf :: !TUIDefContent -> Maybe String
nameOf (TUIEditControl _ {TUIEditControl|name})				= Just name
nameOf (TUIButton {TUIButton|name})							= Just name
nameOf (TUIListContainer {TUIListContainer|name})			= name
nameOf _													= Nothing
