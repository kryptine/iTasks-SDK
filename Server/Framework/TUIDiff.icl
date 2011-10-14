implementation module TUIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple
import Text, Util, GenUpdate, TUIDefinition

derive gEq TUIControlType, TUIChoiceControl, TUIButtonControl, TUITree, TUIDirection, TUISize, TUIHAlign, TUIVAlign, TUIMinSize, TUIMargins, TUIGridControl
derive gEq TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey

:: DiffPath :== [DiffStep] 
:: DiffStep	= ItemStep !Int | MenuStep

instance toString DiffPath
where
	toString path = join "/" (map step (reverse path))
	where
		step (ItemStep i) = toString i
		step (MenuStep) = "m"

diffTUIDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
diffTUIDefinitions old new = diffEditorDefinitions` [ItemStep 0] old new

diffEditorDefinitions` :: !DiffPath !TUIDef !TUIDef -> [TUIUpdate]
diffEditorDefinitions` path oldTui newTui
	| oldTui.margins === newTui.margins
		= case diffEditorDefinitions`` oldTui.TUIDef.content newTui.TUIDef.content of
			Just diff
				| oldTui.width === newTui.width && oldTui.height === newTui.height
					= diff
				| otherwise
					= [TUISetSize (toString path) newTui.width newTui.height:diff]
			Nothing
				= [TUIReplace (toString ppath) pindex newTui]
	| otherwise
		= [TUIReplace (toString ppath) pindex newTui]
where
	[ItemStep pindex:ppath] = path

	diffEditorDefinitions`` :: !TUIDefContent !TUIDefContent -> Maybe [TUIUpdate]
	diffEditorDefinitions`` old new = case (old,new) of
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
				= Just (valueUpdate path oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIButton o,TUIButton n)
			| o.TUIButton.text == n.TUIButton.text && o.TUIButton.iconCls == n.TUIButton.iconCls
				= Just (update (\o n -> o.TUIButton.disabled == n.TUIButton.disabled) (\b -> Just (not b.TUIButton.disabled)) TUISetEnabled path o n
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIContainer o, TUIContainer n)
			|  (o.TUIContainer.direction === n.TUIContainer.direction
				&& o.TUIContainer.halign === n.TUIContainer.halign
				&& o.TUIContainer.valign === n.TUIContainer.valign)
				= Just (diffChildEditorDefinitions path o.TUIContainer.items n.TUIContainer.items)
		(TUIPanel o, TUIPanel n)
			| ( o.TUIPanel.direction === n.TUIPanel.direction
				&& o.TUIPanel.halign === n.TUIPanel.halign
				&& o.TUIPanel.valign === n.TUIPanel.valign
				&& o.TUIPanel.frame === n.TUIPanel.frame
				&& o.TUIPanel.title == n.TUIPanel.title
				&& (isJust o.TUIPanel.iconCls == isJust n.TUIPanel.iconCls))
					# titleUpdate	= update (\o n -> o.TUIPanel.title == n.TUIPanel.title && o.TUIPanel.iconCls == n.TUIPanel.iconCls) (\{TUIPanel|title,iconCls} -> Just (title,iconCls)) TUISetTitle path o n
					# valueUpdates	= diffChildEditorDefinitions path o.TUIPanel.items n.TUIPanel.items
					# menuUpdates	= []
					//# menuUpdates	= diffTUIMenus path o.TUIPanel.menus n.TUIPanel.menus
					= Just (titleUpdate ++ valueUpdates ++ menuUpdates)
		(TUIListContainer lcOld, TUIListContainer lcNew)	
			= Just (diffChildEditorDefinitions path (items lcOld) (items lcNew)
					++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
			where
				items lc = [{content = TUIListItem item, width = Nothing, height = Nothing, margins = Nothing} \\ item <- lc.TUIListContainer.items]
		(TUIListItem liOld, TUIListItem liNew)
			= Just (diffChildEditorDefinitions path [liOld.TUIListItem.items] [liNew.TUIListItem.items])
		// Custom components need to figure out their own update on the client side
		(TUICustom oc, TUICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [TUIUpdate (toString path) newTui]
		// Fallback: always replace
		_	= Nothing
	
	//Determine the updates for child items in containers, lists etc	
	diffChildEditorDefinitions :: DiffPath [TUIDef] [TUIDef] -> [TUIUpdate]
	diffChildEditorDefinitions path old new = diffChildEditorDefinitions` path 0 old new
	where
		diffChildEditorDefinitions` path i [] []
			= []
		diffChildEditorDefinitions` path i old []
			//Less items in new than old (remove starting with the last item)
			= [TUIRemove (toString path) n \\ n <- reverse [i.. i + length old - 1 ]] 
		diffChildEditorDefinitions` path i [] new
			//More items in new than old
			= [TUIAdd (toString path) n def \\ n <- [i..] & def <- new] 
		diffChildEditorDefinitions` path i [o:os] [n:ns] 
			=	(diffEditorDefinitions` [ItemStep i:path] o n)
			++  (diffChildEditorDefinitions` path (i + 1) os ns)

//Update the value of a control
valueUpdate path old new = update sameValue (\{TUIEditControl|value} -> Just value) TUISetValue path old new
where
	sameValue old new = ov == new.TUIEditControl.value
	where
		ov = case new.eventValue of
			Just v	= toJSON v
			Nothing	= old.TUIEditControl.value

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
taskIdOf (TUIEditControl _ {TUIEditControl|taskId})			= Just taskId
taskIdOf (TUIButton {TUIButton|taskId})						= Just taskId
taskIdOf (TUIListContainer {TUIListContainer|taskId})		= taskId
taskIdOf _													= Nothing

nameOf :: !TUIDefContent -> Maybe String
nameOf (TUIEditControl _ {TUIEditControl|name})				= Just name
nameOf (TUIButton {TUIButton|name})							= Just name
nameOf (TUIListContainer {TUIListContainer|name})			= name
nameOf _													= Nothing