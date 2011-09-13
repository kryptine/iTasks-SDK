implementation module TUIDiff

import StdBool, StdClass, StdList, StdEnum, StdMisc, StdTuple
import Text, Util, GenUpdate, TUIDefinition

derive gEq TUIControlType, TUIChoiceControl, TUIButtonControl, TUITree, TUIDirection, TUISize, TUIHGravity, TUIVGravity, TUIMinSize, TUIMargins, TUIGridControl
derive gEq TUIMenuButton, TUIMenu, TUIMenuItem, Hotkey

:: DiffPath :== [DiffStep] 
:: DiffStep	= ItemStep !Int | MenuStep

instance toString DiffPath
where
	toString path = join "/" (map step (reverse path))
	where
		step (ItemStep i) = toString i
		step (MenuStep) = "m"

instance toInt DiffStep
where
	toInt (ItemStep i) = i
	toInt (MenuStep) = 0

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
				= [TUIReplace (toString ppath) (toInt pstep) newTui]
	| otherwise
		= [TUIReplace (toString ppath) (toInt pstep) newTui]
where
	[pstep:ppath] = path

	diffEditorDefinitions`` :: !TUIDefContent !TUIDefContent -> Maybe [TUIUpdate]
	diffEditorDefinitions`` old new = case (old,new) of
		// Documents are replaced if their value has changed
		(TUIControl (TUIDocumentControl odoc) oc, TUIControl (TUIDocumentControl ndoc) nc)
			| odoc == ndoc && oc.TUIControl.taskId == nc.TUIControl.taskId && oc.TUIControl.name == nc.TUIControl.name
				= Just []
		(TUIControl (TUIHtmlDisplay tto) _, TUIControl (TUIHtmlDisplay ttn) _) | tto =!= ttn
			= Nothing
		(TUIControl (TUIGridControl ogrid) _, TUIControl (TUIGridControl ngrid) _) | ogrid =!= ngrid
			= Nothing
		(TUIControl otype oc, TUIControl ntype nc)
			| otype === ntype
				= Just (valueUpdate path oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIButton o,TUIButton n)
				| o.TUIButton.text == n.TUIButton.text && o.TUIButton.iconCls == n.TUIButton.iconCls
					= Just (update (\o n -> o.TUIButton.disabled == n.TUIButton.disabled) (\b -> Just (not b.TUIButton.disabled)) TUISetEnabled path o n
						++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
	
		(TUIContainer o, TUIContainer n)	|  o.TUIContainer.direction === n.TUIContainer.direction
											&& o.TUIContainer.halign === n.TUIContainer.halign
											&& o.TUIContainer.valign === n.TUIContainer.valign
										
			# valueUpdates	= staticContainerUpdate path o.TUIContainer.items n.TUIContainer.items
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (toString path) idx item \\item <- drop numMin n.TUIContainer.items & idx <- [numMin..]]
				(reverse [TUIRemove (toString path) idx \\ idx <- [numMin..numOld-1]])
			= Just (valueUpdates ++ lengthUpdates)
		where
			numOld = length o.TUIContainer.items
			numNew = length n.TUIContainer.items
			numMin = min numOld numNew
		(TUIPanel o, TUIPanel n)	|  o.TUIPanel.direction === n.TUIPanel.direction
														&& o.TUIPanel.halign === n.TUIPanel.halign
														&& o.TUIPanel.valign === n.TUIPanel.valign
														&& o.TUIPanel.frame === n.TUIPanel.frame
														&& o.TUIPanel.title == n.TUIPanel.title
														&& isJust o.TUIPanel.iconCls == isJust n.TUIPanel.iconCls
			# valueUpdates	= staticContainerUpdate path o.TUIPanel.items n.TUIPanel.items
			# menuUpdates	= diffTUIMenus path o.TUIPanel.menus n.TUIPanel.menus
			# titleUpdate	= update (\o n -> o.TUIPanel.title == n.TUIPanel.title && o.TUIPanel.iconCls == n.TUIPanel.iconCls) (\{TUIPanel|title,iconCls} -> Just (title,iconCls)) TUISetTitle path o n
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (toString path) idx item \\item <- drop numMin n.TUIPanel.items & idx <- [numMin..]]
				(reverse [TUIRemove (toString path) idx \\ idx <- [numMin..numOld-1]])
			= Just (titleUpdate ++ valueUpdates ++ menuUpdates ++ lengthUpdates)
		where
			numOld = length o.TUIPanel.items
			numNew = length n.TUIPanel.items
			numMin = min numOld numNew
		(TUITabContainer o, TUITabContainer n)
			# valueUpdates	= staticContainerUpdate path oitems nitems
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (toString path) idx item \\item <- drop numMin nitems & idx <- [numMin..]]
				(reverse [TUIRemove (toString path) idx \\ idx <- [numMin..numOld-1]])
			= Just (valueUpdates ++ lengthUpdates)
		where
			oitems = map toDef o.TUITabContainer.items
			nitems = map toDef n.TUITabContainer.items
			numOld = length o.TUITabContainer.items
			numNew = length n.TUITabContainer.items
			numMin = min numOld numNew
			toDef tab = {TUIDef|content = TUITabItem tab, width= Auto, height = Auto, margins = Nothing}
		(TUITabItem o, TUITabItem n) | o.closeAction === n.closeAction
			# valueUpdates	= staticContainerUpdate path [o.TUITabItem.items] [n.TUITabItem.items]
			# menuUpdates	= diffTUIMenus path o.TUITabItem.menus n.TUITabItem.menus
			# titleUpdate	= update (\o n -> o.TUITabItem.title == n.TUITabItem.title && o.TUITabItem.iconCls == n.TUITabItem.iconCls) (\{TUITabItem|title,iconCls} -> Just (title,iconCls)) TUISetTitle path o n
			= Just (titleUpdate ++ valueUpdates ++ menuUpdates)
		(TUIListContainer lcOld, TUIListContainer lcNew)
			# valueUpdates	= diffListItemDefinitions path lcOld.TUIListContainer.items lcNew.TUIListContainer.items
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (toString path) idx item \\item <- drop numMin lcNew.TUIListContainer.items & idx <- [numMin..]]
				(reverse [TUIRemove (toString path) idx \\ idx <- [numMin..numOld-1]])
			= Just (valueUpdates ++ lengthUpdates)
			where
				numOld = length lcOld.TUIListContainer.items
				numNew = length lcNew.TUIListContainer.items
				numMin = min numOld numNew
				
				diffListItemDefinitions path old new
					= flatten [  diffEditorDefinitions` [ItemStep i:path] co cn
					 		  \\ {TUIDef|content=c=:(TUIListItem {TUIListItem|items=co})} <- old
							  &  {TUIDef|content=c=:(TUIListItem {TUIListItem|items=cn})} <- new
							  &  i <- [0..]]
		// Custom components need to figure out their own update on the client side
		(TUICustom oc, TUICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [TUIUpdate (toString path) newTui]
		// Fallback: always replace
		_	= Nothing

//Simply update all child elements
staticContainerUpdate path old new
	= flatten [diffEditorDefinitions` [ItemStep i:path] co cn \\ co <- old & cn <- new & i <- [0..] ]

//Update the value of a control
valueUpdate path old new = update sameValue (\{TUIControl|value} -> Just value) TUISetValue path old new
where
	sameValue old new = ov == new.TUIControl.value
	where
		ov = case new.eventValue of
			Just v	= toJSON v
			Nothing	= old.TUIControl.value

//Update the task id of a control
taskIdUpdate path old new	= update sameTaskId taskIdOf TUISetTaskId path old new

//Update the name of a control
nameUpdate path old new		= update sameName nameOf TUISetName path old new

update eqfun accfun consfun path old new
	| not (eqfun old new)	= maybe [] (\prop -> [consfun (toString path) prop]) (accfun new)
	| otherwise				= []

diffTUIDefinitionSets :: DiffPath [TUIDef] [TUIDef] -> [TUIUpdate]
diffTUIDefinitionSets path old new = diffTUIDefinitionSets` path 0 old new
where
	diffTUIDefinitionSets` path i [] []
		= []
	diffTUIDefinitionSets` path i old [] 
		= [TUIRemove (toString path) n \\ n <- reverse [i.. i + length old - 1 ]] //Less items in new than old (remove starting with the last item)
	diffTUIDefinitionSets` path i [] new
		= [TUIAdd (toString path) n def \\ n <- [i..] & def <- new] //More items in new than old
	diffTUIDefinitionSets` path i [n:ns] [o:os]
		=	(diffEditorDefinitions` [ItemStep i:path] n o)
		++  (diffTUIDefinitionSets` path (i + 1) ns os)

//If the menus are not exactly the same simply replace all of them 
diffTUIMenus :: DiffPath [TUIMenuButton] [TUIMenuButton] -> [TUIUpdate]
diffTUIMenus path old new
	| old === new	= []
	| otherwise		=  reverse [TUIRemove menupath i \\ i <- [0.. (length old - 1)]]
					++ [TUIAdd menupath i (tuidef b) \\ i <- [0..] & b <- new]
where
	menupath = toString [MenuStep:path]	
	tuidef b	= {TUIDef| content = TUIMenuButton b, width = Auto, height = Auto, margins = Nothing}
	
sameTaskId :: !TUIDefContent !TUIDefContent -> Bool
sameTaskId a b = (taskIdOf a) == (taskIdOf b)

sameName :: !TUIDefContent !TUIDefContent -> Bool
sameName a b = (nameOf a) == (nameOf b)

taskIdOf :: !TUIDefContent -> Maybe String
taskIdOf (TUIControl _ {TUIControl|taskId})					= Just taskId
taskIdOf (TUIButton {TUIButton|taskId})						= Just taskId
taskIdOf (TUIListContainer {TUIListContainer|taskId})		= Just taskId
taskIdOf _													= Nothing

nameOf :: !TUIDefContent -> Maybe String
nameOf (TUIControl _ {TUIControl|name})						= Just name
nameOf (TUIButton {TUIButton|name})							= Just name
nameOf (TUIListContainer {TUIListContainer|name})			= Just name
nameOf _													= Nothing