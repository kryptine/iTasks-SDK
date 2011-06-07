implementation module TUIDiff

import StdBool, StdClass, StdList, StdMisc
import Util, GenUpdate, TUIDefinition

derive gEq TUIControlType, TUIChoiceControl, TUIButtonControl, TUITree, TUIOrientation, TUISize, TUIHGravity, TUIVGravity, TUIMinSize, TUIMargins

gEq{|TUIConstructorControl|} _ _ = False

diffTUIDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
diffTUIDefinitions old new = diffEditorDefinitions` startDataPath old new

diffEditorDefinitions` :: !DataPath !TUIDef !TUIDef -> [TUIUpdate]
diffEditorDefinitions` path oldTui newTui
	| oldTui.margins === newTui.margins
		= case diffEditorDefinitions`` oldTui.TUIDef.content newTui.TUIDef.content of
			Just diff
				| oldTui.width === newTui.width && oldTui.height === newTui.height
					= diff
				| otherwise
					= [TUISetSize (dp2s path) newTui.width newTui.height:diff]
			Nothing
				= [TUIReplace (dp2s path) newTui]
	| otherwise
		= [TUIReplace (dp2s path) newTui]
where
	diffEditorDefinitions`` :: !TUIDefContent !TUIDefContent -> Maybe [TUIUpdate]
	diffEditorDefinitions`` old new = case (old,new) of
		(TUIControl (TUIConstructorControl ccOld) oc, TUIControl (TUIConstructorControl ccNew) nc)
			//Same constructor: diff the children
			| oc.value === nc.value && oc.TUIControl.taskId == nc.TUIControl.taskId && oc.TUIControl.name == nc.TUIControl.name
				= Just (flatten	[ diffEditorDefinitions` (childDataPath path i) co cn
								\\ co <- ccOld.TUIConstructorControl.items
								&  cn <- ccNew.TUIConstructorControl.items
								& i <- [0..] ])
		// Documents are replaced if their value has changed
		(TUIControl (TUIDocumentControl odoc) oc, TUIControl (TUIDocumentControl ndoc) nc)
			| odoc == ndoc && oc.TUIControl.taskId == nc.TUIControl.taskId && oc.TUIControl.name == nc.TUIControl.name
				= Just []
		(TUIControl (TUIHtmlDisplay tto) _, TUIControl (TUIHtmlDisplay ttn) _) | tto =!= ttn
			= Nothing
		(TUIControl otype oc, TUIControl ntype nc)
			| otype === ntype
				= Just (valueUpdate path oc nc ++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIButton o,TUIButton n)
				| o.TUIButton.text == n.TUIButton.text && o.TUIButton.iconCls == n.TUIButton.iconCls
					= Just (update (\o n -> o.TUIButton.disabled == n.TUIButton.disabled) (\b -> Just (not b.TUIButton.disabled)) TUISetEnabled path o n
						++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]])
		(TUIGridContainer {gridEditors = oe,gridHtml = oh}, TUIGridContainer {gridEditors = ne, gridHtml = nh}) | length oe == length ne
			# htmlUpdates	= flatten [[TUISetValue  (dp2s path) (toJSON (i,j,n)) \\ o <- or & n <- nr & j <- [0..] | o <> n] \\ or <- oh & nr <- nh & i <- [0..]]
			# path			= shiftDataPath path
			# editorUpdates	= flatten (flatten [[diffEditorDefinitions` (tablePath or path i j) o n \\ Just o <- or & Just n <- nr & j <- [0..]] \\ or <- oe & nr <- ne & i <- [0..]])
			= Just (htmlUpdates ++ editorUpdates)
		(TUILayoutContainer o, TUILayoutContainer n)	|  o.TUILayoutContainer.orientation === n.TUILayoutContainer.orientation
														&& o.TUILayoutContainer.hGravity === n.TUILayoutContainer.hGravity
														&& o.TUILayoutContainer.vGravity === n.TUILayoutContainer.vGravity
														&& o.TUILayoutContainer.frame === n.TUILayoutContainer.frame
														&& isJust o.TUILayoutContainer.title == isJust n.TUILayoutContainer.title
														&& isJust o.TUILayoutContainer.iconCls == isJust n.TUILayoutContainer.iconCls
			# valueUpdates	= staticContainerUpdate path o.TUILayoutContainer.items n.TUILayoutContainer.items
			# titleUpdate	= update (\o n -> o.TUILayoutContainer.title == n.TUILayoutContainer.title && o.TUILayoutContainer.iconCls == n.TUILayoutContainer.iconCls) (\{TUILayoutContainer|title,iconCls} -> fmap (\t -> (t,toString iconCls)) title) TUISetTitle path o n
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (dp2s path) idx item \\item <- drop numMin n.TUILayoutContainer.items & idx <- [numMin..]]
				(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
			= Just (titleUpdate ++ valueUpdates ++ lengthUpdates)			
		where
			numOld = length o.TUILayoutContainer.items
			numNew = length n.TUILayoutContainer.items
			numMin = min numOld numNew
		(TUIListContainer lcOld, TUIListContainer lcNew)
			# valueUpdates	= diffListItemDefinitions path lcOld.TUIListContainer.items lcNew.TUIListContainer.items
			# lengthUpdates	= if (numOld < numNew)
				[TUIAdd (dp2s path) idx item \\item <- drop numMin lcNew.TUIListContainer.items & idx <- [numMin..]]
				(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
			= Just (valueUpdates ++ lengthUpdates)
			where
				numOld = length lcOld.TUIListContainer.items
				numNew = length lcNew.TUIListContainer.items
				numMin = min numOld numNew
				
				diffListItemDefinitions path old new
					= flatten [  diffEditorDefinitions` (childDataPath path i) co cn
					 		  \\ {TUIDef|content=c=:(TUIListItem {TUIListItem|items=co})} <- old
							  &  {TUIDef|content=c=:(TUIListItem {TUIListItem|items=cn})} <- new
							  &  i <- [0..]]
							  
		(TUIMainContainer mcOld, TUIMainContainer mcNew)
			# menuUpdates = diffTUIDefinitionSets (childDataPath path 0) mcOld.TUIMainContainer.menus mcNew.TUIMainContainer.menus
			# itemUpdates = diffTUIDefinitionSets (childDataPath path 1) mcOld.TUIMainContainer.items mcNew.TUIMainContainer.items
			= Just (menuUpdates ++ itemUpdates)
		
		// Custom components need to figure out their own update on the client side
		(TUICustom oc, TUICustom nc)
			| oc === nc	= Just []
			| otherwise	= Just [TUIUpdate (dp2s path) newTui]
		// Fallback: always replace
		_	= Nothing
	
//Simply update all child elements
staticContainerUpdate path old new = flatten [diffEditorDefinitions` (childDataPath path i) co cn \\ co <- old & cn <- new & i <- [0..] ]

valueUpdate path old new = update sameValue (\{value} -> Just value) TUISetValue path old new
where
	sameValue old new = ov === new.value
	where
		ov = case new.eventValue of
			Just v	= toJSON v
			Nothing	= old.value

taskIdUpdate path old new	= update sameTaskId taskIdOf TUISetTaskId path old new
nameUpdate path old new		= update sameName nameOf TUISetName path old new

update eqfun accfun consfun path old new
	| not (eqfun old new)	= maybe [] (\prop -> [consfun (dp2s path) prop]) (accfun new)
	| otherwise				= []

// don't use index of column in datapath if there is only one column
tablePath l path i j
	| length l == 1	= childDataPath path i
	| otherwise		= childDataPath (childDataPath path i) j

diffTUIDefinitionSets :: DataPath [TUIDef] [TUIDef] -> [TUIUpdate]
diffTUIDefinitionSets path old new = diffTUIDefinitionSets` path 0 old new
where
	diffTUIDefinitionSets` path i [] []
		= []
	diffTUIDefinitionSets` path i [] old
		= [TUIRemove (dp2s path) n \\ n <- [i.. i + length old - 1]] //Less items in new than old
	diffTUIDefinitionSets` path i new []
		= [TUIAdd (dp2s path) n def \\ n <- [i..] & def <- new] //More items in new than old
	diffTUIDefinitionSets` path i [n:ns] [o:os]
		=	(diffEditorDefinitions` (childDataPath path i) n o)
		++  (diffTUIDefinitionSets` path (i + 1) ns os)

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