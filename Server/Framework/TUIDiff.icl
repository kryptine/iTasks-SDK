implementation module TUIDiff

import StdBool, StdClass, StdList, StdMisc
import Util, GenUpdate, TUIDefinition

derive gEq TUIControlType, TUIChoiceControl, TUIButtonControl, TUITree, TUIOrientation, TUISize, TUIHGravity, TUIVGravity, TUIMinSize, TUIMargins

gEq{|TUIConstructorControl|} _ _ = abort "not implemented"

diffEditorDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
diffEditorDefinitions old new = diffEditorDefinitions` startDataPath old new
where
	diffEditorDefinitions` :: !DataPath !TUIDef !TUIDef -> [TUIUpdate]
	diffEditorDefinitions` path oldTui newTui
		| oldTui.width === newTui.width && oldTui.height === newTui.height && oldTui.margins === newTui.margins
			= diffEditorDefinitions`` path oldTui.content newTui.content
		| otherwise
			= [TUIReplace (dp2s path) newTui]
	where
		diffEditorDefinitions`` :: !DataPath !TUIDefContent !TUIDefContent -> [TUIUpdate]
		diffEditorDefinitions`` path old new = case (old,new) of
			(TUIControl (TUIConstructorControl ccOld) oc, TUIControl (TUIConstructorControl ccNew) nc)
				//Same constructor: diff the children
				| oc.value === nc.value && oc.TUIControl.taskId == nc.TUIControl.taskId && oc.TUIControl.name == nc.TUIControl.name
					= flatten [  diffEditorDefinitions` (childDataPath path i) co cn
							  \\ co <- ccOld.TUIConstructorControl.items
							  &  cn <- ccNew.TUIConstructorControl.items
							  & i <- [0..] ]
				//Different constructor: replace everything
				| otherwise
					= [TUIReplace (dp2s path) newTui]
			// Documents are replaced when their value has changed
			(TUIControl (TUIDocumentControl odoc) oc, TUIControl (TUIDocumentControl ndoc) nc)
				| odoc == ndoc && oc.TUIControl.taskId == nc.TUIControl.taskId && oc.TUIControl.name == nc.TUIControl.name
					= []
				| otherwise
					= [TUIReplace (dp2s path) newTui]
			(TUIControl otype oc, TUIControl ntype nc)
				| otype === ntype
					= valueUpdate path oc nc ++ flatten [f path old new \\ f <- [hintUpdate,errorUpdate,taskIdUpdate,nameUpdate]]
				| otherwise
					= [TUIReplace (dp2s path) newTui]
			(TUIButton o,TUIButton n)
					| o.TUIButton.text == n.TUIButton.text && o.TUIButton.iconCls == n.TUIButton.iconCls
						= update (\o n -> o.TUIButton.disabled == n.TUIButton.disabled) (\b -> Just (not b.TUIButton.disabled)) TUISetEnabled path o n
							++ flatten [f path old new \\ f <- [taskIdUpdate,nameUpdate]]
					| otherwise
						= [TUIReplace (dp2s path) newTui]
			(TUIGridContainer {gridEditors = oe,gridHtml = oh}, TUIGridContainer {gridEditors = ne, gridHtml = nh}) | length oe == length ne
				# htmlUpdates	= flatten [[TUISetValue  (dp2s path) (toJSON (i,j,n)) \\ o <- or & n <- nr & j <- [0..] | o <> n] \\ or <- oh & nr <- nh & i <- [0..]]
				# path			= shiftDataPath path
				# editorUpdates	= flatten (flatten [[diffEditorDefinitions` (tablePath or path i j) o n \\ Just o <- or & Just n <- nr & j <- [0..]] \\ or <- oe & nr <- ne & i <- [0..]])
				= htmlUpdates ++ editorUpdates
			(TUIFormContainer o, TUIFormContainer n)
				# valueUpdates	= staticContainerUpdate path o.TUIFormContainer.items n.TUIFormContainer.items
				# lengthUpdates	= if (numOld < numNew)
					[TUIAdd (dp2s path) idx item \\item <- drop numMin n.TUIFormContainer.items & idx <- [numMin..]]
					(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
				= valueUpdates ++ lengthUpdates
			where
				numOld = length o.TUIFormContainer.items
				numNew = length n.TUIFormContainer.items
				numMin = min numOld numNew
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
				= titleUpdate ++ valueUpdates ++ lengthUpdates
			where
				numOld = length o.TUILayoutContainer.items
				numNew = length n.TUILayoutContainer.items
				numMin = min numOld numNew
			// Records are static except if they are optional
			(TUIRecordContainer o, TUIRecordContainer n)
				| o.TUIRecordContainer.optional <> n.TUIRecordContainer.optional = [TUIReplace (dp2s path) newTui]
				= case (o.hasValue,n.hasValue) of
					(True,True)		= staticContainerUpdate path o.TUIRecordContainer.items n.TUIRecordContainer.items
					(False,True)	= [TUISetValue (dp2s path) (JSONString "expand"):[TUIAdd (dp2s path) idx item \\ item <- n.TUIRecordContainer.items & idx <- [0..]]]
					(False,False)	= []
					(True,False)	= [TUISetValue (dp2s path) (JSONString "collapse")]
			(TUIListContainer lcOld, TUIListContainer lcNew)
				# valueUpdates	= diffListItemDefinitions path lcOld.TUIListContainer.items lcNew.TUIListContainer.items
				# lengthUpdates	= if (numOld < numNew)
					[TUIAdd (dp2s path) idx item \\item <- drop numMin lcNew.TUIListContainer.items & idx <- [numMin..]]
					(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
				= valueUpdates ++ lengthUpdates ++ hintUpdate path old new ++ errorUpdate path old new
				where
					numOld = length lcOld.TUIListContainer.items
					numNew = length lcNew.TUIListContainer.items
					numMin = min numOld numNew
					
					diffListItemDefinitions path old new
						= flatten [  diffEditorDefinitions` (childDataPath path i) (hd co) (hd cn)
						 		  \\ {content=c=:(TUIListItem {TUIListItem|items=co})} <- old
								  &  {content=c=:(TUIListItem {TUIListItem|items=cn})} <- new
								  &  i <- [0..]]
			// Custom components need to figure out their own update on the client side
			(TUICustom oc, TUICustom nc)
				| oc === nc	= []
				| otherwise	= [TUIUpdate (dp2s path) newTui]
			// Fallback: always replace
			_	= [TUIReplace (dp2s path) newTui]
		
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
	hintUpdate path old new		= update sameHint hintOf TUISetHint path old new
	errorUpdate path old new	= update sameError errorOf TUISetError path old new
	
	update eqfun accfun consfun path old new
		| not (eqfun old new)	= maybe [] (\prop -> [consfun (dp2s path) prop]) (accfun new)
		| otherwise				= []
	
	// don't use index of column in datapath if there is only one column
	tablePath l path i j
		| length l == 1	= childDataPath path i
		| otherwise		= childDataPath (childDataPath path i) j

sameTaskId :: !TUIDefContent !TUIDefContent -> Bool
sameTaskId a b = (taskIdOf a) == (taskIdOf b)

sameName :: !TUIDefContent !TUIDefContent -> Bool
sameName a b = (nameOf a) == (nameOf b)

sameError :: !TUIDefContent !TUIDefContent -> Bool
sameError a b = (errorOf a) == (errorOf b)

sameHint :: !TUIDefContent !TUIDefContent -> Bool
sameHint a b = (hintOf a) == (hintOf b)

taskIdOf :: !TUIDefContent -> Maybe String
taskIdOf (TUIControl _ {TUIControl|taskId})					= Just taskId
taskIdOf (TUIButton {TUIButton|taskId})						= Just taskId
taskIdOf (TUIRecordContainer {TUIRecordContainer|taskId})	= Just taskId
taskIdOf (TUIListContainer {TUIListContainer|taskId})		= Just taskId
taskIdOf _													= Nothing

nameOf :: !TUIDefContent -> Maybe String
nameOf (TUIControl _ {TUIControl|name})						= Just name
nameOf (TUIButton {TUIButton|name})							= Just name
nameOf (TUIRecordContainer {TUIRecordContainer|name})		= Just name
nameOf (TUIListContainer {TUIListContainer|name})			= Just name
nameOf _													= Nothing

errorOf :: !TUIDefContent -> Maybe String
errorOf (TUIControl _ {TUIControl|errorMsg})				= Just errorMsg
errorOf (TUIListContainer {TUIListContainer|errorMsg})		= Just errorMsg
errorOf _													= Nothing

hintOf :: !TUIDefContent -> Maybe String
hintOf (TUIControl _ {TUIControl|hintMsg})					= Just hintMsg
hintOf (TUIListContainer {TUIListContainer|hintMsg})		= Just hintMsg
hintOf _													= Nothing
