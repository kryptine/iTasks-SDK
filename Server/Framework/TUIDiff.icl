implementation module TUIDiff

import StdBool, StdClass, StdList
import Util, GenUpdate, TUIDefinition

derive gEq TUITree, TUILayout, HAlignment

diffEditorDefinitions :: !TUIDef !TUIDef -> [TUIUpdate]
diffEditorDefinitions old new
	= diffEditorDefinitions` startDataPath old new
where
	diffEditorDefinitions` :: !DataPath !TUIDef !TUIDef -> [TUIUpdate]
	diffEditorDefinitions` path old new
		| sameType old new
			= contentUpdates old new ++ hintUpdate path old new ++ errorUpdate path old new 
		| otherwise
			//If not the same type, just replace
			= [TUIReplace (dp2s path) new]
	where
		contentUpdates old new = case (old,new) of
			// Documents are replaced when their value has changed
			(TUIControl (TUIDocumentControl odoc) _, TUIControl (TUIDocumentControl ndoc) _)
				| odoc.TUIDocumentControl.document == ndoc.TUIDocumentControl.document	= []
				| otherwise																= [TUIReplace (dp2s path) new]
			// Choices are replaced if the options are changed, otherwise their selection is updated
			(TUIControl (TUIChoiceControl oc) ob, TUIControl (TUIChoiceControl nc) nb)
				= if (oc.options == nc.options)
					if (ob.value == nb.value)
						[]
						[TUISetValue (dp2s path) nb.value]
					[TUIReplace (dp2s path) new]
			// Trees are replaced if the nodes are changed, otherwise their selection is updated
			(TUIControl (TUITreeControl oTree) ob, TUIControl (TUITreeControl nTree) nb)
				= if (oTree === nTree)
					if (ob.value == nb.value)
						[]
						[TUISetValue (dp2s path) nb.value]
					[TUIReplace (dp2s path) new]
			(TUIControl _ oc, TUIControl _ nc)
				= controlUpdate path oc nc new
			(TUIButton o,TUIButton n)
					| o.TUIButton.taskId <> n.TUIButton.taskId || o.TUIButton.action <> n.TUIButton.action || o.TUIButton.text <> n.TUIButton.text || o.TUIButton.iconCls <> n.TUIButton.iconCls
						= [TUIReplace (dp2s path) new]
					| o.TUIButton.disabled <> n.TUIButton.disabled
						= [TUISetEnabled (dp2s path) (not n.TUIButton.disabled)]
					| otherwise
						= []
			(TUIConstructorControl ccOld, TUIConstructorControl ccNew)
				//Same constructor: diff the children
				| ccOld.TUIConstructorControl.consSelIdx == ccNew.TUIConstructorControl.consSelIdx
					= flatten [  diffEditorDefinitions` (childDataPath path i) co cn
							  \\ co <- ccOld.TUIConstructorControl.items
							  &  cn <- ccNew.TUIConstructorControl.items
							  & i <- [0..] ]
				//Different constructor: replace everything
				| otherwise
					= [TUIReplace (dp2s path) new]
			(TUIGridContainer {gridEditors = oe,gridHtml = oh}, TUIGridContainer {gridEditors = ne, gridHtml = nh}) | length oe == length ne
				# htmlUpdates	= flatten [[TUISetValue  (dp2s path) (toString (toJSON (i,j,n))) \\ o <- or & n <- nr & j <- [0..] | o <> n] \\ or <- oh & nr <- nh & i <- [0..]]
				# path			= shiftDataPath path
				# editorUpdates	= flatten (flatten [[diffEditorDefinitions` (tablePath or path i j) o n \\ Just o <- or & Just n <- nr & j <- [0..]] \\ or <- oe & nr <- ne & i <- [0..]])
				= htmlUpdates ++ editorUpdates
			(TUIContainer o, TUIContainer n) | o.TUIContainer.cls == n.TUIContainer.cls && o.TUIContainer.restrictedWidth == n.TUIContainer.restrictedWidth && o.TUIContainer.layout === n.TUIContainer.layout
				# valueUpdates	= staticContainerUpdate path o.TUIContainer.items n.TUIContainer.items
				# lengthUpdates	= if (numOld < numNew)
					[TUIAdd (dp2s path) idx item \\item <- drop numMin n.TUIContainer.items & idx <- [numMin..]]
					(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
				= valueUpdates ++ lengthUpdates
			where
				numOld = length o.TUIContainer.items
				numNew = length n.TUIContainer.items
				numMin = min numOld numNew
			// Records are static except if they are optional
			(TUIRecordContainer o, TUIRecordContainer n)
				| o.TUIRecordContainer.optional <> n.TUIRecordContainer.optional = [TUIReplace (dp2s path) new]
				= case (o.hasValue,n.hasValue) of
					(True,True)		= staticContainerUpdate path o.TUIRecordContainer.items n.TUIRecordContainer.items
					(False,True)	= [TUISetValue (dp2s path) "expand":[TUIAdd (dp2s path) idx item \\ item <- n.TUIRecordContainer.items & idx <- [0..]]]
					(False,False)	= []
					(True,False)	= [TUISetValue (dp2s path) "collapse"]
			(TUIListContainer lcOld, TUIListContainer lcNew)
				# valueUpdates	= diffListItemDefinitions path lcOld.TUIListContainer.items lcNew.TUIListContainer.items
				# lengthUpdates	= if (numOld < numNew)
					[TUIAdd (dp2s path) idx item \\item <- drop numMin lcNew.TUIListContainer.items & idx <- [numMin..]]
					(reverse [TUIRemove (dp2s path) idx \\ idx <- [numMin..numOld-1]])
				= valueUpdates ++ lengthUpdates
				where
					numOld = length lcOld.TUIListContainer.items
					numNew = length lcNew.TUIListContainer.items
					numMin = min numOld numNew
					
					diffListItemDefinitions path old new
						= flatten [ diffEditorDefinitions` (childDataPath path i) (hd co) (hd cn)
						 		  \\(TUIListItem {TUIListItem|items=co}) <- old
								  & (TUIListItem {TUIListItem|items=cn}) <- new
								  & i <- [0..]]
			// Custom components need to figure out their own update on the client side
			(TUICustom oc, TUICustom nc)
				| oc === nc	= []
				| otherwise	= [TUIUpdate (dp2s path) new]
			// Fallback: always replace
			_	= [TUIReplace (dp2s path) new]
			
		//Simply update all child elements
		staticContainerUpdate path old new = flatten [diffEditorDefinitions` (childDataPath path i) co cn \\ co <- old & cn <- new & i <- [0..] ]
	
		controlUpdate path old new newTUI
			| old.TUIControl.taskId == new.TUIControl.taskId && old.TUIControl.name == new.TUIControl.name	= valueUpdate path old new
			| otherwise																						= [TUIReplace (dp2s path) newTUI]
	
		valueUpdate path old new	= update doUpdate (\{value} -> Just value) TUISetValue path old new
		where
			doUpdate old new = ov == new.value
			where
				ov = case new.eventValue of
					Just v	= v
					Nothing	= old.value
			
		hintUpdate path old new		= update sameHint hintOf TUISetHint path old new
		errorUpdate path old new	= update sameError errorOf TUISetError path old new
		
		update eqfun accfun consfun path old new
			| not (eqfun old new)	= maybe [] (\prop -> [consfun (dp2s path) prop]) (accfun new)
			| otherwise				= []
		
		// don't use index of column in datapath if there is only one column
		tablePath l path i j
			| length l == 1	= childDataPath path i
			| otherwise		= childDataPath (childDataPath path i) j

//Boilerplate type equality
sameType :: !TUIDef !TUIDef -> Bool
sameType (TUIControl ca _)				(TUIControl cb _)			= sameType` ca cb
where
	sameType` :: !TUIControlType !TUIControlType -> Bool
	sameType` TUIStringControl			TUIStringControl			= True
	sameType` TUICharControl			TUICharControl				= True
	sameType` TUIIntControl				TUIIntControl				= True
	sameType` TUIRealControl			TUIRealControl				= True
	sameType` TUIBoolControl			TUIBoolControl				= True
	sameType` TUINoteControl			TUINoteControl				= True
	sameType` TUIDateControl			TUIDateControl				= True
	sameType` TUITimeControl			TUITimeControl				= True
	sameType` TUIPasswordControl		TUIPasswordControl			= True
	sameType` TUIUserControl			TUIUserControl				= True
	sameType` (TUIChoiceControl _)		(TUIChoiceControl _)		= True
	sameType` (TUICurrencyControl _)	(TUICurrencyControl _)		= True
	sameType` (TUIDocumentControl _)	(TUIDocumentControl _)		= True
	sameType` (TUIButtonControl _)		(TUIButtonControl _)		= True
	sameType` (TUIHtmlDisplay)			(TUIHtmlDisplay)			= True
	sameType` (TUIORYXControl _)		(TUIORYXControl _)			= True
	sameType` (TUITreeControl _)		(TUITreeControl _)      	= True
	sameType` (TUICustomControl _ _)	(TUICustomControl _ _)		= True
	sameType` _							_							= False
sameType (TUIButton _)					(TUIButton _)				= True
sameType (TUIConstructorControl _)		(TUIConstructorControl _)	= True
sameType (TUIListItem _)				(TUIListItem _)				= True
sameType (TUIGridContainer _)			(TUIGridContainer _)        = True
sameType (TUIContainer _)				(TUIContainer _)			= True
sameType (TUIRecordContainer _)			(TUIRecordContainer _)		= True
sameType (TUIListContainer _)			(TUIListContainer _)		= True
sameType (TUIMenuButton _)				(TUIMenuButton _)			= True
sameType (TUIMenuItem _)				(TUIMenuItem _)				= True
sameType (TUIMenuSeparator)				(TUIMenuSeparator)			= True
sameType (TUICustom _)					(TUICustom _)				= True
sameType _								_ 							= False

sameError :: !TUIDef !TUIDef -> Bool
sameError a b = (errorOf a) == (errorOf b)

sameHint :: !TUIDef !TUIDef -> Bool
sameHint a b = (hintOf a) == (hintOf b)

errorOf :: !TUIDef -> Maybe String
errorOf (TUIControl _ {TUIControl|errorMsg})			= Just errorMsg
errorOf (TUIListContainer {TUIListContainer|errorMsg})	= Just errorMsg
errorOf _												= Nothing

hintOf :: !TUIDef -> Maybe String
hintOf (TUIControl _ {TUIControl|hintMsg})				= Just hintMsg
hintOf (TUIListContainer {TUIListContainer|hintMsg})	= Just hintMsg
hintOf _												= Nothing
