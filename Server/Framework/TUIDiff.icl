implementation module TUIDiff

import StdBool, StdClass, StdList
import Util, GenUpdate, TUIDefinition

derive gEq TUITree

diffEditorDefinitions :: !TUIDef !TUIDef ![DataPath] -> [TUIUpdate]
diffEditorDefinitions old new alwaysUpdatePaths
	= diffEditorDefinitions` startDataPath old new
where
	diffEditorDefinitions` :: !DataPath !TUIDef !TUIDef -> [TUIUpdate]
	diffEditorDefinitions` path old new
		| sameType old new
			= valueUpdates old new ++ hintUpdate path old new ++ errorUpdate path old new 
		| otherwise
			//If not the same type, just replace
			= [TUIReplace_ (dp2s path) new]
	where
		valueUpdates old new
			| isStaticContainer old
				// Records and tuples have static children
				= staticContainerUpdate path old new
			| isDynamicContainer old
				// List and Constructor are special: they have dynamic children
				= dynamicContainerUpdate path old new
			| isControl old
				//If not same value, error or hint, create set instructions
				= valueUpdate path old new
			= case (old,new) of //Special cases
				// Records are static except if they are optional
				(TUIRecordContainer o, TUIRecordContainer n)
					= case (o.hasValue,n.hasValue) of
						(True,True)		= staticContainerUpdate path old new
						(False,True)	= map (TUIAddTo n.TUIRecordContainer.id) n.TUIRecordContainer.items
						(False,False)	= []
						(True,False)	= [TUIReplace_ (dp2s path) new]
				// Documents are replaced when their value has changed
				(TUIDocumentControl odoc, TUIDocumentControl ndoc)
					| odoc.TUIDocumentControl.document == ndoc.TUIDocumentControl.document	= []
					| otherwise																= [TUIReplace_ (dp2s path) new]
				// Choices are replaced if the options are changed, otherwise their selection is updated
				(TUIChoiceControl oc, TUIChoiceControl nc)
					= if (oc.options == nc.options)
						if (oc.selection == nc.selection)
							[]
							[TUISetValue_ (dp2s path) (toString (toJSON nc.selection))]
						[TUIReplace_ (dp2s path) new]
				// Trees are replaced if the nodes are changed, otherwise their selection is updated
				(TUITreeControl ot, TUITreeControl nt)
					= if (ot.tuiTree === nt.tuiTree)
						if (ot.selIndex == nt.selIndex)
							[]
							[TUISetValue_ (dp2s path) (toString nt.selIndex)]
						[TUIReplace_ (dp2s path) new]
				(TUIGridControl {gridEditors = oe,gridHtml = oh}, TUIGridControl {gridEditors = ne, gridHtml = nh})
					# htmlUpdates	= flatten [[TUISetValue_  (dp2s path) (toString (toJSON (i,j,n))) \\ o <- or & n <- nr & j <- [0..] | o <> n] \\ or <- oh & nr <- nh & i <- [0..]]
					# path			= shiftDataPath path
					# editorUpdates	= flatten (flatten [[diffEditorDefinitions` (childDataPath (childDataPath path i) j) o n \\ o <- or & n <- nr & j <- [0..]] \\ or <- oe & nr <- ne & i <- [0..]])
					= htmlUpdates ++ editorUpdates
				// Custom components need to figure out their own update on the client side
				(TUICustom oc, TUICustom nc)
					| oc === nc	= []
					| otherwise	= [TUIUpdate_ (dp2s path) new]
				// Fallback: always replace
				_	= [TUIReplace_ (dp2s path) new]
	
		valueUpdate path old new	= update (\o n -> sameValue o n && not (isMember path alwaysUpdatePaths)) valueOf TUISetValue_ path old new
		hintUpdate path old new		= update sameHint hintOf TUISetHint_ path old new	
		errorUpdate path old new	= update sameError errorOf TUISetError_ path old new
		
		update eqfun accfun consfun path old new
			| not (eqfun old new)
				= case accfun new of
					Just prop	= [consfun (dp2s path) prop]
					Nothing		= []
			| otherwise			= []
	
		staticContainerUpdate path old new
			//Simply update all child elements
			= flatten [diffEditorDefinitions` (childDataPath path i) co cn \\ co <- childrenOf old & cn <- childrenOf new & i <- [0..] ]
		
		dynamicContainerUpdate path (TUIListContainer lcOld) (TUIListContainer lcNew)
			# valueUpdates	= diffListItemDefinitions (path) lcOld.TUIListContainer.items lcNew.TUIListContainer.items
			# lengthUpdates	= if (numOld < numNew)
				[TUIAddTo id item \\item <- drop numMin lcNew.TUIListContainer.items]
				[TUIRemove (id +++ "#" +++ toString idx) \\ idx <- [numMin..numOld-1]]
			= valueUpdates ++ lengthUpdates
			where
				id = lcNew.TUIListContainer.id
				numOld = length lcOld.TUIListContainer.items
				numNew = length lcNew.TUIListContainer.items
				numMin = min numOld numNew
				
				diffListItemDefinitions path old new
					= flatten [ diffEditorDefinitions` (childDataPath path i) (hd co) (hd cn)
					 		  \\(TUIListItemControl {TUIListItemControl|items=co}) <- old
							  & (TUIListItemControl {TUIListItemControl|items=cn}) <- new
							  & i <- [0..]]
				
		dynamicContainerUpdate path (TUIConstructorControl ccOld) (TUIConstructorControl ccNew)
			//Same constructor: diff the children
			| ccOld.TUIConstructorControl.consSelIdx == ccNew.TUIConstructorControl.consSelIdx
				= flatten [  diffEditorDefinitions` (childDataPath path i) co cn
						  \\ co <- ccOld.TUIConstructorControl.items
						  &  cn <- ccNew.TUIConstructorControl.items
						  & i <- [0..] ]
			//Different constructor: replace everything
			| otherwise
				= [TUIReplace_ (dp2s path) new]
			
		dynamicContainerUpdate path old new
			= [TUIReplace_ (dp2s path) new]	//Just replace, dynamic containers are too difficult to do at once :)

//Boilerplate type equality
sameType :: TUIDef TUIDef -> Bool
sameType (TUIButton _)				(TUIButton _)				= True
sameType (TUIStringControl _)		(TUIStringControl _)		= True
sameType (TUICharControl _)			(TUICharControl _)			= True
sameType (TUIIntControl _)			(TUIIntControl _)			= True
sameType (TUIRealControl _)			(TUIRealControl _)			= True
sameType (TUIBoolControl _)			(TUIBoolControl _)			= True
sameType (TUINoteControl _)			(TUINoteControl _)			= True
sameType (TUIDateControl _)			(TUIDateControl _)			= True
sameType (TUITimeControl _)			(TUITimeControl _)			= True
sameType (TUIPasswordControl _)		(TUIPasswordControl _)		= True
sameType (TUIChoiceControl _)		(TUIChoiceControl _)		= True
sameType (TUICurrencyControl _)		(TUICurrencyControl _)		= True
sameType (TUIUserControl _)			(TUIUserControl _)			= True
sameType (TUIDocumentControl _)		(TUIDocumentControl _)		= True
sameType (TUIConstructorControl _)	(TUIConstructorControl _)	= True
sameType (TUIHiddenControl _)		(TUIHiddenControl _)		= True
sameType (TUIFormButtonControl _)	(TUIFormButtonControl _)	= True
sameType (TUIListItemControl _)		(TUIListItemControl _)		= True
sameType (TUIAppletControl _)       (TUIAppletControl _)        = True
sameType (TUIGridControl _)			(TUIGridControl _)        	= True
sameType (TUITreeControl _)			(TUITreeControl _)        	= True

sameType (TUITupleContainer _)		(TUITupleContainer _)		= True
sameType (TUIRecordContainer _)		(TUIRecordContainer _)		= True
sameType (TUIListContainer _)		(TUIListContainer _)		= True
sameType (TUIHtmlContainer _)		(TUIHtmlContainer _)		= True

sameType (TUIMenuButton _)			(TUIMenuButton _)			= True
sameType (TUIMenuItem _)			(TUIMenuItem _)				= True
sameType (TUIMenuSeparator)			(TUIMenuSeparator)			= True
sameType (TUICustom _)				(TUICustom _)				= True

sameType _							_ 							= False

sameValue :: TUIDef TUIDef -> Bool
sameValue a b = (valueOf a) == (valueOf b)

sameError :: TUIDef TUIDef -> Bool
sameError a b = (errorOf a) == (errorOf b)

sameHint :: TUIDef TUIDef -> Bool
sameHint a b = (hintOf a) == (hintOf b)

//Basic controls
isControl :: TUIDef -> Bool
isControl (TUIStringControl _)		= True
isControl (TUICharControl _)		= True
isControl (TUIIntControl _)			= True
isControl (TUIRealControl _)		= True
isControl (TUIBoolControl _)		= True
isControl (TUINoteControl _)		= True
isControl (TUIDateControl _)		= True
isControl (TUITimeControl _)		= True
isControl (TUIPasswordControl _)	= True
isControl (TUICurrencyControl _)	= True
isControl (TUIAppletControl _)		= True
isControl (TUIUserControl _)		= True
isControl (TUIHtmlContainer _)		= True
isControl _							= False

errorOf :: TUIDef -> Maybe String
errorOf (TUIStringControl {TUIBasicControl|errorMsg})		= Just errorMsg		
errorOf (TUICharControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIIntControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIRealControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIBoolControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUINoteControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIDateControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUITimeControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIPasswordControl {TUIBasicControl|errorMsg})		= Just errorMsg
errorOf (TUICurrencyControl {TUICurrencyControl|errorMsg})	= Just errorMsg
errorOf (TUIAppletControl {TUIAppletControl|errorMsg})      = Just errorMsg
errorOf (TUIUserControl {TUIBasicControl|errorMsg})			= Just errorMsg
errorOf (TUIListContainer {TUIListContainer|errorMsg})		= Just errorMsg
errorOf (TUIChoiceControl {TUIChoiceControl|errorMsg})		= Just errorMsg
errorOf _													= Nothing

hintOf :: TUIDef -> Maybe String
hintOf (TUIStringControl {TUIBasicControl|hintMsg})			= Just hintMsg		
hintOf (TUICharControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIIntControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIRealControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIBoolControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUINoteControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIDateControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUITimeControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIPasswordControl {TUIBasicControl|hintMsg})		= Just hintMsg
hintOf (TUICurrencyControl {TUICurrencyControl|hintMsg})	= Just hintMsg 
hintOf (TUIAppletControl {TUIAppletControl|hintMsg})		= Just hintMsg
hintOf (TUIUserControl {TUIBasicControl|hintMsg})			= Just hintMsg
hintOf (TUIListContainer {TUIListContainer|hintMsg})		= Just hintMsg
hintOf (TUIChoiceControl {TUIChoiceControl|hintMsg})		= Just hintMsg
hintOf _													= Nothing

//Static containers are GUI elements that contain other elements, but who's structure does not change
isStaticContainer :: TUIDef -> Bool
isStaticContainer (TUITupleContainer _)		= True
isStaticContainer _							= False

//Dynamic containers are GUI elements that contain other elements that can be added, removed or reordered
isDynamicContainer :: TUIDef -> Bool
isDynamicContainer (TUIConstructorControl _)= True
isDynamicContainer (TUIListContainer _)		= True
isDynamicContainer _						= False
