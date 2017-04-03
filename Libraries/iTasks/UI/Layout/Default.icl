implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import iTasks.API.Core.Types
import Text.JSON
import GenEq

from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor
import Data.List
import qualified Data.Map as DM

LABEL_WIDTH :== 100

defaultSessionLayout :: Layout
defaultSessionLayout = foldl1 sequenceLayouts 
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = foldl1 sequenceLayouts
	[layoutSubUIs (SelectByType UIInteract) finalizeInteract
	,layoutSubUIs (SelectByType UIStep) finalizeStep
	,layoutSubUIs (SelectByType UIParallel) finalizeParallel
	]

finalizeInteract :: Layout
finalizeInteract = layout
where
	layout = foldl1 sequenceLayouts 
		[layoutSubUIs (SelectByPath [1]) finalizeEditor
		,copySubUIAttributes (SelectKeys ["title"]) [0] []
		,removeEmptyPrompt
		,setUIType UIPanel
		] 

	removeEmptyPrompt = layoutSubUIs (SelectAND SelectRoot (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIs (SelectByPath [0]))

finalizeEditor :: Layout
finalizeEditor = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectRoot (SelectByType UIRecord)) finalizeRecord
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UICons)) finalizeCons
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UIVarCons)) finalizeVarCons
	,layoutSubUIs (SelectAND SelectRoot selectFormComponent) finalizeFormComponent
	]

selectFormComponent
	= foldl SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		[SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ]

finalizeFormComponent = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]
selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: Layout
finalizeRecord = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIType UIContainer
	,setUIAttributes (heightAttr WrapSize)
	]

finalizeCons :: Layout
finalizeCons = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]
finalizeVarCons :: Layout
finalizeVarCons = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = foldl1 sequenceLayouts
	//STRAIGHTFORWARD TEMPORARY VERSION
	[layoutSubUIs (SelectAND SelectRoot (SelectAND (SelectByType UIStep) (SelectByHasChildrenOfType UIAction)))
					 (foldl1 sequenceLayouts [layoutSubUIs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setUIType UIPanel])
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UIStep))
					(foldl1 sequenceLayouts [unwrapUI,finalizeUI])
	]
/*
	//VERSION THAT SHOULD EVENTUALLY WORK... (NEEDS REDESIGN OF LAYOUT STATE FIRST)
	//In case of nested steps, memove disabled actions
	[layoutSubs (SelectAND SelectRoot (SelectByHasChildrenOfType UIStep))
		(removeSubs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" (JSONBool True)))))
	//If there are no actions, unwrap
	,layoutSubs (SelectAND SelectRoot (SelectNOT (SelectByHasChildrenOfType UIStep)))
		(foldl1 sequenceLayouts [unwrapUI,finalizeUI])
	//Else, create a buttonbar
	,layoutSubs (SelectAND SelectRoot (SelectByType UIStep)) // (only if the previous layout has not yet eliminated the UIStep)
	 	(foldl1 sequenceLayouts [layoutSubs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setNodeType UIPanel])
	]
*/
finalizeParallel :: Layout
finalizeParallel = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectRoot (SelectAND (SelectByType UIParallel) (SelectByHasChildrenOfType UIAction))) layoutWithActions
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = foldl1 sequenceLayouts
		[layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIContainer
		]
	layoutWithActions = foldl1 sequenceLayouts
		[actionsToButtonBar
		,layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIPanel
		]

actionsToButtonBar = foldl1 sequenceLayouts
	[insertSubUI [1] (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren actionToButton) //Transform actions to buttons 
	]

//Flatten an editor into a form
toFormItem :: Layout
toFormItem = layoutSubUIs (SelectAND SelectRoot (SelectByHasAttribute LABEL_ATTRIBUTE))
	(foldl1 sequenceLayouts
		[wrapUI UIContainer
		,setUIAttributes ('DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal, sizeAttr FlexSize WrapSize])
		,insertSubUI [0] (uia UILabel (widthAttr (ExactSize LABEL_WIDTH)))
		,copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
		,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes (SelectKeys ["label","optional","mode"]) formatLabelAttr)
		]
	)
where
	formatLabelAttr attr
		# label = maybe "-" (\(JSONString s) -> s) ('DM'.get "label" attr)
		# optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr) 
		# enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attr) 
		# formatted = formatDefaultLabel label
		# text = (if (enterOrUpdate && not optional) (formatted +++ "*") formatted) +++ ":"
		= textAttr text
//idLayout 
/*
toFormItem = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)

	adjust (ReplaceUI (control=:(UI _ attr _)),s) 
		# label = fromMaybe (ui UIEmpty) (labelControl attr)
		# info = fromMaybe (ui UIEmpty) (infoControl attr)
		# attr = 'DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal, sizeAttr FlexSize WrapSize]
		= (ReplaceUI (uiac UIContainer attr [label,control,info]),s)

	adjust (ChangeUI localChanges childChanges,s) 
		//Check if the tooltip or icon needs to be updated
		#iconChanges = case [remap t v \\ SetAttribute t (JSONString v) <- localChanges | isMember t [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]] of
			[] = []
			changes = [(2,ChangeChild (ChangeUI changes []))]
		# localChanges = [c \\ c=:(SetAttribute t _) <- localChanges | not (isMember t [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE])]
		= (ChangeUI [] ([(1,ChangeChild (ChangeUI localChanges childChanges)):iconChanges]),s)
	where
		remap HINT_ATTRIBUTE v = SetAttribute "tooltip" (JSONString v)
		remap HINT_TYPE_ATTRIBUTE v = SetAttribute "iconCls" (JSONString ("icon-" +++ v))
		remap t v = SetAttribute t (JSONString v)

	adjust (c,s) = (c,s)

	restore _ = NoChange
*/
	
labelControl :: UIAttributes -> Maybe UI
labelControl attributes = case 'DM'.get LABEL_ATTRIBUTE attributes of
	Just (JSONString labelAttr)
		# optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attributes) 
		# enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attributes) 
		# formatted = formatDefaultLabel labelAttr
		# text = (if (enterOrUpdate && not optional) (formatted +++ "*") formatted) +++ ":"

		# attr = 'DM'.unions [textAttr text, widthAttr (ExactSize LABEL_WIDTH)]
		= Just (UI UILabel attr [])
/*
		# (UI type attr items) = stringDisplay text
		# attr = 'DM'.unions [widthAttr (ExactSize LABEL_WIDTH), attr]
		= Just (UI type attr items)
*/
	_   = Nothing

infoControl :: UIAttributes -> Maybe UI
infoControl attributes
	= case ('DM'.get HINT_TYPE_ATTRIBUTE attributes,'DM'.get HINT_ATTRIBUTE attributes) of
		(Just (JSONString type), Just (JSONString hint)) 	= Just (icon type hint)
		_ 						= Nothing
where
	icon type tooltip = uia UIIcon ('DM'.unions [leftMarginAttr 5,tooltipAttr tooltip,iconClsAttr ("icon-"+++type)])

formatDefaultLabel :: String -> String 
formatDefaultLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= fromString label
	addspace []			= []
	addspace [c:cs]
		| c == '_'			= [' ':addspace cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

