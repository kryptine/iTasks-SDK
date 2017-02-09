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
	,removeSubs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = foldl1 sequenceLayouts
	[layoutSubs (SelectByType UIInteract) finalizeInteract
	,layoutSubs (SelectByType UIStep) finalizeStep
	,layoutSubs (SelectByType UIParallel) finalizeParallel
	]

finalizeInteract :: Layout
finalizeInteract = layout
where
	layout = foldl1 sequenceLayouts 
		[layoutSubs (SelectByPath [1]) finalizeEditor
		,copyAttributes ["title"] [0] []
		,removeEmptyPrompt
		,setNodeType UIPanel
		] 

	removeEmptyPrompt = layoutSubs (SelectAND SelectRoot (SelectRelative [0] (SelectByNumChildren 0))) (removeSubs (SelectByPath [0]))

finalizeEditor :: Layout
finalizeEditor = foldl1 sequenceLayouts
	[layoutSubs (SelectAND SelectRoot (SelectByType UIRecord)) finalizeRecord
	,layoutSubs (SelectAND SelectRoot (SelectByType UICons)) finalizeCons
	,layoutSubs (SelectAND SelectRoot (SelectByType UIVarCons)) finalizeVarCons
	,layoutSubs (SelectAND SelectRoot selectFormComponent) finalizeFormComponent
	]

selectFormComponent
	= foldl SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		[SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ]

finalizeFormComponent = foldl1 sequenceLayouts
	[layoutSubs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectIntermediate
	= foldl SelectOR (SelectByType UIRecord) [SelectByType t \\ t <- [UIInteract, UIStep, UIParallel]]
selectEditorIntermediate
	= foldl SelectOR (SelectByType UIRecord) [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: Layout
finalizeRecord = foldl1 sequenceLayouts
	[layoutSubs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setNodeType UIContainer
	,setAttributes (heightAttr WrapSize)
	]


finalizeCons :: Layout
finalizeCons = foldl1 sequenceLayouts
	[layoutSubs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]
finalizeVarCons :: Layout
finalizeVarCons = foldl1 sequenceLayouts
	[layoutSubs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,layoutSubs (SelectByPath [0]) (setAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = foldl1 sequenceLayouts
	//STRAIGHTFORWARD TEMPORARY VERSION

	[layoutSubs (SelectAND SelectRoot (SelectAND (SelectByType UIStep) (SelectByHasChildrenOfType UIAction)))
					 (foldl1 sequenceLayouts [layoutSubs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setNodeType UIPanel])
	,layoutSubs (SelectAND SelectRoot (SelectByType UIStep))
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
	[layoutSubs (SelectAND SelectRoot (SelectAND (SelectByType UIParallel) (SelectByHasChildrenOfType UIAction))) layoutWithActions
	,layoutSubs (SelectAND SelectRoot (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = foldl1 sequenceLayouts
		[layoutSubs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setNodeType UIContainer
		]
	layoutWithActions = foldl1 sequenceLayouts
		[actionsToButtonBar
		,layoutSubs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setNodeType UIPanel
		]


actionsToButtonBar = foldl1 sequenceLayouts
	[insertSubAt [1] (ui UIButtonBar) //Create a buttonbar
	,moveSubs (SelectAND SelectChildren (SelectByType UIAction)) [1,0] //Move all actions to the buttonbar
	,layoutSubs (SelectByPath [1]) (layoutSubs SelectChildren actionToButton) //Transform actions to buttons 
	]

//Flatten an editor into a form
toFormItem :: Layout
toFormItem = {Layout|layout=layout}
where
	layout (ReplaceUI (control=:(UI _ attr _)),s) 
		# label = fromMaybe (ui UIEmpty) (labelControl attr)
		# info = fromMaybe (ui UIEmpty) (infoControl attr)
		# attr = 'DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal, sizeAttr FlexSize WrapSize]
		= (ReplaceUI (uiac UIContainer attr [label,control,info]),s)

	layout (ChangeUI localChanges childChanges,s) 
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

	layout (c,s) = (c,s)
	
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

