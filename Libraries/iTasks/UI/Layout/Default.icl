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
	[layoutSubUIs (SelectAND SelectRoot (SelectAND (SelectByType UIStep) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction)))))
					 (foldl1 sequenceLayouts [layoutSubUIs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setUIType UIPanel])
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UIStep))
					(foldl1 sequenceLayouts [unwrapUI,finalizeUI])
	]
	//VERSION THAT SHOULD EVENTUALLY WORK... (NEEDS REDESIGN OF LAYOUT STATE FIRST)
/*
	//In case of nested steps, memove disabled actions
	[layoutSubUIs (SelectAND SelectRoot (SelectByHasChildrenOfType UIStep))
		(removeSubUIs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" (JSONBool True)))))
	//If there are no actions, unwrap
	,layoutSubUIs (SelectAND SelectRoot (SelectNOT (SelectByHasChildrenOfType UIStep)))
		(foldl1 sequenceLayouts [unwrapUI,finalizeUI])
	//Else, create a buttonbar
	,layoutSubUIs (SelectAND SelectRoot (SelectByType UIStep)) // (only if the previous layout has not yet eliminated the UIStep)
	 	(foldl1 sequenceLayouts [layoutSubUIs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setUIType UIPanel])
	]
*/
finalizeParallel :: Layout
finalizeParallel = foldl1 sequenceLayouts
	[layoutSubUIs (SelectAND SelectRoot (SelectAND (SelectByType UIParallel) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
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
	[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren actionToButton) //Transform actions to buttons 
	]

//Flatten an editor into a form
toFormItem :: Layout
toFormItem = layoutSubUIs (SelectAND SelectRoot (SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE) (SelectByHasAttribute HINT_ATTRIBUTE)))
	(foldl1 sequenceLayouts
		//Create the 'row' that holds the form item
		[wrapUI UIContainer
		,setUIAttributes ('DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal, sizeAttr FlexSize WrapSize])
		//If there is a label attribute, create a label 
		,optAddLabel
		//If there is hint attribute, create an extra icon 
		,optAddIcon
		]
	)
where
	optAddLabel = layoutSubUIs (SelectByContains (SelectAND (SelectByPath [0]) (SelectByHasAttribute LABEL_ATTRIBUTE))) addLabel
	addLabel = foldl1 sequenceLayouts
		[insertChildUI 0 (uia UILabel (widthAttr (ExactSize LABEL_WIDTH)))
		,copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
		,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes (SelectKeys ["label","optional","mode"]) createLabelText)
		]
	where
		createLabelText attr = textAttr text
		where	
			text = formatDefaultLabel label +++ (if (enterOrUpdate && not optional) "*" "") +++ ":"
			formatted = formatDefaultLabel label
			enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attr) 
			optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr) 
			label = maybe "-" (\(JSONString s) -> s) ('DM'.get "label" attr)

	optAddIcon = layoutSubUIs (SelectByContains (SelectAND SelectChildren (SelectByHasAttribute HINT_ATTRIBUTE)))
					(sequenceLayouts 
						(layoutSubUIs (SelectAND SelectRoot (SelectByNumChildren 2)) (addIcon 2)) //A label was added
						(layoutSubUIs (SelectAND SelectRoot (SelectByNumChildren 1)) (addIcon 1)) //No label was added
					)

	addIcon iconIndex = foldl1 sequenceLayouts
		[insertChildUI iconIndex (uia UIIcon (leftMarginAttr 5))
		,copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [1] [iconIndex]
		,layoutSubUIs (SelectByPath [iconIndex]) (modifyUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) createIconAttr)
		]
	where
		createIconAttr attr = 'DM'.unions [iconClsAttr iconCls, tooltipAttr tooltip]
		where 
			iconCls = maybe "icon-info" (\(JSONString t) -> "icon-" +++ t) ('DM'.get HINT_TYPE_ATTRIBUTE attr)
			tooltip = maybe "-" (\(JSONString s) -> s) ('DM'.get HINT_ATTRIBUTE attr)

formatDefaultLabel :: String -> String 
formatDefaultLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= fromString label
	addspace []			= []
	addspace [c:cs]
		| c == '_'			= [' ':addspace cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]
