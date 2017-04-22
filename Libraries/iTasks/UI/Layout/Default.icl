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

//Util:
sequenceAllLayouts [] = idLayout
sequenceAllLayouts list = foldl1 sequenceLayouts list 

defaultSessionLayout :: Layout
//defaultSessionLayout = idLayout
defaultSessionLayout = sequenceAllLayouts 
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = sequenceAllLayouts
	[layoutSubUIs (SelectByType UIInteract) finalizeInteract
	,layoutSubUIs (SelectByType UIStep) finalizeStep
	,layoutSubUIs (SelectByType UIParallel) finalizeParallel
	]

finalizeInteract :: Layout
finalizeInteract = sequenceAllLayouts
		[copyContentTitle
		,layoutSubUIs (SelectByPath [1]) finalizeEditor
		,removeEmptyPrompt
		,setUIType UIPanel
		] 
where
	copyContentTitle = copySubUIAttributes (SelectKeys ["title"]) [0] []
	removeEmptyPrompt = layoutSubUIs (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIs (SelectByPath [0]))

finalizeEditor :: Layout
finalizeEditor = sequenceAllLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIRecord)) finalizeRecord
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UICons)) finalizeCons
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIVarCons)) finalizeVarCons
	,layoutSubUIs (SelectAND (SelectByPath []) selectFormComponent) finalizeFormComponent
	//Fallback in case the editor is some other container (this happens with lists...)
	,layoutSubUIs (SelectAND SelectDescendents selectEditorIntermediate) finalizeEditor 
	]

selectFormComponent
	= SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		(foldl1 SelectOR [SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ])

finalizeFormComponent = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: Layout
finalizeRecord = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIType UIContainer
	,setUIAttributes (heightAttr WrapSize)
	]

finalizeCons :: Layout
finalizeCons = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeVarCons :: Layout
finalizeVarCons = sequenceAllLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = sequenceAllLayouts
	[removeDisabledActionsOfNestedSteps //In case of nested steps, memove disabled actions
	//If there are no actions, unwrap
	,layoutSubUIs (ContainsNoChildOfType UIAction) (sequenceLayouts unwrapUI finalizeUI)
	//If the previous rule did not eliminate the UIStep
	,layoutSubUIs RootIsStep  
	 	(sequenceAllLayouts [layoutSubUIs (SelectByPath [0]) finalizeUI, actionsToButtonBar,setUIType UIPanel])
	]
where
	removeDisabledActionsOfNestedSteps //Select all children of type UIstep
		= layoutSubUIs (ContainsChildOfType UIStep)
			(sequenceAllLayouts 
				[removeDisabledActions //Remove disabled actions
				,layoutSubUIs (SelectAND SelectChildren (SelectByType UIStep)) removeDisabledActions //Recursively apply to the UIStep child
				]
			)
	removeDisabledActions
		= removeSubUIs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" (JSONBool False))))

	ContainsChildOfType type =SelectAND (SelectByPath []) (SelectByContains (SelectAND SelectChildren (SelectByType type)))
	ContainsNoChildOfType type =SelectAND (SelectByPath []) (SelectNOT (SelectByContains (SelectAND SelectChildren (SelectByType type))))
	RootIsStep = SelectAND (SelectByPath []) (SelectByType UIStep)

finalizeParallel :: Layout
finalizeParallel = sequenceAllLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectAND (SelectByType UIParallel) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = sequenceAllLayouts
		[layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIContainer
		]
	layoutWithActions = sequenceAllLayouts
		[actionsToButtonBar
		,layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIPanel
		]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]

actionsToButtonBar = sequenceAllLayouts
	[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren actionToButton) //Transform actions to buttons 
	]

//Flatten an editor into a form
toFormItem :: Layout
toFormItem = layoutSubUIs (SelectAND (SelectByPath []) (SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE) (SelectByHasAttribute HINT_ATTRIBUTE)))
	(sequenceAllLayouts
		//Create the 'row' that holds the form item
		[wrapUI UIContainer
		,setUIAttributes ('DM'.unions [marginsAttr 2 4 2 4, directionAttr Horizontal,valignAttr AlignMiddle, sizeAttr FlexSize WrapSize])
		//If there is a label attribute, create a label 
		,optAddLabel
		//If there is hint attribute, create an extra icon 
		,optAddIcon
		]
	)
where
	optAddLabel = layoutSubUIs (SelectByContains (SelectAND (SelectByPath [0]) (SelectByHasAttribute LABEL_ATTRIBUTE))) addLabel
	addLabel = sequenceAllLayouts
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
						(layoutSubUIs (SelectAND (SelectByPath []) (SelectByNumChildren 2)) (addIcon 2)) //A label was added
						(layoutSubUIs (SelectAND (SelectByPath []) (SelectByNumChildren 1)) (addIcon 1)) //No label was added
					)

	addIcon iconIndex = foldl1 sequenceLayouts
		[insertChildUI iconIndex (uia UIIcon (leftMarginAttr 5))
		,copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [iconIndex - 1] [iconIndex]
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
