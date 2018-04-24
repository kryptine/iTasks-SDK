implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import Text.GenJSON
import Data.GenEq

from Data.Func import $
from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor, Data.Maybe
import Data.List, StdString
import qualified Data.Map as DM

//Util:
defaultSessionLayout :: LayoutRule
//defaultSessionLayout = idLayout
defaultSessionLayout = sequenceLayoutsRule
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubUIsRule (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setUIAttributesRule (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: LayoutRule
finalizeUI = sequenceLayoutsRule
	[layoutSubUIsRule (SelectByType UIInteract) finalizeInteract
	,layoutSubUIsRule (SelectByType UIStep) finalizeStep
	,layoutSubUIsRule (SelectByType UIParallel) finalizeParallel
	,layoutSubUIsRule (SelectByType UIList) finalizeList
	]

finalizeList :: LayoutRule
finalizeList = sequenceLayoutsRule
	[layoutSubUIsRule (SelectByDepth 1) (setUIAttributesRule (heightAttr WrapSize))
	,setUIAttributesRule (heightAttr WrapSize)
	]

finalizeInteract :: LayoutRule
finalizeInteract = sequenceLayoutsRule
		[copyContentTitle
		,layoutSubUIsRule (SelectByPath [1]) finalizeEditor
		,removeEmptyPrompt
		,setUITypeRule UIContainer
		,layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByHasAttribute "title")) (setUITypeRule UIPanel)
		] 
where
	copyContentTitle = copySubUIAttributesRule (SelectKeys ["title"]) [0] []
	removeEmptyPrompt = layoutSubUIsRule (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIsRule (SelectByPath [0]))

finalizeEditor :: LayoutRule
finalizeEditor = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByType UIRecord)) finalizeRecord
	,layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByType UICons)) finalizeCons
	,layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByType UIVarCons)) finalizeVarCons
	,layoutSubUIsRule (SelectAND (SelectByPath []) selectFormComponent) finalizeFormComponent
	//Fallback in case the editor is some other container (this happens with lists...)
	,layoutSubUIsRule (SelectAND SelectDescendents selectEditorIntermediate) finalizeEditor 
	]

selectFormComponent
	= SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		(foldl1 SelectOR [SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ])

finalizeFormComponent = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: LayoutRule
finalizeRecord = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND SelectDescendents selectEditorParts) finalizeEditor
	,setUITypeRule UIContainer
	,setUIAttributesRule (heightAttr WrapSize)
	]

finalizeCons :: LayoutRule
finalizeCons = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIAttributesRule (directionAttr Horizontal)
	,setUITypeRule UIContainer
	,toFormItem
	]

finalizeVarCons :: LayoutRule
finalizeVarCons = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND SelectDescendents selectEditorParts) finalizeEditor
	,layoutSubUIsRule (SelectByPath [0]) (setUIAttributesRule (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setUIAttributesRule (directionAttr Horizontal)
	,setUITypeRule UIContainer
	,toFormItem
	]

finalizeStep :: LayoutRule
finalizeStep = sequenceLayoutsRule
	[removeDisabledActionsOfNestedSteps //In case of nested steps, memove disabled actions
	//If there are no actions, unwrap
	,layoutSubUIsRule (ContainsNoChildOfType UIAction) (sequenceLayoutsRule [copySubUIAttributesRule SelectAll [] [0], unwrapUIRule,finalizeUI])
	//If the previous rule did not eliminate the UIStep
	,layoutSubUIsRule RootIsStep
		$ sequenceLayoutsRule
			[layoutSubUIsRule (SelectByPath [0]) finalizeUI
			,actionsToButtonBar
			,setUITypeRule UIPanel]
	]
where
	// Nested steps are steps having steps under them
	removeDisabledActionsOfNestedSteps
		= layoutSubUIsRule
				(SelectAND                             // (Nested)
					(SelectByType UIStep)              // Steps (are steps)
						$ SelectByContains             // having
							$ SelectAND
								(SelectByType UIStep)  // steps
								SelectDescendents)     // under them
			removeDisabledActions

	removeDisabledActions
		= removeSubUIsRule (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" ((==) (JSONBool False)))))

	ContainsNoChildOfType type = SelectAND (SelectByPath []) (SelectNOT (SelectByContains (SelectAND SelectChildren (SelectByType type))))
	RootIsStep = SelectAND (SelectByPath []) (SelectByType UIStep)

finalizeParallel :: LayoutRule
finalizeParallel = sequenceLayoutsRule
	[layoutSubUIsRule (SelectAND (SelectByPath []) (SelectAND (SelectByType UIParallel) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
	,layoutSubUIsRule (SelectAND (SelectByPath []) (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = sequenceLayoutsRule
		[layoutSubUIsRule (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUITypeRule UIContainer
		]
	layoutWithActions = sequenceLayoutsRule
		[actionsToButtonBar
		,layoutSubUIsRule (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUITypeRule UIPanel
		]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]

actionsToButtonBar = sequenceLayoutsRule
	[insertChildUIRule 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIsRule (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,layoutSubUIsRule (SelectByPath [1]) (layoutSubUIsRule SelectChildren actionToButton) //Transform actions to buttons
	]

