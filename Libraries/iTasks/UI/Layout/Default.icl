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

defaultSessionLayout :: LayoutExpression
defaultSessionLayout = SequenceLayouts 
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,RemoveSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,SetUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: LayoutExpression
finalizeUI = SequenceLayouts
	[LayoutSubUIs (SelectByType UIInteract) finalizeInteract
	,LayoutSubUIs (SelectByType UIStep) finalizeStep
	,LayoutSubUIs (SelectByType UIParallel) finalizeParallel
	,LayoutSubUIs (SelectByType UIList) finalizeList
	]

finalizeList :: LayoutExpression
finalizeList = SequenceLayouts
	[LayoutSubUIs (SelectByDepth 1) (SetUIAttributes (heightAttr WrapSize))
	,SetUIAttributes (heightAttr WrapSize)
	]

finalizeInteract :: LayoutExpression
finalizeInteract = SequenceLayouts
		[copyContentTitle
		,LayoutSubUIs (SelectByPath [1]) finalizeEditor
		,removeEmptyPrompt
		,SetUIType UIContainer
		,LayoutSubUIs (SelectAND (SelectByPath []) (SelectByHasAttribute "title")) (SetUIType UIPanel)
		]
where
	copyContentTitle = CopySubUIAttributes (SelectKeys ["title"]) [0] []
	//removeEmptyPrompt = layoutSubUIs (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIs (SelectByPath [0]))
	removeEmptyPrompt = LayoutSubUIs (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (RemoveSubUIs (SelectByPath [0]))

finalizeEditor :: LayoutExpression
finalizeEditor = SequenceLayouts
	[LayoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIRecord)) finalizeRecord
	,LayoutSubUIs (SelectAND (SelectByPath []) (SelectByType UICons)) finalizeCons
	,LayoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIVarCons)) finalizeVarCons
	,LayoutSubUIs (SelectAND (SelectByPath []) selectFormComponent) finalizeFormComponent
	//Fallback in case the editor is some other container (this happens with lists...)
	,LayoutSubUIs (SelectAND SelectDescendents selectEditorIntermediate) finalizeEditor 
	]

selectFormComponent
	= SelectOR (SelectByHasAttribute LABEL_ATTRIBUTE)
		(foldl1 SelectOR [SelectByType t \\ t <- [UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
						        ,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup,UITextView,UIHtmlView]
					    ])

finalizeFormComponent = SequenceLayouts
	[LayoutSubUIs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: LayoutExpression
finalizeRecord = SequenceLayouts
	[LayoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,SetUIType UIContainer
	,SetUIAttributes (heightAttr WrapSize)
	]

finalizeCons :: LayoutExpression
finalizeCons = SequenceLayouts
	[LayoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,SetUIAttributes (directionAttr Horizontal)
	,SetUIType UIContainer
	,toFormItem
	]

finalizeVarCons :: LayoutExpression
finalizeVarCons = SequenceLayouts
	[LayoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,LayoutSubUIs (SelectByPath [0]) (SetUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,SetUIAttributes (directionAttr Horizontal)
	,SetUIType UIContainer
	,toFormItem
	]

finalizeStep :: LayoutExpression
finalizeStep = SequenceLayouts
	[removeDisabledActionsOfNestedSteps //In case of nested steps, memove disabled actions
	//If there are no actions, unwrap
	,LayoutSubUIs (ContainsNoChildOfType UIAction) (SequenceLayouts [CopySubUIAttributes SelectAll [] [0], UnwrapUI,finalizeUI])
	//If the previous rule did not eliminate the UIStep
	,LayoutSubUIs RootIsStep
		$ SequenceLayouts
			[LayoutSubUIs (SelectByPath [0]) finalizeUI
			,actionsToButtonBar
			,SetUIType UIPanel]
	]
where
	// Nested steps are steps having steps under them
	removeDisabledActionsOfNestedSteps
		= LayoutSubUIs
				(SelectAND                             // (Nested)
					(SelectByType UIStep)              // Steps (are steps)
						$ SelectByContains             // having
							$ SelectAND
								(SelectByType UIStep)  // steps
								SelectDescendents)     // under them
			removeDisabledActions

	removeDisabledActions
		= RemoveSubUIs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" ((==) (JSONBool False)))))

	ContainsNoChildOfType type = SelectAND (SelectByPath []) (SelectNOT (SelectByContains (SelectAND SelectChildren (SelectByType type))))
	RootIsStep = SelectAND (SelectByPath []) (SelectByType UIStep)

finalizeParallel :: LayoutExpression
finalizeParallel = SequenceLayouts
	[LayoutSubUIs (SelectAND (SelectByPath []) (SelectAND (SelectByType UIParallel) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
	,LayoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = SequenceLayouts
		[LayoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,SetUIType UIContainer
		]
	layoutWithActions = SequenceLayouts
		[actionsToButtonBar
		,LayoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,SetUIType UIPanel
		]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]

actionsToButtonBar = SequenceLayouts
	[InsertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,MoveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,LayoutSubUIs (SelectByPath [1]) (LayoutSubUIs SelectChildren actionToButton) //Transform actions to buttons 
	]

