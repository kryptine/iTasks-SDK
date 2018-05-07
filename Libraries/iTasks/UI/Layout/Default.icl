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

import iTasks.UI.Layout.Minimal
import iTasks.UI.Layout.BasicForms

//Util:
defaultSessionLayout :: LayoutRule
defaultSessionLayout = basicFormsSessionLayout
/*
defaultSessionLayout = sequenceLayouts
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubUIs (SelectAND SelectDescendents (SelectByType UIEmpty))  //Remove temporary placeholders
	,setUIAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]
*/

//The finalize layouts remove all intermediate 
finalizeUI :: LayoutRule
finalizeUI = sequenceLayouts
	[layoutSubUIs (SelectByType UIInteract) finalizeInteract
	,layoutSubUIs (SelectByType UIStep) finalizeStep
	,layoutSubUIs (SelectByType UIParallel) finalizeParallel
	,layoutSubUIs (SelectByType UIList) finalizeList
	]

finalizeList :: LayoutRule
finalizeList = sequenceLayouts
	[layoutSubUIs (SelectByDepth 1) (setUIAttributes (heightAttr WrapSize))
	,setUIAttributes (heightAttr WrapSize)
	]

finalizeInteract :: LayoutRule
finalizeInteract = sequenceLayouts
		[copyContentTitle
		,layoutSubUIs (SelectByPath [1]) finalizeEditor
		,removeEmptyPrompt
		,setUIType UIContainer
		,layoutSubUIs (SelectAND (SelectByPath []) (SelectByHasAttribute "title")) (setUIType UIPanel)
		] 
where
	copyContentTitle = copySubUIAttributes (SelectKeys ["title"]) [0] []
	removeEmptyPrompt = layoutSubUIs (SelectAND (SelectByPath []) (SelectRelative [0] (SelectByNumChildren 0))) (removeSubUIs (SelectByPath [0]))

finalizeEditor :: LayoutRule
finalizeEditor = sequenceLayouts
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

finalizeFormComponent = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents (selectEditorIntermediate)) finalizeEditor
	,toFormItem
	]

selectEditorIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UICons, UIVarCons]]

selectEditorParts
	= SelectOR selectFormComponent selectEditorIntermediate

finalizeRecord :: LayoutRule
finalizeRecord = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor
	,setUIType UIContainer
	,setUIAttributes (heightAttr WrapSize)
	]

finalizeCons :: LayoutRule
finalizeCons = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor 
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeVarCons :: LayoutRule
finalizeVarCons = sequenceLayouts
	[layoutSubUIs (SelectAND SelectDescendents selectEditorParts) finalizeEditor
	,layoutSubUIs (SelectByPath [0]) (setUIAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setUIAttributes (directionAttr Horizontal)
	,setUIType UIContainer
	,toFormItem
	]

finalizeStep :: LayoutRule
finalizeStep = sequenceLayouts
	[removeDisabledActionsOfNestedSteps //In case of nested steps, memove disabled actions
	//If there are no actions, unwrap
	,layoutSubUIs (ContainsNoChildOfType UIAction) (sequenceLayouts [copySubUIAttributes SelectAll [] [0], unwrapUI,finalizeUI])
	//If the previous rule did not eliminate the UIStep
	,layoutSubUIs RootIsStep
		$ sequenceLayouts
			[layoutSubUIs (SelectByPath [0]) finalizeUI
			,actionsToButtonBar
			,setUIType UIPanel]
	]
where
	// Nested steps are steps having steps under them
	removeDisabledActionsOfNestedSteps
		= layoutSubUIs
				(SelectAND                             // (Nested)
					(SelectByType UIStep)              // Steps (are steps)
						$ SelectByContains             // having
							$ SelectAND
								(SelectByType UIStep)  // steps
								SelectDescendents)     // under them
			removeDisabledActions

	removeDisabledActions
		= removeSubUIs (SelectAND SelectChildren (SelectAND (SelectByType UIAction) (SelectByAttribute "enabled" ((==) (JSONBool False)))))

	ContainsNoChildOfType type = SelectAND (SelectByPath []) (SelectNOT (SelectByContains (SelectAND SelectChildren (SelectByType type))))
	RootIsStep = SelectAND (SelectByPath []) (SelectByType UIStep)

finalizeParallel :: LayoutRule
finalizeParallel = sequenceLayouts
	[layoutSubUIs (SelectAND (SelectByPath []) (SelectAND (SelectByType UIParallel) (SelectByContains (SelectAND SelectChildren (SelectByType UIAction))))) layoutWithActions
	,layoutSubUIs (SelectAND (SelectByPath []) (SelectByType UIParallel)) layoutWithoutActions
	]
where
	layoutWithoutActions = sequenceLayouts
		[layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIContainer
		]
	layoutWithActions = sequenceLayouts
		[actionsToButtonBar
		,layoutSubUIs (SelectAND SelectDescendents selectIntermediate) finalizeUI
		,setUIType UIPanel
		]

selectIntermediate
	= foldl1 SelectOR [SelectByType t \\ t <- [UIRecord, UIInteract, UIStep, UIParallel]]

actionsToButtonBar = sequenceLayouts
	[insertChildUI 1 (ui UIButtonBar) //Create a buttonbar
	,moveSubUIs (SelectAND SelectChildren (SelectByType UIAction)) [1] 0 //Move all actions to the buttonbar
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs SelectChildren actionToButton) //Transform actions to buttons
	]

