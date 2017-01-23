implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import iTasks.API.Core.Types
import Text.JSON
import GenEq

from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor
import qualified Data.Map as DM

LABEL_WIDTH :== 100

defaultSessionLayout :: Layout
defaultSessionLayout = sequenceLayouts 
    [finalizeUI                                      //Finalize all remaining intermediate layouts
	,removeSubsMatching [] isEmpty                   //Remove temporary placeholders
	,setAttributes (sizeAttr FlexSize FlexSize)      //Make sure we use the full viewport
    ]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = selectLayout
	[(isInteract,finalizeInteract) 	
	,(isStep,finalizeStep)
	,(isParallel,finalizeParallel)
	//Always recursively finalize the children
	,(const True,layoutSubsMatching [] isIntermediate finalizeUI)
	]

finalizeInteract :: Layout
finalizeInteract = conditionalLayout isInteract layout
where
	layout = sequenceLayouts 
		[layoutSubAt [1] finalizeEditor
		,copyAttributes ["title"] [0] []
		,removeEmptyPrompt
		,setNodeType UIPanel
		] 

	removeEmptyPrompt = conditionalLayout emptyPrompt (removeSubAt [0])
	where
		emptyPrompt (UI _ _ [UI _ _ []:_]) = True
		emptyPrompt _ = False

finalizeEditor :: Layout
finalizeEditor = selectLayout
	[(isRecord,finalizeRecord)
	,(isCons,finalizeCons)
	,(isVarCons,finalizeVarCons)
	,(isFormComponent,finalizeFormComponent)
	,(const True, layoutSubsMatching [] isEditorPart finalizeEditor)
	]

finalizeFormComponent = sequenceLayouts
	[layoutSubsMatching [] isEditorIntermediate finalizeEditor
	,toFormItem
	]

finalizeRecord :: Layout
finalizeRecord = sequenceLayouts
	[layoutSubsMatching [] isEditorPart finalizeEditor 
	,setNodeType UIContainer
	,setAttributes (heightAttr WrapSize)
	]

finalizeCons :: Layout
finalizeCons = sequenceLayouts
	[layoutSubsMatching [] isEditorPart finalizeEditor 
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]
finalizeVarCons :: Layout
finalizeVarCons = sequenceLayouts
	[layoutSubsMatching [] isEditorPart finalizeEditor 
	,layoutSubAt [0] (setAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = selectLayout
		[(hasActions, selectLayout
						[(nestedStep, sequenceLayouts [hideInActiveActions, layoutWithButtons])
						,(const True, layoutWithButtons)])
		,(const True, layoutMinimal)
		]
    //NOTE: We would like to be able to do the hasActions, after the nestedStep check
    //      Because the current selectLayout is static, we cannot do that...

	//Just unwrap
	layoutMinimal = sequenceLayouts [unwrapUI,finalizeUI]

	//Create a buttonbar with buttons for each action
	layoutWithButtons = sequenceLayouts [layoutSubAt [0] finalizeUI, actionsToButtonBar,setNodeType UIPanel]

	//For directly nested steps we hide all actions that are not active to prevent lots of
    //buttons that are not yet relevant to clutter up the UI
	hideInActiveActions = hideSubsMatching [] inActiveAction
	
	nestedStep (UI type attr [UI UIStep _ _:_]) = True
	nestedStep _ = False

	inActiveAction (UI type attr _) = (type =: UIAction) && (maybe False (\(JSONBool b) -> not b) ('DM'.get "enabled" attr))

finalizeParallel :: Layout
finalizeParallel = selectLayout
	[(\ui -> isParallel ui && hasActions ui,layoutWithActions)
	,(isParallel,layoutWithoutActions)
	]
where
	layoutWithoutActions = sequenceLayouts
		[layoutSubsMatching [] isIntermediate finalizeUI
		,setNodeType UIContainer
		]
	layoutWithActions = sequenceLayouts
		[actionsToButtonBar
		,layoutSubsMatching [] isIntermediate finalizeUI
		,setNodeType UIPanel
		]

hasActions (UI _ _ items) = any isAction items //Dangerous? TODO: check
	
actionsToButtonBar= sequenceLayouts
	[insertSubAt [1] (ui UIButtonBar) 	 //Create a buttonbar
	,moveChildren [] isAction [1,0]   	 //Move all actions to the buttonbar
	,layoutChildrenOf [1] actionToButton //Transform actions to buttons 
	]

//Util predicates
isInteract = \n -> n =:(UI UIInteract _ _)
isStep = \n -> n =:(UI UIStep _ _)
isParallel = \n -> n =:(UI UIParallel _ _)
isAction = \n -> n =:(UI UIAction _ _)
isEmpty = \n -> n =:(UI UIEmpty _ _)
isRecord = \n -> n =:(UI UIRecord _ _)
isCons = \n -> n =:(UI UICons _ _)
isVarCons = \n -> n =:(UI UIVarCons _ _)

isIntermediate (UI type _ _) = isMember type [UIInteract,UIStep,UIParallel]
isEditorIntermediate (UI type _ _) = isMember type [UIRecord, UICons, UIVarCons] 
isEditorPart ui = isEditorIntermediate ui || isFormComponent ui

isFormComponent (UI type attr _) = isMember type 
	[UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
	,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup
	,UITextView,UIHtmlView
	] || isJust ('DM'.get LABEL_ATTRIBUTE attr) //If another type (for example some editlet representation) has a label attribute we also need to process it

instance == UINodeType where (==) x y = x === y

//Flatten an editor into a form
toFormItem :: Layout
toFormItem = layout
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

