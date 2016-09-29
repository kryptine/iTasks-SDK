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
    [layoutSubsMatching [] isIntermediate finalizeUI //Finalize all remaining intermediate layouts
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
	,(const True,layoutChildrenOf [] finalizeUI)
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
	,(isFormComponent,toFormItem)
	,(const True, layoutChildrenOf [] finalizeEditor)
	]

finalizeRecord :: Layout
finalizeRecord = sequenceLayouts
	[layoutChildrenOf [] finalizeEditor 
	,setNodeType UIContainer
	,setAttributes (heightAttr WrapSize)
	]

finalizeCons :: Layout
finalizeCons = sequenceLayouts
	[layoutChildrenOf [] finalizeEditor 
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]
finalizeVarCons :: Layout
finalizeVarCons = sequenceLayouts
	[layoutChildrenOf [] finalizeEditor 
	,layoutSubAt [0] (setAttributes (widthAttr WrapSize)) //Make the constructor selection wrapping
	,setAttributes (directionAttr Horizontal)
	,setNodeType UIContainer
	,toFormItem
	]

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = selectLayout
		[(hasActions, sequenceLayouts [layoutSubAt [0] finalizeUI, actionsToButtonBar,setNodeType UIPanel])
		,(const True, sequenceLayouts [unwrapUI,finalizeUI])
		]

finalizeParallel :: Layout
finalizeParallel = selectLayout [(\ui -> isParallel ui && hasActions ui,layoutWithActions), (isParallel,layoutWithoutActions)]
where
	layoutWithoutActions = sequenceLayouts
		[layoutChildrenOf [] finalizeUI
		,setNodeType UIContainer
		]
	layoutWithActions = sequenceLayouts
		[actionsToButtonBar
		,layoutChildrenOf [] finalizeUI
		,setNodeType UIPanel
		]

	isSingle (UI _ _ [_]) = True
	isSingle _ = False

hasActions (UI _ _ items) = any isAction items
	
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

isFormComponent (UI type attr _) = isMember type 
	[UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
	,UICheckbox,UISlider,UIDocumentField,UIDropdown,UICheckGroup
	,UITextView,UIHtmlView
	] || isJust ('DM'.get LABEL_ATTRIBUTE attr)
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

	layout (c=:(ChangeUI localChanges childChanges),s) 
		//Check if the tooltip or icon needs to be updated
		= (ChangeUI [] (iconChanges ++ [(1,ChangeChild c)]),s)
	where
		iconChanges = case changeType ++ changeTooltip of
			[] = []
			changes = [(2,ChangeChild (ChangeUI changes []))]

		changeType = case [t \\ SetAttribute HINT_TYPE_ATTRIBUTE (JSONString t) <- localChanges] of
			[type] 	= [SetAttribute "iconCls" (JSONString ("icon-" +++ type))]
			_ 		= []

		changeTooltip= case [h \\ SetAttribute HINT_ATTRIBUTE (JSONString h) <- localChanges] of
			[hint] 	= [SetAttribute "tooltip" (JSONString hint)]
			_ 		= []
	
	layout (c,s) = (c,s)
	
labelControl :: UIAttributes -> Maybe UI
labelControl attributes 
	= case 'DM'.get LABEL_ATTRIBUTE attributes of
		Just (JSONString label)
			# optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attributes)
			# (UI type attr items) = stringDisplay (formatLabel optional label)
			# attr = 'DM'.unions [widthAttr (ExactSize LABEL_WIDTH), attr]
			= Just (UI type attr items)
		_   = Nothing

infoControl :: UIAttributes -> Maybe UI
infoControl attributes
	= case ('DM'.get HINT_TYPE_ATTRIBUTE attributes,'DM'.get HINT_ATTRIBUTE attributes) of
		(Just (JSONString type), Just (JSONString hint)) 	= Just (icon type hint)
		_ 						= Nothing
where
	icon type tooltip = uia UIIcon ('DM'.unions [leftMarginAttr 5,tooltipAttr tooltip,iconClsAttr ("icon-"+++type)])

formatLabel :: Bool String -> String
formatLabel optional label
	= camelCaseToWords label +++ if optional "" "*" +++ ":"
where
	camelCaseToWords label = {c \\ c <- [toUpper lname : addspace lnames]}
	where
		[lname:lnames]		= fromString label
		addspace []			= []
		addspace [c:cs]
			| c == '_'			= [' ':addspace cs]
			| isUpper c			= [' ',toLower c:addspace cs]
			| otherwise			= [c:addspace cs]

