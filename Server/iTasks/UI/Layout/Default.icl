implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Layout.Common
import iTasks.UI.Definition
import iTasks.API.Core.Types
import Text.JSON

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
		[layoutSubAt [1] editorToForm 
    	,layoutSubAt [1] finalizeForm 
		,copyAttributes [0] []
		,removeEmptyPrompt
		,setNodeType UIContainer
		] 

	removeEmptyPrompt = conditionalLayout emptyPrompt (removeSubAt [0])
	where
		emptyPrompt (UI _ _ [UI _ _ []:_]) = True
		emptyPrompt _ = False

finalizeForm :: Layout
finalizeForm
	= sequenceLayouts [layoutSubsMatching [] isFormItem layoutRow
					  ,setNodeType UIContainer
					  ]
where
	//Case when 
	layoutRow = sequenceLayouts
		[setAttributes ('DM'.unions [marginsAttr 5 5 5 5,directionAttr Horizontal, sizeAttr FlexSize WrapSize])
		,setNodeType UIContainer
		]

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = selectLayout
		[(isEmpty,setNodeType UIEmpty)
		,(hasActions,sequenceLayouts[layoutSubAt [0] finalizeUI, actionsToButtonBar,setNodeType UIPanel])
		,(const True,sequenceLayouts[unwrapUI,finalizeUI])
		]

	isEmpty (UI _ _ [] ) = True
	isEmpty _            = False

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
	[insertSubAt [1] buttonBar 				//Create a buttonbar
	,moveChildren [] isAction [1,0]   		//Move all actions to the buttonbar
	,layoutChildrenOf [1] actionToButton	//Transform actions to buttons 
	]

//Util predicates
isInteract = \n -> n =:(UI UIInteract _ _)
isStep = \n -> n =:(UI UIStep _ _)
isParallel = \n -> n =:(UI UIParallel _ _)
isAction = \n -> n =:(UI UIAction _ _)
isEmpty = \n -> n =:(UI UIEmpty _ _)
isFormItem = \n -> n =:(UI UIFormItem _ _)

isIntermediate (UI type _ _) = isMember type [UIInteract,UIStep,UIParallel]

isFormComponent (UI type _ _) = isMember type 
	[UITextField,UITextArea,UIPasswordField,UIIntegerField,UIDecimalField
	,UICheckbox,UIEditSlider,UIEditDocument,UIDropdown,UIRadioGroup,UICheckboxGroup]
instance == UINodeType where (==) x y = x === y

//Flatten an editor into a form
editorToForm :: Layout
editorToForm = sequenceLayouts [layoutSubsMatching [] isFormComponent toFormItem, wrapUI UIForm]

toFormItem :: Layout
toFormItem = layout
where
	layout (ReplaceUI (control=:(UI _ attr _)),s) 
		# label = fromMaybe (ui UIEmpty) (labelControl attr)
		# info = fromMaybe (ui UIEmpty) (infoControl attr)
		= (ReplaceUI (uic UIFormItem [label,control,info]),s)

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

buttonBar :: UI
buttonBar = uic UIButtonBar []

labelControl :: UIAttributes -> Maybe UI
labelControl attributes 
	= case 'DM'.get LABEL_ATTRIBUTE attributes of
		Just (JSONString label)
			# optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attributes)
			# (UI type attr items) = stringDisplay (formatLabel optional label)
			# attr = 'DM'.union (widthAttr (ExactSize LABEL_WIDTH)) attr
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

