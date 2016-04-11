implementation module iTasks.UI.Layout.Default

import iTasks.UI.Layout
import iTasks.UI.Definition
import iTasks.API.Core.Types
import Text.JSON

from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor
import qualified Data.Map as DM

LABEL_WIDTH :== 100

defaultSessionLayout :: Layout
defaultSessionLayout = sequenceLayouts 
    [layoutSubsMatching [] isIntermediate finalizeUI 	//Finalize all remaining intermediate layouts
	,removeSubsMatching [] isEmpty 						//Remove temporary placeholders
    ,changeNodeType (setSize FlexSize FlexSize) 		//Make sure we use the full viewport
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
		,removeEmptyPrompt
		,changeNodeType (\(UI UIInteract attr items) -> UI UIPanel attr items)
		] 

	removeEmptyPrompt = conditionalLayout emptyPrompt (removeSubAt [0])
	where
		emptyPrompt (UI _ _ [UI _ _ []:_]) = True
		emptyPrompt _ = False

finalizeForm :: Layout
finalizeForm
	= sequenceLayouts [layoutChildrenOf [] layoutRow
					  ,changeNodeType (\(UI UIForm attr items) -> UI UIContainer attr items)
					  ]
where
	//Case when 
	layoutRow = changeNodeType toRow
	where
		toRow (UI UIFormItem attr items) = (setMargins 5 5 5 5 o setDirection Horizontal o setSize FlexSize WrapSize) (uiac UIContainer attr items)
		toRow ui = ui

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = selectLayout
		[(isEmpty,changeNodeType (\(UI _ attr items) -> UI UIEmpty attr items))
		,(hasActions,sequenceLayouts[layoutSubAt [0] finalizeUI,moveActions])
		,(const True,sequenceLayouts[unwrapUI,finalizeUI])
		]

	isEmpty (UI _ _ [] ) = True
	isEmpty _            = False

	hasActions (UI _ _ items) = length items > 1
	moveActions = sequenceLayouts
        [insertSubAt [1] buttonBar 				//Create a buttonbar
	    ,moveChildren [] isAction [1,0]   		//Move all actions to the buttonbar
	    ,layoutChildrenOf [1] actionToButton	//Transform actions to buttons 
        ,changeNodeType (\(UI UIStep attr items) -> UI UIPanel attr items) //Change to a standard container
		]

finalizeParallel :: Layout
finalizeParallel = conditionalLayout isParallel layout
where
	layout = sequenceLayouts
		[layoutChildrenOf [] finalizeUI
		,selectLayout
			[(isSingle, unwrapUI)
			,(const True,changeNodeType (\(UI UIParallel attr items) -> UI UIContainer attr items))
			]
		]

	isSingle (UI _ _ [_]) = True
	isSingle _ = False
	
//Util predicates
isInteract = \n -> n =:(UI UIInteract _ _)
isStep = \n -> n =:(UI UIStep _ _)
isParallel = \n -> n =:(UI UIParallel _ _)
isAction = \n -> n =:(UI UIAction _ _)
isEmpty = \n -> n =:(UI UIEmpty _ _)

isIntermediate (UI type _ _) = isMember type [UIInteract,UIStep,UIParallel]

isFormComponent (UI type _ _) = isMember type 
	[UIEditString,UIEditNote,UIEditPassword,UIEditInt,UIEditDecimal
	,UIEditCheckbox,UIEditSlider,UIEditDate,UIEditTime,UIEditDateTime
	,UIEditDocument,UIEditButton,UIDropdown,UIRadioGroup,UICheckboxGroup]
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
		# iconChanges = []
		= (ChangeUI [] (iconChanges ++ [(1,ChangeChild c)]),s)
	where
		iconChanges = case changeType ++ changeTooltip of
			[] = []
			changes = [(2,ChangeChild (ChangeUI changes []))]

		changeType = case [t \\ ("setAttribute",[JSONString HINT_TYPE_ATTRIBUTE,JSONString t]) <- localChanges] of
			[type] 	= [("setAttribute",[JSONString "iconCls",JSONString ("icon-" +++ type)])]
			_ 		= []

		changeTooltip= case [h \\ ("setAttribute",[JSONString HINT_ATTRIBUTE,JSONString h]) <- localChanges] of
			[hint] 	= [("setAttribute",[JSONString "tooltip", JSONString hint])]
			_ 		= []
	
	layout (c,s) = (c,s)

actionToButton :: Layout
actionToButton = layout 
where
	layout (ReplaceUI (UI UIAction attr _),_)
		# buttonOpts = maybe id (\(JSONString a) -> setText a) ('DM'.get "actionId" attr)
		= (ReplaceUI (buttonOpts (uia UIActionButton attr)),JSONNull)
	
	layout (ChangeUI local [],s) = (ChangeUI (map remap local) [],s)
	layout (change,s) = (change,s)

	remap ("enable",[])  = ("setAttribute",[JSONString "enabled", JSONBool True])
	remap ("disable",[]) = ("setAttribute",[JSONString "enabled", JSONBool False])
	remap (op,args)      = (op,args)


mapLst f [] = []
mapLst f [x] = [f True x]
mapLst f [x:xs] = [f False x: mapLst f xs]

buttonBar :: UI
buttonBar = (wrapHeight o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight o setBaseCls "buttonbar") (uic UIPanel [])

labelControl :: UIAttributes -> Maybe UI
labelControl attributes 
	# optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attributes)
	= fmap (\(JSONString l) -> setWidth (ExactSize LABEL_WIDTH) (stringDisplay (formatLabel optional l))) ('DM'.get LABEL_ATTRIBUTE attributes)

infoControl :: UIAttributes -> Maybe UI
infoControl attributes
	= case ('DM'.get HINT_TYPE_ATTRIBUTE attributes,'DM'.get HINT_ATTRIBUTE attributes) of
		(Just (JSONString type), Just (JSONString hint)) 	= Just (icon type hint)
		_ 						= Nothing
where
	icon type tooltip = (setLeftMargin 5 o setTooltip tooltip o setIconCls ("icon-"+++type)) (ui UIIcon)

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

