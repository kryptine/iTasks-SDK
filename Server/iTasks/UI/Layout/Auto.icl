implementation module iTasks.UI.Layout.Auto

import iTasks.UI.Layout
import Text.JSON

from StdFunc import id, o, const
import StdList, StdBool, StdArray, StdTuple, Data.Tuple, Data.Functor
import qualified Data.Map as DM

LABEL_WIDTH :== 100

autoLayoutInteract :: Layout
autoLayoutInteract = layoutChild [1] editorToForm //Remap changes in the editor, ignore changes to the prompt, it should be constant

autoLayoutStep :: Layout
autoLayoutStep = id

autoLayoutParallel :: Layout
autoLayoutParallel = id

autoLayoutAttach :: Layout
autoLayoutAttach = id

autoLayoutSession :: Layout
autoLayoutSession = sequenceLayouts [finalizeUI,changeContainerType (setSize WrapSize WrapSize o setTopMargin 50 o setMinWidth (ExactBound 600))]

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = selectLayout
	[(isInteract,finalizeInteract)
	,(isStep,finalizeStep)
	,(isParallel,finalizeParallel)
	,(onlyFinalizeChildren,layoutChildrenOf [] finalizeUI)
	]

onlyFinalizeChildren (UI (UITabSet _ _ ) _ _) = True
onlyFinalizeChildren (UI (UITab _ _ ) _ _)    = True
onlyFinalizeChildren _                        = False

finalizeInteract :: Layout
finalizeInteract = conditionalLayout isInteract layout
where
	layout = sequenceLayouts 
		[layoutChild [1] finalizeForm
		,changeContainerType (\(UI UIInteract attr items) -> UI defaultPanel attr items)
		] 

finalizeForm :: Layout
finalizeForm
	= sequenceLayouts [layoutChildrenOf [] layoutRow
					  ,changeContainerType (\(UI UIForm attr items) -> UI defaultContainer attr items)
					  ]
where
	//Case when 
	layoutRow = selectLayout [(hasLabel,toRowWithLabel),(const True,toRowWithoutLabel)]
	
	hasLabel (UI UIFormItem _ [UI UIEmpty _ _,_,_]) = False
	hasLabel _ = True

	toRowWithLabel = changeContainerType (\(UI UIFormItem _ [label,item,icon]) -> row [label,item,icon])
	toRowWithoutLabel = sequenceLayouts 
							[changeContainerType (\(UI UIFormItem _ [label,item,icon]) -> row [label,item,icon])
							,removeChild [0]
							]

	row items = (setMargins 5 5 5 5 o setDirection Horizontal o setSize FlexSize WrapSize) (uic defaultContainer items)

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = sequenceLayouts
        [layoutChild [0] finalizeUI 			//Recursively finalize
        ,insertChild [1] buttonBar 				//Create a buttonbar
	    ,moveChildren [] isAction [1]   		//Move all actions to the buttonbar
	    ,layoutChildrenOf [1] actionToButton	//Transform actions to buttons 
        ,changeContainerType (\(UI UIStep attr items) -> UI defaultPanel attr items) //Change to a standard container
        ]

finalizeParallel :: Layout
finalizeParallel = conditionalLayout isParallel layout
where
	layout = sequenceLayouts
		[layoutChildrenOf [0] finalizeUI
		,layoutChildrenOf [1] finalizeUI
		,changeContainerType (\(UI UIParallel attr items) -> UI defaultPanel attr items)
		]

//Util predicates
isInteract = \n -> n =:(UI UIInteract _ _)
isStep = \n -> n =:(UI UIStep _ _)
isParallel = \n -> n =:(UI UIParallel _ _)
isAction = \n -> n =:(UI (UIAction _) _ _)

//Flatten an editor into a form
editorToForm :: Layout
editorToForm = layout
where
	//Flatten the editor to a list of form items
	layout (ReplaceUI editor,s) = (ReplaceUI (uic UIForm (items editor)),s)
	where	
		items (UI (UIEditor {UIEditor|optional}) attr [control])
			# label = fromMaybe (ui UIEmpty) (labelControl optional attr)
			# info = fromMaybe (ui UIEmpty) (infoControl attr)
			= [uic UIFormItem [label,control,info]]
		items (UI UIEmpty _ _) //Placeholders for constructor changes
			= [uic UIFormItem [ui UIEmpty,ui UIEmpty, ui UIEmpty ]] 
		items (UI (UIEditor _) _ parts) = flatten (map items parts)
		items _ = []

	//Remap the changes to the flattened list of form items
	layout (change,s) = (ChangeUI [] (snd (flattenChanges 0 change)),s)
	where
		//Leaf
		flattenChanges n NoChange = (n + 1, [])
		//Leaf (Change the middle element of the form)
		flattenChanges n c=:(ReplaceUI _) = (n + 1, [ChangeChild n (ChangeUI [] [ChangeChild 1 c])])
		//Leaf (Update the middle element and check for attribute changes
		flattenChanges n c=:(ChangeUI local []) = (n + 1,[ChangeChild n (ChangeUI [] (iconChanges ++ [ChangeChild 1 c]))])
		where
			iconChanges = case changeType ++ changeTooltip of
				[] = []
				changes = [ChangeChild 2 (ChangeUI changes [])]

			changeType = case [t \\ ("setAttribute",[JSONString HINT_TYPE_ATTRIBUTE,JSONString t]) <- local] of
				[type] 	= [("setIconCls",[JSONString ("icon-" +++ type)])]
				_ 		= []

			changeTooltip= case [h \\ ("setAttribute",[JSONString HINT_ATTRIBUTE,JSONString h]) <- local] of
				[hint] 	= [("setTooltip",[JSONString hint])]
				_ 		= []
			
		//Container (search recursively for more form items)
		flattenChanges n (ChangeUI _ children) = flattenChildren n children
		where	
			flattenChildren n [] = (n,[])
			flattenChildren n [ChangeChild _ c:cs]
				# (n, childChanges) = flattenChanges n c
				# (n, remainderChanges) = flattenChildren n cs
				= (n, childChanges ++ remainderChanges)

actionToButton :: Layout
actionToButton = layout 
where
	layout (ReplaceUI (UI (UIAction {UIAction|taskId,action=action=:(Action actionId _),enabled}) _ _ ),_)
		= (ReplaceUI (ui (UIControl (UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId=actionId}
				{UIButtonOpts|text = Just (actionName action), iconCls = (actionIcon action), disabled = not enabled}))),JSONNull)
	
	layout (ChangeUI local [],s) = (ChangeUI (map remap local) [],s)
	layout (change,s) = (change,s)

	remap ("enable",[])  = ("setDisabled",[JSONBool False])
	remap ("disable",[]) = ("setDisabled",[JSONBool True])
	remap (op,args)      = (op,args)

mapLst f [] = []
mapLst f [x] = [f True x]
mapLst f [x:xs] = [f False x: mapLst f xs]

buttonBar :: UI
buttonBar = (wrapHeight o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight o setBaseCls "buttonbar") (uic defaultPanel [])

labelControl :: Bool UIAttributes -> Maybe UI
labelControl optional attributes 
	= fmap (\l -> setWidth (ExactSize LABEL_WIDTH) (stringDisplay (formatLabel optional l))) ('DM'.get LABEL_ATTRIBUTE attributes)

infoControl :: UIAttributes -> Maybe UI
infoControl attributes
	= case ('DM'.get HINT_TYPE_ATTRIBUTE attributes,'DM'.get HINT_ATTRIBUTE attributes) of
		(Just type, Just hint) 	= Just (icon type hint)
		_ 						= Nothing
where
	icon type tooltip = setLeftMargin 5 (ui (UIControl (UIIcon defaultFSizeOpts {UIIconOpts|iconCls = "icon-" +++ type, tooltip = Just tooltip})))

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

