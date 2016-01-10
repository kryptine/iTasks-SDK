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
autoLayoutSession = finalizeUI

//The finalize layouts remove all intermediate 
finalizeUI :: Layout
finalizeUI = selectLayout
	[(isInteract,finalizeInteract)
	,(isStep,finalizeStep)
	,(isParallel,finalizeParallel)
	,(onlyFinalizeChildren,layoutChildrenOf [] finalizeUI)
	]

onlyFinalizeChildren (UITabSet _ _ _) = True
onlyFinalizeChildren (UITab _ _ _)    = True
onlyFinalizeChildren _                = False

finalizeInteract :: Layout
finalizeInteract = conditionalLayout isInteract layout
where
	layout = sequenceLayouts 
		[layoutChild [1] finalizeForm
		,changeContainerType (\(UIInteract items) -> defaultPanel items)
		] 

finalizeForm :: Layout
finalizeForm
	= sequenceLayouts [layoutChildrenOf [] layoutRow
					  ,changeContainerType (\(UIForm items) -> defaultContainer items)
					  ]
where
	//Case when 
	layoutRow = selectLayout [(hasLabel,toRowWithLabel),(const True,toRowWithoutLabel)]
	
	hasLabel (UIFormItem UIEmpty _ _) = False
	hasLabel _ = True

	toRowWithLabel = changeContainerType (\(UIFormItem label item icon) -> row [label,item,icon])
	toRowWithoutLabel = sequenceLayouts 
							[changeContainerType (\(UIFormItem label item icon) -> row [label,item,icon])
							,removeChild [0]
							]

	row items = (setMargins 5 5 5 5 o setDirection Horizontal o setSize FlexSize WrapSize) (defaultContainer items)

finalizeStep :: Layout
finalizeStep = conditionalLayout isStep layout
where
	layout = sequenceLayouts
        [layoutChild [0] finalizeUI 			//Recursively finalize
        ,insertChild [1] buttonBar 				//Create a buttonbar
	    ,moveChildren [] isAction [1]   		//Move all actions to the buttonbar
	    ,layoutChildrenOf [1] actionToButton	//Transform actions to buttons 
        ,changeContainerType (\(UIStep items) -> defaultPanel items) //Change to a standard container
        ]

finalizeParallel :: Layout
finalizeParallel = conditionalLayout isParallel layout
where
	layout = sequenceLayouts
		[layoutChildrenOf [0] finalizeUI
		,layoutChildrenOf [1] finalizeUI
		,changeContainerType (\(UIParallel items) -> defaultPanel items)
		]

//Util predicates
isInteract = \n -> n =:(UIInteract _)
isStep = \n -> n =:(UIStep _)
isParallel = \n -> n =:(UIParallel _)
isAction = \n -> n =:(UIAction _)

//Flatten an editor into a form
editorToForm :: Layout
editorToForm = layout
where
	//Flatten the editor to a list of form items
	layout (ReplaceUI editor,s) = (ReplaceUI (UIForm (items editor)),s)
	where	
		items (UIEditor {UIEditor|optional,attributes} control)
			# label = fromMaybe UIEmpty (labelControl optional attributes)
			# info = fromMaybe UIEmpty (infoControl attributes)
			= [UIFormItem label control info]
		items UIEmpty //Placeholders for constructor changes
			= [UIFormItem UIEmpty UIEmpty UIEmpty] 
		items (UICompoundEditor _ parts) = flatten (map items parts)
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

//Finalize a form to a final UI container
formToBlock :: Layout
formToBlock = layout
where
	layout (NoChange,s) = (NoChange,s)
	layout (ReplaceUI (UIForm items),_)
		//Transform all items to standard containers
		# (rows,itemLabels) = unzip (mapLst (makeRow (labelInItems items)) items)
		# copts 	= {UIContainerOpts|defaultContainerOpts & direction=Vertical}
		= (ReplaceUI (UIBlock defaultSizeOpts copts rows),JSONArray itemLabels)

	layout (ReplaceUI def,_) = (ReplaceUI def,JSONNull) //Do nothing if it isn't a form

	layout (ChangeUI local itemChanges, JSONArray itemLabels)
		# itemChanges = shiftItemsInRows itemChanges [(i,l) \\ i <- [0..] & JSONBool l <- itemLabels]
		= (ChangeUI local itemChanges, JSONArray itemLabels) 

	layout (change,JSONNull) = (change,JSONNull) //Ignore, if we didn't arrange during replacement

	//Check if one of the form items has a label, in that case all form items need to be indented
	labelInItems [] = False
	labelInItems [UIFormItem UIEmpty _ _:is] = labelInItems is
	labelInItems _ = True
	
	makeRow labelsUsed isLast (UIFormItem labelDef itemDef iconDef) 
		= (defaultContainer ctrls,JSONBool noLabel)
		//= (setMargins 5 5 (if isLast 5 0) 5 (setSize FlexSize WrapSize (setDirection Horizontal (defaultContainer ctrls))),JSONBool noLabel)
	where
		ctrls = labelCtrl ++ itemCtrl ++ iconCtrl 

		noLabel = isEmpty labelCtrl
		labelCtrl = fromControl labelDef
		itemCtrl = if (labelsUsed && noLabel) (map id /*(setLeftMargin LABEL_WIDTH)*/ (fromControl itemDef)) (fromControl itemDef)
		iconCtrl = fromControl iconDef

		fromControl (UIControl c) 	= [UIControl c]
		//fromControl (UIEmpty c) 	= [c]
		fromControl _ 			 	= []

	//If the label was removed, the indices of the change of the item and its icon are one less
	//TODO: Handle dynamic forms (AddChild and RemoveChild)
	shiftItemsInRows [] _ = [] 
	shiftItemsInRows [(ChangeChild n c):cs] labelInfo
		//Not all form items have to have explicit changes, so we need to drop the labelInfo for those missing items
		# [(_,noLabel):ls] = dropWhile (\(i,_) -> i < n) labelInfo 
		//If we removed the label, decrease all indices
		= [(ChangeChild n (if noLabel (shiftUp c) c)):shiftItemsInRows cs ls] 
	where
		shiftUp (ChangeUI l is) = ChangeUI l [ChangeChild (n - 1) c \\ ChangeChild n c <- is | n > 0]
		shiftUp c = c

actionToButton :: Layout
actionToButton = layout 
where
	layout (ReplaceUI (UIAction {UIAction|taskId,action=action=:(Action actionId _),enabled}),_)
		= (ReplaceUI (UIControl (UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId=actionId}
				{UIButtonOpts|text = Just (actionName action), iconCls = (actionIcon action), disabled = not enabled})),JSONNull)
	
	layout (ChangeUI local [],s) = (ChangeUI (map remap local) [],s)
	layout (change,s) = (change,s)

	remap ("enable",[])  = ("setDisabled",[JSONBool False])
	remap ("disable",[]) = ("setDisabled",[JSONBool True])
	remap (op,args)      = (op,args)

mapLst f [] = []
mapLst f [x] = [f True x]
mapLst f [x:xs] = [f False x: mapLst f xs]

buttonBar :: UIDef 
buttonBar = (wrapHeight o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight o setBaseCls "buttonbar") (defaultPanel [])

labelControl :: Bool UIAttributes -> Maybe UIDef
labelControl optional attributes 
	= fmap (\l -> setWidth (ExactSize LABEL_WIDTH) (UIControl (stringDisplay (formatLabel optional l)))) ('DM'.get LABEL_ATTRIBUTE attributes)

infoControl :: UIAttributes -> Maybe UIDef
infoControl attributes
	= case ('DM'.get HINT_TYPE_ATTRIBUTE attributes,'DM'.get HINT_ATTRIBUTE attributes) of
		(Just type, Just hint) 	= Just (icon type hint)
		_ 						= Nothing
where
	icon type tooltip = setLeftMargin 5 (UIControl (UIIcon defaultFSizeOpts {UIIconOpts|iconCls = "icon-" +++ type, tooltip = Just tooltip}))

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

