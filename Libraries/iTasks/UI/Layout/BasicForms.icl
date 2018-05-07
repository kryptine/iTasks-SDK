implementation module iTasks.UI.Layout.BasicForms

import iTasks.UI.Definition
import iTasks.UI.Layout
import StdBool, StdString, StdArray, Data.List, Data.Maybe, Text.GenJSON
import qualified Data.Map as DM

basicFormsSessionLayout :: LayoutRule
basicFormsSessionLayout = layoutCombinatorContainers

layoutCombinatorContainers = sequenceLayouts
	[layoutSubUIs (SelectByType UIInteract) layoutInteract
	,layoutSubUIs SelectCombinatorContainers layoutCombinatorContainer
	,layoutSubUIs (SelectByType UIAction) layoutAsButton
	,removeSubUIs (SelectByType UIEmpty)
	]
SelectCombinatorContainers = foldr1 SelectOR
	(map SelectByType [UIStep,UIParallel])

layoutCombinatorContainer = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs SelectChildren layoutCombinatorContainers
	]

layoutInteract = sequenceLayouts
	[setUIType UIPanel
	,layoutSubUIs (SelectAND SelectDescendents SelectFormElement) layoutFormElement
	,layoutSubUIs (SelectAND SelectDescendents SelectEditorContainers) layoutEditorContainer
	]

SelectFormElement = SelectByHasAttribute LABEL_ATTRIBUTE
SelectEditorContainers = foldr1 SelectOR
	(map SelectByType [UIPair,UIRecord,UICons,UIVarCons])

layoutFormElement = sequenceLayouts
	[wrapUI UIContainer
	,setUIAttributes (directionAttr Horizontal)
	,insertChildUI 0 (uia UILabel (widthAttr (ExactSize 100)))
	,copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
	,layoutSubUIs (SelectByPath [0]) (modifyUIAttributes (SelectKeys ["label","optional","mode"]) createLabelText)
	//Layout potential sub forms (nested records)
	,layoutSubUIs (SelectByPath [1]) (layoutSubUIs (SelectAND SelectDescendents SelectFormElement) layoutFormElement)
	]
where
	createLabelText attr = textAttr text
	where	
		text = formatDefaultLabel label +++ (if (enterOrUpdate && not optional) "*" "") +++ ":"
		formatted = formatDefaultLabel label
		enterOrUpdate = maybe False (\(JSONString m) -> isMember m ["enter","update"]) ('DM'.get "mode" attr) 
		optional = maybe False (\(JSONBool b) -> b) ('DM'.get "optional" attr) 
		label = maybe "-" (\(JSONString s) -> s) ('DM'.get "label" attr)

	formatDefaultLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
	where
		[lname:lnames]		= fromString label
		addspace []			= []
		addspace [c:cs]
			| c == '_'			= [' ':addspace cs]
			| isUpper c			= [' ',toLower c:addspace cs]
			| otherwise			= [c:addspace cs]

layoutEditorContainer = sequenceLayouts
	[setUIType UIContainer
	,layoutSubUIs (SelectAND SelectDescendents SelectEditorContainers) layoutEditorContainer
	]

layoutAsButton = sequenceLayouts
	[setUIType UIButton
	,modifyUIAttributes (SelectKeys ["actionId"]) toButtonAttributes
	]
where
	toButtonAttributes attr 
		= maybe attr (\(JSONString a) -> 'DM'.unions [valueAttr (JSONString a),textAttr a]) ('DM'.get "actionId" attr)
