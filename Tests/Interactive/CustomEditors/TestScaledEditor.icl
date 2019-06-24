module TestScaledEditor

import iTasks
import iTasks.Extensions.ScaledEditor
import StdFunctions
import Text.HTML

test = (testHtmlFrame -&&- testResizable) <<@ AddCSSClass "itasks-horizontal"

testHtmlFrame :: Task HtmlTag
testHtmlFrame = viewInformation () [ViewUsing id (scaledEditor 300 200 (htmlView <<@ styleAttr "padding: 0"))] html
		<<@ ApplyLayout (sequenceLayouts [layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize))
										,setUIAttributes (sizeAttr FlexSize FlexSize)
										]
			)
where
	html = DivTag [StyleAttr "width: 100%; height: 100%; background: blue; color: white; margin: 0"]
			[SpanTag [StyleAttr "font-size: 20px"] [Text "This text is automatically scaled"]
			]

testResizable :: Task String
testResizable = viewInformation () [] "RESIZE THIS PANEL"
			<<@ ApplyLayout (sequenceLayouts[setUIType UIPanel,setUIAttributes (resizableAttr [LeftSide])])

Start world = doTasks test world
