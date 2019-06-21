module TestScaledEditor

import iTasks
import iTasks.Extensions.ScaledEditor
import StdFunctions
import Text.HTML

test :: Task HtmlTag
test = viewInformation () [ViewUsing id (scaledEditor 600 400 (htmlView <<@ styleAttr "padding: 0"))] html
		<<@ ApplyLayout (layoutSubUIs (SelectByPath [1]) (setUIAttributes (sizeAttr FlexSize FlexSize)))
where
	html = DivTag [StyleAttr "width: 100%; height: 100%; background: blue; color: white; margin: 0"]
			[SpanTag [StyleAttr "font-size: 20px"] [Text "This text is automatically scaled"]
			]

Start world = doTasks test world
