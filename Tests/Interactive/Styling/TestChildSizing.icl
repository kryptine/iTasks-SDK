module TestChildSizing
import iTasks

test :: Task ()
test
	= (testItem "A" -&&- testItem "B") <<@ ArrangeHorizontal <<@ CSSStyle "justify-content: center"
	@! ()

testItem label = viewInformation () [] label <<@ ApplyLayout (setUIAttributes (sizeAttr (ExactSize 100) (ExactSize 300))) <<@ InPanel False

Start world = doTasks test world
