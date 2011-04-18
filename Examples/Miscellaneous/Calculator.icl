implementation module Calculator

import iTasks

calculatorExample :: [Workflow]
calculatorExample = [workflow "Examples/Miscellaneous/Calculator" "A simple calculator demonstrating how to layout buttons." calculator]

calculator :: Task Int
calculator = calculate 0 0 0 (+) False <<@ calculatorLayout
where
	calculate :: !Int !Int !Int !(Int Int -> Int) !Bool -> Task Int
	calculate display x y op showResult =
							showMessageAboutA "Calculator" Display actions display
		>>= \(action,_).	case action of
								Action "C" _	= calculator
								Action "+" _	= calc (+) False
								Action "-" _	= calc (-) False
								Action "*" _	= calc (*) False
								Action "/" _	= calc (/) False
								Action "=" _	= calc op True
								Action digit _	= enterDigit (toInt digit)
								_				= return x
	where
		enterDigit d = calculate newV x newV op False
		where
			newV = if showResult d (display*10 + d)
			
		calc nop alwaysCalc = calculate v v y nop True
		where
			v = if (not showResult || alwaysCalc) (op x y) display
				
	actions :: [TaskAction Int]
	actions = [(Action b b,always) \\ b <- calcButtons] ++ [(ActionQuit,always)]
	where
		calcButtons =	["7","8","9","C"
						,"4","5","6","/"
						,"1","2","3","*"
						,"0","+","-","="]
				
	calculatorLayout :: !TUIInteractive -> TUIDef
	calculatorLayout {title,mbContext,buttons,type,isControlTask} = defaultPanel
		title
		(defaultInteractiveIcon type isControlTask)
		[defaultContentPanel (maybeToList mbContext ++ buttonLayout buttons)]
		Auto
	where
		buttonLayout buttons = buttonLayout` buttons []
		buttonLayout` buttons acc = case splitAt 4 buttons of
			([],_)		= reverse			acc
			([quit],_)	= reverse			[{content = TUILayoutContainer {defaultLayoutContainer [quit] & orientation = Horizontal, hGravity = HGRight}, width = FillParent 1 ContentSize, height = Wrap}:acc]
			(row,r)		= buttonLayout` r	[{content = TUILayoutContainer {defaultLayoutContainer row & orientation = Horizontal}, width = FillParent 1 ContentSize, height = Wrap}:acc]