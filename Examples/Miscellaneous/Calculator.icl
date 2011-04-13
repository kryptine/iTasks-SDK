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
	calculatorLayout {title,mbContext,editor,buttons}
		= TUIContainer {simpleContainer [defaultTitlePanel title, defaultContentPanel (maybeToList mbContext ++ editor ++ [buttonPanel])] & restrictedWidth = True}
	where
		buttonPanel = TUIContainer (simpleContainer (buttonPanel` buttons []))
		buttonPanel` buttons acc = case splitAt 4 buttons of
			([],_)		= reverse acc
			([quit],_)	= reverse [TUIContainer {simpleContainer [quit] & layout = Horizontal HRight}:acc]
			(row,r)		= buttonPanel` r [TUIContainer {simpleContainer row & layout = Horizontal HLeft}:acc]
