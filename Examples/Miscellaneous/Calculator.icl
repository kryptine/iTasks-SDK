implementation module Calculator

import iTasks

calculatorExample :: [Workflow]
calculatorExample = [workflow "Examples/Miscellaneous/Calculator" "A simple calculator demonstrating how to layout buttons." calculator]

calculator :: Task Int
calculator = interactLocal "Calculator" parts initSt terms <<@ calculatorLayout
where
	initSt =	{ display		= 0
				, x				= 0
				, y				= 0
				, op			= +
				, showsResult	= False
				}
				
	parts st =
		[ DisplayView st.display
		, Update "7" (enterDigit 7)
		, Update "8" (enterDigit 8)
		, Update "9" (enterDigit 9)
		, Update "C" initSt
		, Update "4" (enterDigit 4)
		, Update "5" (enterDigit 5)
		, Update "6" (enterDigit 6)
		, Update "/" (calc (/) False)
		, Update "1" (enterDigit 1)
		, Update "2" (enterDigit 2)
		, Update "3" (enterDigit 3)
		, Update "*" (calc (*) False)
		, Update "0" (enterDigit 0)
		, Update "+" (calc (+) False)
		, Update "-" (calc (-) False)
		, Update "=" (calc st.op True)
		]
	where
		enterDigit d = {st & display = newV, y = newV, showsResult = False}
		where
			newV = if st.showsResult d (st.display*10 + d)
				
		calc nop alwaysCalc =	{ st
								& display		= v
								, x				= v
								, op			= nop
								, showsResult	= True
								}
		where
			v = if (not st.showsResult || alwaysCalc) (st.op st.x st.y) st.display
			
	terms {x} = UserActions [(ActionQuit,Just x)]
	
	calculatorLayout :: !TUIInteraction -> TUIDef
	calculatorLayout {title,buttons,editorParts=p=:[display:stButtons]} = defaultPanel
		title
		""
		(WrapContent 0)
		(defaultContent [display,columnLayout 4 stButtons] buttons)
	where
		buttonLayout buttons = buttonLayout` buttons []
		buttonLayout` buttons acc = case splitAt 4 buttons of
			([],_)		= reverse			acc
			(row,r)		= buttonLayout` r	[{content = TUILayoutContainer {defaultLayoutContainer row & orientation = Horizontal}, width = FillParent 1 ContentSize, height = (WrapContent 0), margins = Nothing}:acc]
	
:: CalculatorState =	{ display		:: !Int
						, x				:: !Int
						, y				:: !Int
						, op			:: !(Int Int -> Int)
						, showsResult	:: !Bool
						}
derive class iTask CalculatorState
derive bimap Maybe, (,)