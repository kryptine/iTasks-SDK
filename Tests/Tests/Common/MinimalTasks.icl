implementation module Tests.Common.MinimalTasks
import TestFramework
import iTasks.UI.Editor

minimalEditor :: Task String
minimalEditor = updateInformation "Minimal String editor" [] "Hello World"

minimalEditlet :: Task String
minimalEditlet = updateInformation "Minimal String editlet" [UpdateUsing id const (fromEditlet editlet)] "Hello World"
where
	//Simple button
	editlet = { genUI      = \cid val w -> ({ComponentHTML|width=WrapSize,height=WrapSize,html=html cid},w)
			  , initClient = initClient
			  , appDiffClt = \_ cid n _ w -> (n,w)
			  , genDiffSrv = \o n -> if (o == n) Nothing (Just n) 
              , appDiffSrv = \n _ -> n
			  }

	html cid = ButtonTag [IdAttr (cid +++ "-button")] [Text "Click me"]

	//Register eventhandler for clicking the button
	initClient sv mkHandler cid world
		# (button, world) 	= .? (getElementById (cid +++ "-button")) world
		# world 			= jsSetObjectAttr "onclick" (toJSVal (mkHandler onClick cid)) button world
		= (sv,world)
	
 	onClick cid event cv world
		= (cv,Diff "Click" rollback,world)

	rollback _ cv world = (cv,NoDiff,world)

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (updateInformation "Result" []))]

minimalParallel :: Task (String,String)
minimalParallel =  	 updateInformation "Edit string 1" [] "A"
				-&&- updateInformation "Edit string 2" [] "B"
