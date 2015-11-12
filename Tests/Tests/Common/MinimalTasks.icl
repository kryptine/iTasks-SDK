implementation module Tests.Common.MinimalTasks
import TestFramework
import iTasks.UI.Editor

minimalEditor :: Task String
minimalEditor = updateInformation "Minimal String editor" [] "Hello World"

minimalEditlet :: Task String
minimalEditlet = updateInformation "Minimal String editlet" [UpdateUsing id const (fromEditlet editlet)] "Hello World"
where
	editlet = { genUI      = \cid val w -> ({ComponentHTML|width=WrapSize,height=WrapSize,html=H1Tag [] [Text val]},w)
			  , initClient = \sv _ _ w -> (sv,w)
			  , appDiffClt = \_ cid n _ w -> (n,w)
			  , genDiffSrv = \o n -> if (o == n) Nothing (Just n) 
              , appDiffSrv = \n _ -> n
			  }

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (viewInformation "Result" []))]

minimalParallel :: Task (String,String)
minimalParallel =  	 updateInformation "Edit string 1" [] "A"
				-&&- updateInformation "Edit string 2" [] "B"
