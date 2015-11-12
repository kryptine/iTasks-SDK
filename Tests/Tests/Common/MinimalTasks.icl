implementation module Tests.Common.MinimalTasks
import TestFramework

minimalEditor :: Task String
minimalEditor = updateInformation "Minimal String editor" [] "Hello World"

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (viewInformation "Result" []))]

minimalParallel :: Task (String,String)
minimalParallel =  	 updateInformation "Edit string 1" [] "A"
				-&&- updateInformation "Edit string 2" [] "B"
