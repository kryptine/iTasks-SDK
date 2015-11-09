implementation module Tests.Common.MinimalTasks
import TestFramework

minimalEditor :: Task String
minimalEditor = updateInformation "Mimimal String editor" [] "Hello World"

minimalStep :: Task String
minimalStep = enterInformation "Minimal Step combinator" []
            >>* [OnAction ActionOk (hasValue (viewInformation "Result" []))]
