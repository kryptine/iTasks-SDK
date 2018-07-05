implementation module BasicAPIExamples.InteractionWithTheSystem.RunProcess

import iTasks
import iTasks.Extensions.Terminal
import Text.Terminal.VT100

wf :: String -> Workflow
wf a = workflow a "Run a process, e.g. /bin/bash" runProcess

Start :: *World -> *World
Start w = startEngine runProcess w

runProcess :: Task Int
runProcess = enterInformation "Command" [] -&&- enterInformation "Arguments" []
	>>= \(cmd, args)->runProcessInteractive zero cmd args Nothing
