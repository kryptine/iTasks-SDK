implementation module BasicAPIExamples.InteractionWithTheSystem.RunProcess

import iTasks
import iTasks.Extensions.Terminal
import Text.Terminal.VT100

wf :: String -> Workflow
wf a = workflow a "Run a process, e.g. /bin/bash" runProcess

main :: Task ()
main = runProcess @! ()

runProcess :: Task Int
runProcess = enterInformation [EnterWithHint "Command"] -&&- enterInformation [EnterWithHint "Arguments"]
	>>= \(cmd, args)->runProcessInteractive zero cmd args Nothing
