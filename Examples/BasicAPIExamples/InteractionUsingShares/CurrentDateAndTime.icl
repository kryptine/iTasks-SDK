implementation module BasicAPIExamples.InteractionUsingShares.CurrentDateAndTime

// Just show the current date and time which are offered in a share
// Next the current time is displayed in the view of an analog clock

import iTasks
import iTasks.Extensions.DateTime
import iTasks.Extensions.Clock

wf :: String -> Workflow
wf a = workflow a "View the current Date and Time" showDateAndTime

main :: Task ()
main = showDateAndTime @! ()

showDateAndTime :: Task Time
showDateAndTime
	= 	viewSharedInformation [ViewWithHint "The current Date and Time is:"] currentDateTime
	>>| viewSharedInformation [ViewWithHint "The current time is:", ViewAs AnalogClock] currentTime
