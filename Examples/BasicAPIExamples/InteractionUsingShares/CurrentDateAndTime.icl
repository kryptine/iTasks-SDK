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
	= 	Hint "The current Date and Time is:" @>> viewSharedInformation [] currentDateTime
	>!| Hint "The current time is:" @>> viewSharedInformation [ViewAs AnalogClock] currentTime
