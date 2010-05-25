definition module GUI

import iTasks

:: WizardStep state = ViewOnState !String ![View state] | CustomTask (state WizardAction -> Task (state,WizardAction))
:: WizardAction = GotoNext | GotoPrevious

derive gPrint		WizardAction
derive gParse		WizardAction
derive gUpdate		WizardAction
derive gVisualize	WizardAction
derive gHint		WizardAction
derive gError		WizardAction

wizard :: !description ![WizardStep state] !state -> Task (Maybe state) | html description & iTask state & SharedVariable state

editOptions :: !description !state !(state -> opts) !(opts state -> state) -> Task state | html description & iTask state & iTask opts