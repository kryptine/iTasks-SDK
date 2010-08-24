implementation module GUI

import iTasks, CommonDomain

derive class iTask	WizardAction
derive bimap		Maybe, (,)

wizard :: !question ![WizardStep state] !state -> Task (Maybe state) | html question & iTask state & SharedVariable state
wizard question [] initSt = return (Just initSt)
wizard question views initSt =
					createDB initSt
	>>= \sid.		wizard` [] views GotoNext sid			
where
	wizard` prev [current:next] previousAction sid = case current of
		ViewOnState instr views =
								updateShared question actions sid [listener {listenerFrom = \_ -> (Note instr)}:views]
			>>= \(action,_).	case action of
									ActionNext = wizard` [current:prev] next GotoNext sid
									ActionPrevious
										# [newCur:newPrev] = prev
										= wizard` newPrev [newCur,current:next] GotoPrevious sid
									ActionFinish	= getResult sid
									ActionCancel	= return Nothing
		CustomTask task =
								readDB sid
			>>= \state.			task state previousAction
			>>= \(state,act).	writeDB sid state
			>>|					case act of
									GotoNext
										| isEmpty next	= getResult sid				
										| otherwise		= wizard` [current:prev] next GotoNext sid
									GotoPrevious
										| isEmpty prev	= wizard` prev [current:next] GotoPrevious sid
										| otherwise
											# [newCur:newPrev] = prev
											= wizard` newPrev [newCur,current:next] GotoPrevious sid
	where
		actions
			| isEmpty next		= [cancelAction, prevAction, finAction]
			| isEmpty prev		= [cancelAction, nextAction]
			| otherwise			= [cancelAction, prevAction, nextAction]
		nextAction				= ButtonAction (ActionNext,		IfValid)
		prevAction				= ButtonAction (ActionPrevious,	Always)
		finAction				= ButtonAction (ActionFinish,	IfValid)
		cancelAction			= ButtonAction (ActionCancel,	Always)
		getResult sid =
							readDB sid
			>>= \result.	deleteDB sid
			>>|				return (Just result)
			
editOptions :: !description !state !(state -> opts) !(opts state -> state) -> Task state | html description & iTask state & iTask opts
editOptions description st getOpts putbackOpts =
						ExcludeGroupActions @>>
						updateInformationA "Edit options" description [ButtonAction (ActionCancel, Always), ButtonAction (ActionOk, IfValid)] (getOpts st)
	>>= \(action,opts).	case action of
							ActionOk		= return (putbackOpts opts st)
							ActionCancel	= return st