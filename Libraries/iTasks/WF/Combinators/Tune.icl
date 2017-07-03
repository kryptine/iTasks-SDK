implementation module iTasks.WF.Combinators.Tune

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Layout

import iTasks._Framework.TaskState
import iTasks._Framework.TaskEval
import Text.JSON

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LayoutState, LayoutTree, MvUI, MvUIChild
derive JSONDecode LayoutState, LayoutTree, MvUI, MvUIChild

/*
* Tuning of tasks
*/
class tune b :: !b !(Task a) -> Task a
class tunev b a | iTask a :: !(b a) !(Task a) -> Task a

instance tune LazyRefresh
where
	tune _ (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld
			= case (eval event evalOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskEvalInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)

instance tune ApplyLayout
where
	tune (ApplyLayout l) task=:(Task evala) = Task eval
	where
		eval event evalOpts (TCDestroy (TCLayout s tt)) iworld //Cleanup duty simply passed to inner task
			= evala event evalOpts (TCDestroy tt) iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			= eval ResetEvent evalOpts (TCLayout JSONNull tt) iworld //On initialization, we need to do a reset to be able to apply the layout

		//On Reset events, we (re-)apply the layout
		eval ResetEvent evalOpts (TCLayout _ tt) iworld = case evala ResetEvent evalOpts tt iworld of
			(ValueResult value info (ReplaceUI ui) tt,iworld)
				//Determine the change the layout makes to the UI
				# (change,state) = l.Layout.apply ui
				//Modify the layout accorgingly
				# ui = applyUIChange change ui
				= (ValueResult value info (ReplaceUI ui) (TCLayout (toJSON state) tt), iworld)		
            (res,iworld) = (res,iworld)

		eval event evalOpts (TCLayout json tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				= case fromJSON json of
					(Just s)	
						# (change,s) = l.Layout.adjust (change,s)
						= (ValueResult value info change (TCLayout (toJSON s) tt), iworld)
					Nothing	
						= (ExceptionResult (exception ("Corrupt layout state:" +++ toString json)), iworld)
            (res,iworld) = (res,iworld)
		
		eval event evalOpts state iworld = evala event evalOpts state iworld //Catchall


