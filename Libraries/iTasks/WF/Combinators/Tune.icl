implementation module iTasks.WF.Combinators.Tune

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Tune
import iTasks.UI.Layout

import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import Data.Maybe, Text.GenJSON, StdString
import qualified Data.Map as DM

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set
derive JSONDecode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set

/*
* Tuning of tasks
*/
instance tune LazyRefresh Task
where
	tune _ (Task eval) = Task eval`
	where
		eval` event evalOpts state iworld
			= case (eval event evalOpts state iworld) of
				(ValueResult value info rep tree,iworld) = (ValueResult value {TaskEvalInfo|info&refreshSensitive=False} rep tree, iworld)
				(res,iworld) = (res,iworld)

instance tune ApplyLayout Task
where
	tune (ApplyLayout l) task = applyLayout l task

applyLayout :: LayoutRule (Task a) -> Task a
applyLayout rule task=:(Task evala) = Task eval
	where
		ruleNo = LUINo [0]

		eval event evalOpts (TCDestroy (TCLayout s tt)) iworld //Cleanup duty simply passed to inner task
			= evala event evalOpts (TCDestroy tt) iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			= eval ResetEvent evalOpts (TCLayout JSONNull tt) iworld //On initialization, we need to do a reset to be able to apply the layout

		//On Reset events, we (re-)apply the layout
		eval ResetEvent evalOpts (TCLayout _ tt) iworld = case evala ResetEvent evalOpts tt iworld of
			(ValueResult value info (ReplaceUI ui) tt,iworld)
				# (change,state) = extractResetChange (rule ruleNo (initLUI ui, initLUIMoves))
				//| not (trace_tn ("STATE AFTER RESET: \n"+++toString (toJSON state))) = undef
				= (ValueResult value info change (TCLayout (toJSON state) tt), iworld)		
            (res,iworld) = (res,iworld)

		eval event evalOpts (TCLayout json tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				= case fromJSON json of
					(Just state1)	
						//| not (trace_tn ("UPSTREAM CHANGE: \n"+++toString (toJSON change))) = undef
						//| not (trace_tn ("STATE BEFORE CHANGE: \n"+++toString (toJSON state1))) = undef
						# state2 = applyUpstreamChange change state1
						//| not (trace_tn ("STATE AFTER CHANGE: \n"+++toString (toJSON state2))) = undef
						# state3 = rule ruleNo state2
						//| not (trace_tn ("STATE AFTER RULES: \n"+++toString (toJSON state3))) = undef
						# (change,state4) = extractDownstreamChange state3
						//| not (trace_tn ("STATE AFTER EXTRACT: \n"+++toString (toJSON state4))) = undef
						//| not (trace_tn ("DOWNSTREAM CHANGE: \n"+++toString (toJSON change))) = undef
						//| not (trace_tn "=====") = undef
						| not (fullyApplied_ state4)
							# iworld = traceState "state-before-change.txt" state1 iworld
							# iworld = traceState "state-after-change.txt" state2 iworld
							# iworld = traceState "state-after-rules.txt" state3 iworld
							# iworld = traceState "state-after-extract.txt" state4 iworld
							= (ExceptionResult (exception ("Corrupt layout state")), iworld)
						= (ValueResult value info change (TCLayout (toJSON state4) tt), iworld)
					Nothing	
						= (ExceptionResult (exception ("Corrupt layout state:" +++ toString json)), iworld)
            (res,iworld) = (res,iworld)
		
		eval event evalOpts state iworld = evala event evalOpts state iworld //Catchall

traceState filename state iworld
	# {IWorld|world} = iworld
	# (_,world) = writeFile filename (jsonPrettyPrint (toJSON state)) world
	= {IWorld|iworld & world = world}

import StdDebug, StdMisc, System.File, StdFile
import iTasks.Internal.IWorld

class toAttribute a where toAttribute :: a -> JSONNode
instance toAttribute String where toAttribute s = JSONString s

instance tune (ApplyAttribute a) Task | toAttribute a
where
	tune (ApplyAttribute k v) task = tune (ApplyLayout (setUIAttributes ('DM'.fromList [(k,toAttribute v)]))) task

