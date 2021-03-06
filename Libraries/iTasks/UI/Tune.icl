implementation module iTasks.UI.Tune

import StdList, StdTuple, StdFunctions
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Layout

import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.Task
import Data.Maybe, Data.Error, Data.Functor, Text.GenJSON, StdString
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS

(<<@) infixl 2 :: !tunedItem !option -> tunedItem | tune option tunedItem
(<<@) t a = tune a t

(@>>) infixr 2 :: !option !tunedItem -> tunedItem | tune option tunedItem
(@>>) a t = tune a t

instance tune UIAttribute (Editor a)
where
	tune (k,v) editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
	where
		genUI attr dp mode vst = editorGenUI ('DM'.put k v attr) dp (mapEditMode id mode) vst

instance tune UIAttributes (Editor a)
where
	tune extra editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
	where
		genUI attr dp mode vst = editorGenUI ('DM'.union extra attr) dp (mapEditMode id mode) vst

instance tune UIAttribute (Task a)
where
	tune attr task = tune ('DM'.fromList [attr]) task

instance tune UIAttributes (Task a)
where
	tune attrs task = Task (eval task)
	where
		eval (Task inner) event evalOpts iworld
			# (result,iworld) = inner event evalOpts iworld
			= (wrapTaskContinuation eval (withExtraAttributes attrs result), iworld) 
	
		withExtraAttributes extra (ValueResult value info (ReplaceUI (UI type attr items)) task)
			# attr = 'DM'.union extra attr
			= ValueResult value info (ReplaceUI (UI type attr items)) task
		withExtraAttributes extra (ValueResult value info (ChangeUI attrChanges itemChanges) task)
			//The constant value overrules any changes to the attribute
			# attrChanges = filter (not o ignoreAttributes ('DM'.keys extra)) attrChanges
			= ValueResult value info (ChangeUI attrChanges itemChanges) task
		where
			ignoreAttributes keys (SetAttribute k _) = isMember k keys
			ignoreAttributes keys (DelAttribute k) = isMember k keys

		withExtraAttributes extra result = result

instance tune Title (Task a)
where tune (Title title) t = tune (titleAttr title) t
instance tune Title (Editor a)
where tune (Title title) e = tune (titleAttr title) e

instance tune Hint (Task a)
where tune (Hint hint) t = tune (hintAttr hint) t
instance tune Hint (Editor a)
where tune (Hint hint) e = tune (hintAttr hint) e

instance tune Icon (Task a)
where tune (Icon icon) t = tune ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]) t
instance tune Icon (Editor a)
where tune (Icon icon) e = tune ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]) e

instance tune Label (Task a)
where tune (Label label) t = tune ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]) t
instance tune Label (Editor a)
where tune (Label label) e = tune ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]) e

instance tune ApplyLayout (Task a)
where tune (ApplyLayout l) task = applyLayout l task

applyLayout :: LayoutRule (Task a) -> Task a
applyLayout rule task = Task evalinit
where
	ruleNo = LUINo [0]

	evalinit event = eval (initLUI (ui UIEmpty), initLUIMoves) task ResetEvent

	//Cleanup duty simply passed to inner task
	eval _ (Task inner) DestroyEvent evalOpts iworld
		= inner DestroyEvent evalOpts iworld
	//On Reset events, we (re-)apply the layout
	eval state (Task inner) ResetEvent evalOpts iworld
		= case inner ResetEvent evalOpts iworld of
			(ValueResult value info (ReplaceUI ui) task,iworld)
				# (change,state) = extractResetChange (rule ruleNo (initLUI ui, initLUIMoves))
				= (wrapTaskContinuation (eval state) (ValueResult value info change task), iworld)
			(val, iworld) = (wrapTaskContinuation (eval state) val, iworld)

	eval state (Task inner) event evalOpts iworld
		= case inner event evalOpts iworld of
			(ValueResult value info change task,iworld)
				# state = applyUpstreamChange change state
				# state = rule ruleNo state
				# (change,state) = extractDownstreamChange state
				= (wrapTaskContinuation (eval state) (ValueResult value info change task), iworld)
			(val, iworld) = (wrapTaskContinuation (eval state) val, iworld)
