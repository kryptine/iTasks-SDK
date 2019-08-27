implementation module iTasks.WF.Combinators.Tune

import StdList, StdFunctions
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Layout

import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import Data.Maybe, Data.Error, Data.Functor, Text.GenJSON, StdString
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS

class addConstantAttribute f :: !String !b !(f a) -> f a | toAttribute b
instance addConstantAttribute Task
where
	addConstantAttribute attrName value task = Task (eval task)
	where
		eval (Task task) event evalOpts iworld
			# (result,iworld) = task event evalOpts iworld
			# attrValue       = toAttribute value
			# (val, iworld)   = addAttributeChange attrName attrValue attrValue result iworld
			= (wrapTaskContinuation eval val, iworld)

class addValueAttribute f :: !String ((Maybe a) -> b) !(f a) -> f a | toAttribute b
instance addValueAttribute Task
where
	addValueAttribute attrName attrValueFun task = Task (eval (toAttribute (attrValueFun Nothing)) task)
	where
		eval curAttrValue (Task inner) DestroyEvent evalOpts iworld
			= inner DestroyEvent evalOpts iworld
		eval curAttrValue (Task inner) event evalOpts iworld
			# (result,iworld) = inner event evalOpts iworld
			# newAttrValue    = refreshAttribute result
			# (val, iworld)   = addAttributeChange attrName curAttrValue newAttrValue result iworld
			= (wrapTaskContinuation (eval newAttrValue) val, iworld)
		where
			refreshAttribute (ValueResult (Value v _) _ _ _) = toAttribute (attrValueFun (Just v))
			refreshAttribute _ = toAttribute (attrValueFun Nothing)

class addSDSAttribute f :: !String (sds () r w) (r -> b) !(f a) -> f a | toAttribute b & TC r & TC w & Registrable, Readable sds
instance addSDSAttribute Task
where
	addSDSAttribute attrName sds attrValueFun task = Task evalinit
	where
		evalinit DestroyEvent evalOpts iworld = (DestroyedResult, iworld)
		evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
			# (mbr,iworld) = 'SDS'.readRegister taskId sds iworld 
			| isError mbr = (ExceptionResult (fromError mbr),iworld)
			# v = directResult (fromOk mbr)
			= eval (toAttribute (attrValueFun v)) task event evalOpts iworld

		eval curAttrValue (Task inner) event evalOpts=:{TaskEvalOpts|taskId} iworld
			//Evaluate inner task
			# (result,iworld) = inner event evalOpts iworld
			//Check if we need to refresh the share
			# (mbNewAttrValue,iworld) = refreshAttribute taskId curAttrValue event iworld
			| isError mbNewAttrValue
				= (ExceptionResult (fromError mbNewAttrValue),iworld)
			//Add/change the value 
			# (Ok newAttrValue) = mbNewAttrValue
			# (val, iworld)   = addAttributeChange attrName curAttrValue newAttrValue result iworld
			= (wrapTaskContinuation (eval newAttrValue) val, iworld)
		where
			refreshAttribute taskId cur (RefreshEvent refreshSet _) iworld
				| 'DS'.member taskId refreshSet
					# (mbr,iworld) = 'SDS'.readRegister taskId sds iworld 
					= (fmap (toAttribute o attrValueFun o directResult) mbr,iworld)
			refreshAttribute taskId cur _ iworld
				= (Ok cur,iworld)

//Shared helper functions
addAttributeChange attrName _ new (ValueResult value info (ReplaceUI (UI type attr items)) task) iworld
	# attr = 'DM'.put attrName (JSONString new) attr
	# info = {TaskEvalInfo|info & attributes = 'DM'.put attrName new info.TaskEvalInfo.attributes}
	= (ValueResult value info (ReplaceUI (UI type attr items)) task, iworld)
addAttributeChange attrName cur new (ValueResult value info (ChangeUI attrChanges itemChanges) task) iworld
	//The constant value overrules any changes to the attribute
	# attrChanges = filter (ignoreAttribute attrName) attrChanges
	//If the annotated attribute changes, we need to set it
	# attrChanges = (if (cur <> new) [SetAttribute attrName (JSONString new)] []) ++ attrChanges
	# info = {TaskEvalInfo|info & attributes = 'DM'.put attrName new info.TaskEvalInfo.attributes}
	= (ValueResult value info (ChangeUI attrChanges itemChanges) task, iworld)
addAttributeChange attrName cur new result iworld
	= (result,iworld)

ignoreAttribute x (SetAttribute y _) = x == y
ignoreAttribute x (DelAttribute y) = x == y

class toAttribute a where toAttribute :: a -> String
instance toAttribute String where toAttribute s = s
instance toAttribute Int where toAttribute i = toString i

class tune b f :: !b !(f a) -> f a
class tunev b a f | iTask a :: !(b a) !(f a) -> f a

(<<@) infixl 2 :: !(f a) !b	-> f a | tune b f
(<<@) t a = tune a t

(@>>) infixr 2 :: !b !(f a)	-> f a | tune b f
(@>>) a t = tune a t

(<@@) infixl 2 :: !(f a) !(b a) -> f a | tunev b a f & iTask a
(<@@) t a = tunev a t

(@@>) infixr 2 :: !(b a) !(f a) -> f a | tunev b a f & iTask a
(@@>) a t = tunev a t

instance tune (ApplyAttribute a) Task | toAttribute a
where tune (ApplyAttribute k v) task = addConstantAttribute k v task

instance tune (ApplySDSAttribute a r w) Task | toAttribute a & TC r & TC w
where tune (ApplySDSAttribute k sds f) task = addSDSAttribute k sds f task

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

instance tune ApplyLayout Task
where tune (ApplyLayout l) task = applyLayout l task
