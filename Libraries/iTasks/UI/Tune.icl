implementation module iTasks.UI.Tune

import StdList, StdTuple, StdFunctions
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Layout

import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import Data.Maybe, Data.Error, Data.Functor, Text.GenJSON, StdString
import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS

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

instance tune UIAttribute Editor
where
	tune (k,v) editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
	where
		genUI attr dp mode vst = editorGenUI ('DM'.put k v attr) dp (mapEditMode id mode) vst

instance tune UIAttributes Editor
where
	tune extra editor=:{Editor|genUI=editorGenUI} = {Editor|editor & genUI = genUI}
	where
		genUI attr dp mode vst = editorGenUI ('DM'.union attr extra) dp (mapEditMode id mode) vst

instance tune UIAttribute Task
where
	tune attr task = tune ('DM'.fromList [attr]) task

instance tune UIAttributes Task
where
	tune attrs task=:(Task evala) = Task eval
	where
		eval event evalOpts tree iworld
			# (result,iworld) = evala event evalOpts tree iworld
			= (withExtraAttributes attrs result, iworld) 
	
		withExtraAttributes extra (ValueResult value info (ReplaceUI (UI type attr items)) tree)
			# attr = 'DM'.union extra attr
			= ValueResult value info (ReplaceUI (UI type attr items)) tree
		withExtraAttributes extra (ValueResult value info (ChangeUI attrChanges itemChanges) tree)
			//The constant value overrules any changes to the attribute
			# attrChanges = filter (ignoreAttributes ('DM'.keys extra)) attrChanges
			= ValueResult value info (ChangeUI attrChanges itemChanges) tree
		where
			ignoreAttributes keys (SetAttribute k _) = isMember k keys
			ignoreAttributes keys (DelAttribute k) = isMember k keys

		withExtraAttributes extra result = result

instance tune Title Task
where tune (Title title) t = tune (titleAttr title) t
instance tune Title Editor
where tune (Title title) e = tune (titleAttr title) e

instance tune Hint Task
where tune (Hint hint) t = tune (hintAttr hint) t
instance tune Hint Editor
where tune (Hint hint) e = tune (hintAttr hint) e
       
instance tune Icon Task
where tune (Icon icon) t = tune ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]) t
instance tune Icon Editor
where tune (Icon icon) e = tune ('DM'.fromList [(ICON_ATTRIBUTE,JSONString icon)]) e

instance tune Label Task
where tune (Label label) t = tune ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]) t
instance tune Label Editor
where tune (Label label) e = tune ('DM'.fromList [(LABEL_ATTRIBUTE,JSONString label)]) e

instance tune ApplyLayout Task
where
	tune (ApplyLayout rule) task=:(Task evala) = Task eval
	where
		ruleNo = LUINo [0]

		eval DestroyEvent evalOpts (TCLayout s tt) iworld //Cleanup duty simply passed to inner task
			= evala DestroyEvent evalOpts tt iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			//On initialization, we need to do a reset to be able to apply the layout
			= eval ResetEvent evalOpts (TCLayout (initLUI (ui UIEmpty),initLUIMoves) tt) iworld 

		//On Reset events, we (re-)apply the layout
		eval ResetEvent evalOpts (TCLayout _ tt) iworld = case evala ResetEvent evalOpts tt iworld of
			(ValueResult value info (ReplaceUI ui) tt,iworld)
				# (change,state) = extractResetChange (rule ruleNo (initLUI ui, initLUIMoves))
				= (ValueResult value info change (TCLayout state tt), iworld)		
            (res,iworld) = (res,iworld)

		eval event evalOpts (TCLayout state tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				# state = applyUpstreamChange change state
				# state = rule ruleNo state
				# (change,state) = extractDownstreamChange state
				= (ValueResult value info change (TCLayout state tt), iworld)
            (res,iworld) = (res,iworld)
		
		eval event evalOpts state iworld = evala event evalOpts state iworld //Catchall
