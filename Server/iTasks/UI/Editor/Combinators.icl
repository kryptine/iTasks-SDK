implementation module iTasks.UI.Editor.Combinators

import iTasks.UI.Editor, iTasks.UI.Definition
import Data.Error, Text.JSON
import qualified Data.Map as DM

withHintAttributes :: String (Editor a) -> Editor a
withHintAttributes typeDesc editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|taskId,optional}
		= case editor.Editor.genUI dp val vst of
			(Ok (UI type attr items,mask),vst) 
				//Add hint attributes
				# attr = 'DM'.union (stdAttributes typeDesc optional mask) attr
				= (Ok (UI type attr items,mask),vst) 
			(e,vst) = (e,vst)

	onEdit dp e val mask ust = editor.Editor.onEdit dp e val mask ust

	onRefresh dp new old mask vst=:{VSt|optional}
		= case stdAttributeChanges typeDesc optional mask mask of
			[] = editor.Editor.onRefresh dp new old mask vst //Nothing to add
			hintChanges = case editor.Editor.onRefresh dp new old mask vst of
				(Ok (NoChange,mask),new,vst) = (Ok (ChangeUI hintChanges [],mask),new,vst)
				(Ok (ChangeUI attrChanges itemChanges,mask),new,vst) = (Ok (ChangeUI (attrChanges ++ hintChanges) itemChanges,mask),new,vst)
				(e,val,vst) = (e,val,vst)

whenDisabled :: (Editor a) (Editor a) -> Editor a
whenDisabled disabledEditor enabledEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode}
		| mode =: View = disabledEditor.Editor.genUI dp val vst
                       = enabledEditor.Editor.genUI dp val vst

	onEdit dp e val mask ust
		= enabledEditor.Editor.onEdit dp e val mask ust

	onRefresh dp new old mask vst=:{VSt|mode}
		| mode =: View = disabledEditor.Editor.onRefresh dp new old mask vst
		               = enabledEditor.Editor.onRefresh dp new old mask vst
	
liftEditor :: (b -> a) (a -> b) (Editor a) -> Editor b
liftEditor tof fromf editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst
	onEdit dp e val mask ust
		# (mask,val,ust) = editor.Editor.onEdit dp e (tof val) mask ust 
		= (mask,fromf val,ust)
	onRefresh dp new old mask vst
		# (change,val,vst) = editor.Editor.onRefresh dp (tof new) (tof old) mask vst
		= (change,fromf val,vst)

liftEditorAsymmetric :: (b -> a) (a -> MaybeErrorString b) (Editor a) -> Editor b
liftEditorAsymmetric tof fromf editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst

	onEdit dp e old mask vst
		# (mask,val,vst) = editor.Editor.onEdit dp e (tof old) mask vst 
		= case fromf val of
			(Ok new)  = (mask,new,vst)
			(Error e) = (mask,old,vst)

	onRefresh dp new old mask vst 
		# (change,val,vst) = editor.Editor.onRefresh dp (tof new) (tof old) mask vst
		= case fromf val of 
			(Ok new)  = (change,new,vst)
			(Error e) = (change,old,vst)

constEditor :: a (Editor a) -> (Editor a)
constEditor val editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp _ vst = editor.Editor.genUI dp val vst
	onEdit dp _ val mask vst = (Ok (NoChange,mask),val,vst)
	onRefresh dp _ val mask vst = (Ok (NoChange,mask),val,vst)

