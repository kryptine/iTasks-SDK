implementation module iTasks.UI.Editor.Combinators

import iTasks.UI.Editor, iTasks.UI.Definition
import qualified Data.Map as DM

withHintAttributes :: String (Editor a) -> Editor a
withHintAttributes typeDesc editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val upd vst=:{VSt|taskId,optional}
		= case editor.Editor.genUI dp val upd vst of
			(Ok (UI type attr items,mask),vst) 
				//Add hint attributes
				# attr = 'DM'.union (stdAttributes typeDesc optional mask) attr
				= (Ok (UI type attr items,mask),vst) 
			(e,vst) = (e,vst)
	updUI dp ov om nv nm vst=:{VSt|optional,disabled}
		= case stdAttributeChanges typeDesc optional om nm of
			[] = editor.Editor.updUI dp ov om nv nm vst //Nothing to add
			hintChanges = case editor.Editor.updUI dp ov om nv nm vst of
				(Ok NoChange,vst) = (Ok (ChangeUI hintChanges []),vst)
				(Ok (ChangeUI attrChanges itemChanges),vst) = (Ok (ChangeUI (attrChanges ++ hintChanges) itemChanges),vst)
				(e,vst) = (e,vst)

	onEdit dp e val mask ust = editor.Editor.onEdit dp e val mask ust

whenDisabled :: (Editor a) (Editor a) -> Editor a
whenDisabled disabledEditor enabledEditor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val mask vst=:{VSt|taskId,disabled}
		| disabled = disabledEditor.Editor.genUI dp val mask vst
                   = enabledEditor.Editor.genUI dp val mask vst

	updUI dp ov om nv nm vst=:{VSt|optional,disabled}
		| disabled = disabledEditor.Editor.updUI dp ov om nv nm vst
		           = enabledEditor.Editor.updUI dp ov om nv nm vst
	
	onEdit dp e val mask ust
		= enabledEditor.Editor.onEdit dp e val mask ust

liftEditor :: (b -> a) (a -> b) (Editor a) -> Editor b
liftEditor tof fromf editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val upd vst = editor.Editor.genUI dp (tof val) upd vst
	updUI dp ov om nv nm vst = editor.Editor.updUI dp (tof ov) om (tof nv) nm vst
	onEdit dp e val mask ust
		# (val,mask,ust) = editor.Editor.onEdit dp e (tof val) mask ust 
		= (fromf val,mask,ust)

liftEditorAsymmetric :: (b -> a) (a -> MaybeErrorString b) (Editor a) -> Editor b
liftEditorAsymmetric tof fromf editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val upd vst = editor.Editor.genUI dp (tof val) upd vst
	updUI dp ov om nv nm vst = editor.Editor.updUI dp (tof ov) om (tof nv) nm vst

	onEdit dp e old mask ust
		# (val,mask,ust) = editor.Editor.onEdit dp e (tof old) mask ust 
		= case fromf val of
			(Ok new)  = (new,mask,ust)
			(Error e) = (old,mask,ust)

constEditor :: a (Editor a) -> (Editor a)
constEditor val editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp _ upd vst = editor.Editor.genUI dp val upd vst
	updUI dp _ _ _ _ vst = (Ok NoChange,vst)
	onEdit dp _ val mask ust = (val,mask,ust)

