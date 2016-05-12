implementation module iTasks.UI.Editor.Combinators

import iTasks.UI.Editor, iTasks.UI.Definition
import qualified Data.Map as DM

withHintAttributes :: String (Editor a) -> Editor a
withHintAttributes typeDesc editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val mask vst=:{VSt|taskId,optional}
		= case editor.Editor.genUI dp val mask vst of
			(Ok (UI type attr items),vst) 
				//Add hint attributes
				# attr = 'DM'.union (stdAttributes typeDesc optional mask) attr
				= (Ok (UI type attr items),vst) 
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

liftEditor :: (a -> b) (b -> a) (Editor a) -> (Editor b)
liftEditor tof fromf editor = {Editor|genUI=genUI,updUI=updUI,onEdit=onEdit}
where
	genUI dp val mask vst = editor.Editor.genUI dp (fromf val) mask vst
	updUI dp ov om nv nm vst = editor.Editor.updUI dp (fromf ov) om (fromf nv) nm vst
	onEdit dp e val mask ust = case editor.Editor.onEdit dp e (fromf val) mask ust of
		(val,mask,ust) = (tof val,mask,ust)

	
