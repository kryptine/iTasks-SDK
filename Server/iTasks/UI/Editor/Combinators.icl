implementation module iTasks.UI.Editor.Combinators

import iTasks.UI.Editor, iTasks.UI.Definition
import Data.Error, Text.JSON
import qualified Data.Map as DM

withAttributes :: UIAttributes (Editor a) -> Editor a
withAttributes extra editor = {Editor|editor & genUI = genUI}
where
	genUI dp val vst=:{VSt|taskId,optional}
		= case editor.Editor.genUI dp val vst of
			(Ok (UI type attr items,mask),vst) = (Ok (UI type ('DM'.union attr extra) items,mask),vst) 
			(e,vst) = (e,vst)

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

	onEdit dp e oval omask vst=:{VSt|optional}
		= addHintAttrChanges omask (editor.Editor.onEdit dp e oval omask vst)
	onRefresh dp e oval omask vst=:{VSt|optional}
		= addHintAttrChanges omask (editor.Editor.onRefresh dp e oval omask vst)

	addHintAttrChanges omask (Ok (change,nmask),nval,vst=:{VSt|optional})
		# attrChange = case stdAttributeChanges typeDesc optional omask nmask of
			[] = NoChange
			cs = ChangeUI cs []
		# change = mergeUIChanges change attrChange
		= (Ok (change,nmask),nval,vst)
	addHintAttrChanges omask (e,val,vst) = (e,val,vst)

withLabel :: String (Editor a) -> Editor a
withLabel label editor = withAttributes (labelAttr label) editor

whenDisabled :: (Editor a) (Editor a) -> Editor a
whenDisabled disabledEditor enabledEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode}
		| mode =: View = disabledEditor.Editor.genUI dp val vst
                       = enabledEditor.Editor.genUI dp val vst

	onEdit dp e val mask vst
		= enabledEditor.Editor.onEdit dp e val mask vst

	onRefresh dp new old mask vst=:{VSt|mode}
		| mode =: View = disabledEditor.Editor.onRefresh dp new old mask vst
		               = enabledEditor.Editor.onRefresh dp new old mask vst
	
liftEditor :: (b -> a) (a -> b) (Editor a) -> Editor b
liftEditor tof fromf editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst
	onEdit dp e val mask vst
		# (mask,val,vst) = editor.Editor.onEdit dp e (tof val) mask vst 
		= (mask,fromf val,vst)
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

composeEditors :: UINodeType (Editor a) (Editor b) -> Editor (a,b)
composeEditors type ex ey = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp (x,y) vst
		# (vizx, vst)	= ex.Editor.genUI (dp ++ [0]) x vst
		| vizx =: (Error _) = (vizx,vst)
		# (vizy, vst)	= ey.Editor.genUI (dp ++ [1]) y vst
		| vizy =: (Error _) = (vizy,vst)
		# ((vizx,maskx),(vizy,masky)) = (fromOk vizx,fromOk vizy)
		= (Ok (uic type [vizx,vizy],CompoundMask {fields=[maskx,masky],state=JSONNull}),vst)

	onEdit dp ([0:ds],e) (x,y) (CompoundMask {fields=[xmask,ymask],state}) vst
		= case  ex.Editor.onEdit (dp ++ [0]) (ds,e) x xmask vst of
			(Ok (xchange,xmask),x,vst)
				= (Ok (ChangeUI [] [(0,ChangeChild xchange)],CompoundMask {fields=[xmask,ymask],state=state}),(x,y),vst)
			(Error e,x,vst) = (Error e,(x,y),vst)
	onEdit dp ([1:ds],e) (x,y) (CompoundMask {fields=[xmask,ymask],state}) vst
		= case  ey.Editor.onEdit (dp ++ [1]) (ds,e) y ymask vst of
			(Ok (ychange,ymask),y,vst)
				= (Ok (ChangeUI [] [(1,ChangeChild ychange)],CompoundMask {fields=[xmask,ymask],state=state}),(x,y),vst)
			(Error e,y,vst) = (Error e,(x,y),vst)
	onEdit _ _ val mask vst = (Ok (NoChange,mask),val,vst)

	onRefresh dp (newx,newy) (oldx,oldy) (CompoundMask {fields=[maskx,masky],state}) vst
		# (changex,newx,vst) 	= ex.Editor.onRefresh (dp ++ [0]) newx oldx maskx vst
		| changex=: (Error _) = (changex,(oldx,oldy),vst)
		# (changey,newy,vst) 	= ey.Editor.onRefresh (dp ++ [1]) newy oldy masky vst
		| changey =: (Error _) = (changey,(oldx,oldy),vst)
		# ((changex,maskx),(changey,masky)) = (fromOk changex,fromOk changey)
		= (Ok (ChangeUI [] [(0,ChangeChild changex),(1,ChangeChild changey)],CompoundMask {fields=[maskx,masky],state=state}),(newx,newy), vst)
