implementation module iTasks.UI.Editor.Modifiers

import StdBool
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.Tune
import Data.Error, Text.JSON
import GenEq
import qualified Data.Map as DM

withAttributes :: UIAttributes (Editor a) -> Editor a
withAttributes extra editor = {Editor|editor & genUI = genUI}
where
	genUI dp val vst=:{VSt|taskId,optional}
		= case editor.Editor.genUI dp val vst of
			(Ok (UI type attr items,mask),vst) = (Ok (UI type ('DM'.union attr extra) items,mask),vst) 
			(e,vst) = (e,vst)

instance tune UIAttributes Editor
where
	tune attr editor = withAttributes attr editor

withLabelAttr :: String (Editor a) -> Editor a
withLabelAttr label editor = withAttributes (labelAttr label) editor

withEditModeAttr :: (Editor a) -> Editor a
withEditModeAttr editor = {Editor|editor & genUI = genUI}
where
	genUI dp val vst=:{VSt|taskId,mode}
		= case editor.Editor.genUI dp val vst of
			(Ok (UI type attr items,mask),vst) = (Ok (UI type ('DM'.put "mode" (JSONString (toString mode)) attr) items, mask),vst) 
			(e,vst) = (e,vst)

withDynamicHintAttributes :: String (Editor a) -> Editor a
withDynamicHintAttributes typeDesc editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
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
/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: String Bool EditMask -> UIAttributes
stdAttributes typename optional (CompoundMask _) = 'DM'.newMap
stdAttributes typename optional mask
	# (touched,valid,state) = case mask of
		(FieldMask {FieldMask|touched,valid,state}) = (touched,valid,state)
		mask = (isTouched mask,True,JSONNull)
	| state =:JSONNull && not touched
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INFO)
                        ,(HINT_ATTRIBUTE,JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)"))]
	| state =: JSONNull 
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("You need to enter a "+++ typename +++ " (this value is required)"))]
	| valid
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_VALID)
						,(HINT_ATTRIBUTE,JSONString ("You have correctly entered a " +++ typename))]
	| otherwise
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE,JSONString HINT_TYPE_INVALID)
						,(HINT_ATTRIBUTE,JSONString ("This value not in the required format of a " +++ typename))]

stdAttributeChanges :: String Bool EditMask EditMask -> [UIAttributeChange]
stdAttributeChanges typename optional om nm 
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename optional nm)]

selectByMode :: (Editor a) (Editor a) (Editor a) -> Editor a
selectByMode viewEditor enterEditor updateEditor= {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode} = case mode of
		View = viewEditor.Editor.genUI dp val vst
		Enter = enterEditor.Editor.genUI dp val vst
		Update = updateEditor.Editor.genUI dp val vst

	onEdit dp e val mask vst=:{VSt|mode} = case mode of
		View = viewEditor.Editor.onEdit dp e val mask vst
		Enter = enterEditor.Editor.onEdit dp e val mask vst
		Update = updateEditor.Editor.onEdit dp e val mask vst

	onRefresh dp new old mask vst=:{VSt|mode} = case mode of
		View = viewEditor.Editor.onRefresh dp new old mask vst
		Enter = enterEditor.Editor.onRefresh dp new old mask vst
		Update = updateEditor.Editor.onRefresh dp new old mask vst

withEditMode :: EditMode (Editor a) -> Editor a
withEditMode newMode editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|mode}
		# (res,vst) = editor.Editor.genUI dp val {VSt|vst & mode = newMode}
		= (res,{VSt|vst & mode=mode})

	onEdit dp e val mask vst=:{VSt|mode}
		# (mask,val,vst) = editor.Editor.onEdit dp e val mask {VSt|vst & mode = newMode}
		= (mask,val,{VSt|vst & mode=mode})

	onRefresh dp new old mask vst=:{VSt|mode} 
		# (change,val,vst) = editor.Editor.onRefresh dp new old mask {VSt|vst & mode = newMode}
		= (change,val,{VSt|vst & mode=mode})

bijectEditorValue :: (b -> a) (a -> b) (Editor a) -> Editor b
bijectEditorValue tof fromf editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst
	onEdit dp e val mask vst
		# (mask,val,vst) = editor.Editor.onEdit dp e (tof val) mask vst 
		= (mask,fromf val,vst)
	onRefresh dp new old mask vst
		# (change,val,vst) = editor.Editor.onRefresh dp (tof new) (tof old) mask vst
		= (change,fromf val,vst)

surjectEditorValue :: (b -> a) (a -> MaybeErrorString b) (Editor a) -> Editor b
surjectEditorValue tof fromf editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst

	onEdit dp e old mask vst
		# (mask,val,vst) = editor.Editor.onEdit dp e (tof old) mask vst 
		= case fromf val of
			(Ok new)  = (mask,new,vst)
			(Error e) = case mask of
				(Ok (change,FieldMask mask))
					# attrChange = ChangeUI [SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_INVALID)
											,SetAttribute HINT_ATTRIBUTE (JSONString e)] []
					# change = mergeUIChanges change attrChange
					= (Ok (change,FieldMask {FieldMask|mask & valid = False}),old,vst)	
					
				_ = (mask,old,vst)

	onRefresh dp new old mask vst 
		# (change,val,vst) = editor.Editor.onRefresh dp (tof new) (tof old) mask vst
		= case fromf val of 
			(Ok new)  = (change,new,vst)
			(Error e) = (change,old,vst)

comapEditorValue :: (b -> a) (Editor a) -> (Editor b)
comapEditorValue tof editor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst = editor.Editor.genUI dp (tof val) vst

	onEdit dp _ val mask vst = (Ok (NoChange,mask),val,vst) //Ignore edits

	onRefresh dp new old mask vst 
		# (change,val,vst) = editor.Editor.onRefresh dp (tof new) (tof old) mask vst
		= (change, if (change =: Error _) old new,vst)