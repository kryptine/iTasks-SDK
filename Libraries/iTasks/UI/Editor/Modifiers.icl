implementation module iTasks.UI.Editor.Modifiers

from StdFunc import o, const, flip, id
import StdBool, StdString, StdList
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.Tune
import Data.Error, Text.GenJSON, Data.Tuple, Data.Functor, Data.Maybe
import Data.GenEq, Data.Func, Data.List
import qualified Data.Map as DM

withEditModeAttr :: !(Editor r w) -> Editor r w
withEditModeAttr editor=:{Editor|onReset=editorOnReset} = {Editor|editor & onReset = onReset}
where
	onReset attr dp mode vst=:{VSt|taskId} = case editorOnReset attr dp (mapEditMode id mode) vst of
		(Ok (UI type attr items,mask,mbw),vst) = (Ok (UI type ('DM'.put "mode" (JSONString (modeString mode)) attr) items, mask, mbw),vst)
		(e,vst) = (e,vst)
	where
		modeString Enter      = "enter"
		modeString (Update _) = "update"
		modeString (View _)   = "view"

withDynamicHintAttributes :: !String !(Editor r w) -> Editor r w
withDynamicHintAttributes typeDesc editor=:{Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh, valueFromState}
	= {Editor| editor & onReset=onReset,onEdit=onEdit,onRefresh=onRefresh}
where
	onReset attr dp mode vst=:{VSt|taskId,optional} = case editorOnReset attr dp (mapEditMode id mode) vst of
		(Ok (UI type attr items,mask,mbw),vst)
			//Add hint attributes
			# attr = 'DM'.union (stdAttributes typeDesc (isJust $ valueFromState mask) optional mask) attr
			= (Ok (UI type attr items,mask,mbw),vst)
		(e,vst) = (e,vst)

	onEdit dp e omask vst=:{VSt|optional} = addHintAttrChanges omask (editorOnEdit dp e omask vst)
	onRefresh dp e omask vst=:{VSt|optional} = addHintAttrChanges omask (editorOnRefresh dp e omask vst)

	addHintAttrChanges omask (Ok (change,nmask,mbwrite),vst=:{VSt|optional})
		# attrChange = case stdAttributeChanges typeDesc optional omask (isJust $ valueFromState nmask) nmask of
			[] = NoChange
			cs = ChangeUI cs []
		# change = mergeUIChanges change attrChange
		= (Ok (change,nmask,mbwrite),vst)
	addHintAttrChanges omask (e,vst) = (e,vst)

/**
* Set basic hint and error information based on the verification
*/
stdAttributes :: !String !Bool !Bool !EditState -> UIAttributes
stdAttributes typename valid optional mask
	| valid
		= 'DM'.fromList [(HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_VALID)
						,(HINT_ATTRIBUTE, JSONString ("You have correctly entered a " +++ typename))]
	| otherwise
		| not $ isTouched mask = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INFO)
			, ( HINT_ATTRIBUTE
			  , JSONString ("Please enter a " +++ typename +++ if optional "" " (this value is required)")
			  )
			]
		| isCompound mask = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INVALID)
			, (HINT_ATTRIBUTE, JSONString ("You need to enter a "+++ typename +++ " (this value is required)"))
			]
		| otherwise = 'DM'.fromList
			[ (HINT_TYPE_ATTRIBUTE, JSONString HINT_TYPE_INVALID)
			, (HINT_ATTRIBUTE, JSONString ("This value not in the required format of a " +++ typename))
			]

stdAttributeChanges :: !String !Bool !EditState !Bool !EditState -> [UIAttributeChange]
stdAttributeChanges typename optional om nvalid nm
	| om === nm = [] //Nothing to change
	| otherwise = [SetAttribute k v \\ (k,v) <- 'DM'.toList (stdAttributes typename nvalid optional nm)]

:: StoredMode = StoredEnter | StoredUpdate | StoredView

derive JSONEncode StoredMode
derive JSONDecode StoredMode

selectByMode :: !(Editor r w) !(Editor r w) !(Editor r w) -> Editor r w
selectByMode
		viewEditor  =:{Editor|onReset=viewOnReset, onEdit=viewOnEdit, onRefresh=viewOnRefresh, valueFromState=viewValueFromState}
		enterEditor =:{Editor|onReset=enterOnReset, onEdit=enterOnEdit, onRefresh=enterOnRefresh, valueFromState=enterValueFromState}
		updateEditor=:{Editor|onReset=updateOnReset, onEdit=updateOnEdit,onRefresh=updateOnRefresh, valueFromState=updateValueFromState}
	= editorModifierWithStateToEditor
		{EditorModifierWithState|onReset=onReset,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst = attachMode storedMode $ case mode of
		View _   = viewOnReset   attr dp mode` vst
		Enter    = enterOnReset  attr dp mode` vst
		Update _ = updateOnReset attr dp mode` vst
	where
		mode` = mapEditMode id mode

		storedMode = case mode of
			Enter    = StoredEnter
			Update _ = StoredUpdate
			View _   = StoredView

	onEdit dp e mode st vst = attachMode mode $ case mode of
		StoredView   = viewOnEdit dp e st vst
		StoredEnter  = enterOnEdit dp e st vst
		StoredUpdate = updateOnEdit dp e st vst

	onRefresh dp new mode st vst = attachMode mode $ case mode of
		StoredView   = viewOnRefresh dp new st vst
		StoredEnter  = enterOnRefresh dp new st vst
		StoredUpdate = updateOnRefresh dp new st vst

	valueFromState mode st = case mode of
		StoredView   = viewValueFromState st
		StoredEnter  = enterValueFromState st
		StoredUpdate = updateValueFromState st

	attachMode mode (res, vst) = ((\(x, st, mbw) -> (x, mode, st, mbw)) <$> res, vst)

withChangedEditMode :: !((EditMode r) -> EditMode r) !(Editor r w) -> Editor r w
withChangedEditMode toNewMode editor=:{Editor|onReset=editorOnReset} = {Editor| editor & onReset = onReset}
where
	onReset attr dp mode vst = editorOnReset attr dp (mapEditMode id $ toNewMode mode) vst

viewConstantValue :: !r !(Editor r w) -> Editor () w
viewConstantValue val e = bijectEditorValue (const val) (const ()) $ withChangedEditMode (const $ View val) e

ignoreEditorWrites :: !(Editor ra wb) -> Editor ra wa
ignoreEditorWrites editor=:{Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh}
	= {Editor|editor & onReset=onReset, onEdit=onEdit, onRefresh = onRefresh}
where
	onReset attr dp mode vst = case editorOnReset attr dp (mapEditMode id mode) vst of
		(Ok (ui,st,_),vst) = (Ok (ui,st,Nothing),vst)
		(Error e,vst) = (Error e,vst)
	onEdit dp e st vst = case editorOnEdit dp e st vst of
		(Ok (ui,st,_),vst) = (Ok (ui,st,Nothing),vst)
		(Error e,vst) = (Error e,vst)

	onRefresh dp new st vst = case editorOnRefresh dp new st vst of
		(Ok (ui,st,_),vst) = (Ok (ui,st,Nothing),vst)
		(Error e,vst) = (Error e,vst)

ignoreEditorReads :: !(Editor rb wa) -> Editor ra wa
ignoreEditorReads editor=:{Editor|onReset=editorOnReset, onRefresh=editorOnRefresh}
	= {Editor|editor & onReset=onReset, onRefresh=onRefresh, valueFromState=valueFromState}
where
	onReset attr dp mode vst = editorOnReset attr dp Enter vst
	onRefresh dp new st vst = (Ok (NoChange,st,Nothing),vst)
	valueFromState st = Nothing

bijectEditorValue :: !(ra -> rb) !(rb -> ra) !(Editor rb w) -> Editor ra w
bijectEditorValue tof fromf editor=:{Editor|onReset=editorOnReset,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor| editor & onReset=onReset,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst  = editorOnReset attr dp (mapEditMode tof mode) vst
	onRefresh dp new st vst = editorOnRefresh dp (tof new) st vst

	valueFromState st = case editorValueFromState st of
		Just val = Just $ fromf val
		_        = Nothing

comapEditorValue :: !(ra -> rb) !(Editor rb w) -> Editor ra w
comapEditorValue tof editor=:{Editor|onReset=editorOnReset,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor| editor & onReset=onReset,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst = editorOnReset attr dp (mapEditMode tof mode) vst
	onRefresh dp new st vst = editorOnRefresh dp (tof new) st vst
	valueFromState st = Nothing

injectEditorValue :: !(ra -> rb) !(rb -> MaybeErrorString ra) !(Editor rb w) -> Editor ra w
injectEditorValue tof fromf {Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor|onReset=onReset,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst = editorOnReset attr dp (mapEditMode tof mode) vst
	onEdit dp e st vst       = editorOnEdit dp e st vst
	onRefresh dp newa st vst = editorOnRefresh dp (tof newa) st vst

	valueFromState st = case editorValueFromState st of
		Just valb = case fromf valb of
			Ok vala = Just vala
			_       = Nothing
		_ = Nothing

surjectEditorValue :: !(ra (Maybe rb) -> rb) !(rb (Maybe ra) -> ra) !(Editor rb w) -> Editor ra w | JSONEncode{|*|}, JSONDecode{|*|} ra
surjectEditorValue tof fromf {Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= editorModifierWithStateToEditor
		{EditorModifierWithState|onReset=onReset,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst = case editorOnReset attr dp (mapEditMode (\a -> tof a Nothing) mode) vst of
		(Error e,vst) = (Error e,vst)
		//Track value of the 'outer' editor
		(Ok (ui, st, mbw),vst) = (Ok (ui, editModeValue mode, st, mbw), vst)

	onEdit dp e mbOldA st vst = case editorOnEdit dp e st vst of
		(Error e, vst) = (Error e, vst)
		(Ok (change, st,mbw),vst) = (Ok (change, updatedState mbOldA st, st,mbw), vst)

	onRefresh dp newA _ st vst = case editorOnRefresh dp (tof newA (editorValueFromState st)) st vst of
		(Error e, vst) = (Error e, vst)
		(Ok (change, st,mbw), vst) = (Ok (change, updatedState (Just newA) st, st, mbw), vst)

	// only give value if inner editor is in valid state
	valueFromState val innerSt | isJust $ editorValueFromState innerSt = val
	valueFromState _   _                                               = Nothing

	updatedState mbOldA innerSt = maybe mbOldA (\newB -> Just $ fromf newB mbOldA) mbNewB
	where
		mbNewB = editorValueFromState innerSt

mapEditorWrite :: !(wb -> w) !(Editor r wb) -> Editor r w
mapEditorWrite fromf editor = mapEditorWriteWithValue (\_ w -> fromf w) editor

mapEditorWriteError :: !(wb -> MaybeErrorString w) !(Editor r wb) -> Editor r w
mapEditorWriteError fromf editor=:{Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= editorModifierWithStateToEditor {EditorModifierWithState|onReset=onReset,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst = case editorOnReset attr dp (unique mode) vst of
		(Ok (ui,est,mbw),vst)
			# (mbtype,mbhint) = errorInfoUI ui
			# (ReplaceUI ui,st,est,mbw) = mapFromF (ReplaceUI ui,(mbtype,mbhint,False),est,mbw)
			= (Ok (ui,st,est,mbw),vst)
		(Error e,vst) = (Error e,vst)
	onEdit dp e st est vst = case editorOnEdit dp e est vst of
		(Ok (change, est, mbw),vst) = (Ok $ mapFromF (change, st, est, mbw), vst)
		(Error e,vst) = (Error e,vst)
	onRefresh dp new st est vst = case editorOnRefresh dp new est vst of
		(Ok (change, est, mbw),vst) = (Ok $ mapFromF (change, st, est, mbw), vst)
		(Error e,vst) = (Error e,vst)
	valueFromState st est = editorValueFromState est

	mapFromF (change, st, est, Nothing) = (change, st, est, Nothing)
	mapFromF (change, st, est, Just w) = case fromf w of
		(Ok w)
			# (change,st) = restoreErrorInfo change st
			= (change, st, est, Just w)
		(Error e)
			# (change,st) = maskErrorInfo e change st
			= (change, st, est, Nothing)

	//Track the hint and type attributes of the underlying editor
	errorInfoUI (UI _ attrs _) = ('DM'.get HINT_TYPE_ATTRIBUTE attrs, 'DM'.get HINT_ATTRIBUTE attrs)

	errorInfoChange NoChange = (Nothing,Nothing)
	errorInfoChange (ReplaceUI ui)
		# (typeUpd,hintUpd) = errorInfoUI ui
		= (Just typeUpd,Just hintUpd)
	errorInfoChange (ChangeUI attrChanges _)
		# typeUpd = foldl (check HINT_TYPE_ATTRIBUTE) Nothing attrChanges
		# hintUpd = foldl (check HINT_ATTRIBUTE) Nothing attrChanges
		= (typeUpd,hintUpd)
	where
		check attr cur (SetAttribute k v) = if (k == attr) (Just (Just v)) cur
		check attr cur (DelAttribute k) = if (k == attr) (Just Nothing) cur

	//Restore masked value if necessary and track underlying /hint
	restoreErrorInfo change st=:(mbtype,mbhint,masked)
		# (newtype,newhint) = errorInfoChange change
		= (mergeUIChanges (restore st) change, (fromMaybe mbtype newtype,fromMaybe mbhint newhint,False))
	where
		restore (mbtype,mbhint,True)
			# typeChange =  maybe [DelAttribute HINT_TYPE_ATTRIBUTE] (\e -> [SetAttribute HINT_TYPE_ATTRIBUTE e]) mbtype
			# hintChange =  maybe [DelAttribute HINT_ATTRIBUTE] (\e -> [SetAttribute HINT_ATTRIBUTE e]) mbhint
			= ChangeUI (typeChange ++ hintChange) []
		restore _
			= NoChange

	maskErrorInfo msg change (mbtype,mbhint,masked)
		# (newtype,newhint) = errorInfoChange change
		= (mergeUIChanges change (mask msg), (fromMaybe mbtype newtype, fromMaybe mbhint newhint, True))
	where
		mask msg = ChangeUI [SetAttribute HINT_TYPE_ATTRIBUTE (JSONString HINT_TYPE_INVALID), SetAttribute HINT_ATTRIBUTE (JSONString msg)] []

	unique Enter = Enter
	unique (View x) = View x
	unique (Update x) = View x

mapEditorWriteWithValue :: !((Maybe r) wb -> w) !(Editor r wb) -> Editor r w
mapEditorWriteWithValue fromf editor=:{Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= {Editor| editor & onReset=onReset, onEdit=onEdit,onRefresh=onRefresh}
where
	onReset attr dp mode vst = case editorOnReset attr dp (mapEditMode id mode) vst of
		(Ok (ui,st,mbw),vst) = (Ok (ui,st,fmap (fromf $ editorValueFromState st) mbw),vst)
		(Error e,vst) = (Error e,vst)

	onEdit dp e st vst = case editorOnEdit dp e st vst of
		(Ok (ui,st,mbw),vst) = (Ok (ui,st,fmap (fromf $ editorValueFromState st) mbw),vst)
		(Error e,vst) = (Error e,vst)

	onRefresh dp new st vst = case editorOnRefresh dp new st vst of
		(Ok (ui,st,mbw),vst) = (Ok (ui,st,fmap (fromf $ editorValueFromState st) mbw),vst)
		(Error e,vst) = (Error e,vst)

	fmap :: (a -> b) (Maybe a) -> *Maybe b //Stupid inlining of fmap because of need for unique w
	fmap fromf Nothing = Nothing
	fmap fromf (Just w) = Just (fromf w)


lensEditor :: !((Maybe ra) rb -> ra) !((Maybe rb) wa -> Maybe wb) !(Editor ra wa) -> Editor rb wb | JSONEncode{|*|}, JSONDecode{|*|} rb
lensEditor tof fromf {Editor|onReset=editorOnReset,onEdit=editorOnEdit,onRefresh=editorOnRefresh,valueFromState=editorValueFromState}
	= editorModifierWithStateToEditor
		{EditorModifierWithState|onReset=onReset,onEdit=onEdit,onRefresh=onRefresh,valueFromState=valueFromState}
where
	onReset attr dp mode vst
		= appFst (fmap (\(ui, st, mbw) -> (ui, editModeValue mode, st, modWrite Nothing mbw)))
	    $ editorOnReset attr dp (mapEditMode (tof Nothing) mode) vst

	onEdit dp event mbB st vst
		= appFst (fmap (\(ui, st, mbw) -> (ui, mbB, st, modWrite mbB mbw)))
		$ editorOnEdit dp event st vst

	onRefresh dp newB mbB st vst
		= appFst (fmap (\(ui, st, mbw) -> (ui, mbB, st, modWrite mbB mbw)))
		$ editorOnRefresh dp (tof (editorValueFromState st) newB) st vst

	modWrite mbb mbwa = maybe Nothing (fromf mbb) mbwa
	valueFromState mbB st = mbB
