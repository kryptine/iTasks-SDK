definition module iTasks.UI.Editor.Modifiers
/**
* This module provides combinator functions for combining editors
*/
import iTasks.UI.Editor, iTasks.UI.Definition, iTasks.UI.Tune
import Data.Error

//### Modifying atributes of editors ### 
/**
* Adds an attribute that tags the UI with the edit mode of the editor: view, enter, update
* This does not change the mode of the editor
*/
withEditModeAttr :: !(Editor r w) -> Editor r w

/**
* Adds a hint and hint-type attribute based on the editor's state.
* Layouts (such as the automatic default layout) can use these attributes to create hint icons and tooltips
*/
withDynamicHintAttributes :: !String !(Editor r w) -> Editor r w

//### Changing the edit mode ### 
/*
* Change the edit mode (view/enter/update)
*/
withChangedEditMode :: !((EditMode r) -> EditMode r) !(Editor r w) -> Editor r w

/**
* Selecting between editors based on the edit mode of the interaction (view/enter/update)
* 
* @param Editor for viewing a value
* @param Editor for entering a value
* @param Editor for updating a value
*/
selectByMode :: !(Editor r w) !(Editor r w) !(Editor r w) -> Editor r w

/**
* Uses the given editor to view a constant value.
*
* @param The constant value to view.
* @param The editor used to view the value.
* @result An editor viewing the constant value.
*/
viewConstantValue :: !r !(Editor r w) -> Editor () w

/**
* Drop all writes that the editor produces
*/
ignoreEditorWrites :: !(Editor ra wb) -> Editor ra wa

/**
* Ignore all data reads
*/
ignoreEditorReads :: !(Editor rb wa) -> Editor ra wa

// ### Changing the model value of the editor ###

/**
* Map the read value of an editor to another (isomorphic) domain
*/
bijectEditorValue :: !(ra -> rb) !(rb -> ra) !(Editor rb w) -> Editor ra w

/**
* Map the read value of an editor to another domain, without mapping value changes back
* Editor writes are still passed on, but `valueFromState` will always be Nothing
*/
comapEditorValue :: !(ra -> rb) !(Editor rb w) -> Editor ra w

/**
* Map the value of an editor to another domain which is 'bigger' than the original domain
* so conversion back to the original is not always possible
* When mapping back is impossible, the 'hint' and 'hint-type' attributes are set on the editor
* and `valueFromState` will not yield a value.
*/
injectEditorValue :: !(ra -> rb) !(rb -> MaybeErrorString ra) !(Editor rb w) -> Editor ra w

/**
* Map the value of an editor to another domain which is 'smaller' than the original domain
*/
surjectEditorValue :: !(ra (Maybe rb) -> rb) !(rb (Maybe ra) -> ra) !(Editor rb w) -> Editor ra w | JSONEncode{|*|}, JSONDecode{|*|} ra

/**
* Map editor writes to a different domain
*/
mapEditorWrite :: !(wb -> w) !(Editor r wb) -> Editor r w
/**
* Map editor writes to a different domain with potential errors
* If the mapped editor returns an error, the write is not propagated and
* a hint-type error and hint attribute are set on the editor.
*/
mapEditorWriteError :: !(wb -> MaybeErrorString w) !(Editor r wb) -> Editor r w
mapEditorWriteWithValue :: !((Maybe r) wb -> w) !(Editor r wb) -> Editor r w

/**
* Select part of a larger datastructure and map writes back
* This allows mapping read and write values in one go
*/
lensEditor :: !((Maybe ra) rb -> ra) !((Maybe rb) wa -> Maybe wb) !(Editor ra wa) -> Editor rb wb | JSONEncode{|*|}, JSONDecode{|*|} rb
