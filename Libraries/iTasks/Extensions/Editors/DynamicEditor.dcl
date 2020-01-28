definition module iTasks.Extensions.Editors.DynamicEditor

/**
 * This provides dynamic editors, which can be constructed dynamically by data
 * and also can make use of the power of the dynamic type system which allows for quantified type variables.
 * This makes it possible to achieve type safety similar to the safety provided by GADTs.
 *
 * The main idea is to provide a number of dynamic conses producing values and requiring arguments.
 * For all required arguments the user has the choice between all dynamic conses providing a value of the proper type.
 */

from Data.Maybe   import :: Maybe
from Data.GenEq   import generic gEq
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks       import class iTask, class tune, generic gEditor, generic gText, :: Editor, :: TextFormat

/**
 * This provides the iTasks editor corresponding to a dynamic editor definition.
 *
 * @param The dynamic editor definition.
 * @result The iTasks editor.
 */
dynamicEditor :: !(DynamicEditor a) -> Editor (DynamicEditorValue a) (Maybe (DynamicEditorValue a)) | TC a

/**
 * This provides the iTasks editor corresponding to a dynamic editor definition parametrised by an additional value.
 * TODO: This could be done more clean if iTasks editor would support parameters.
 *
 * @param The parametrised dynamic editor definition.
 * @result The iTasks editor additionally working on the parameter.
 */
parametrisedDynamicEditor ::
	!(p -> DynamicEditor a) -> Editor (!p, !DynamicEditorValue a) (Maybe (!p, !DynamicEditorValue a)) | TC a & gEq{|*|}, JSONEncode{|*|}, JSONDecode{|*|} p

/**
 * Represents a dynamic editor value, which represents an actual value.
 * There may however be multiple dynamic editor values mapping to the same actual value.
 * Because of this only dynamic editor values can be edited by dynamic editors.
 */
:: DynamicEditorValue a
	= DynamicEditorValue !DynamicConsId !DEVal
	| Undefined // TODO: Undefined can be removed once we have parametrised editors

derive class iTask DynamicEditorValue

/**
 * The identity of a dynamic constructor.
 */
:: DynamicConsId :== String

/**
 * The value of a dynamic editor constructor.
 */
:: DEVal = DEApplication ![(DynamicConsId, DEVal)] //* A dynamic constructor applied to a number of arguments.
         | DEJSONValue   !JSONNode                 //* An ordinary, JSON-encoded value.

/**
 * `DynamicEditor a` provides a dynamic editor definition for editing values of type `a`.
 */
:: DynamicEditor a =: DynamicEditor [DynamicEditorElement]

/**
 * `valueCorrespondingTo dynamicEditor dynamicEditorValue = value`:
 *     `value` is the actual value corresponding to `dynamicEditorValue` given `dynamicEditor`.
 */
valueCorrespondingTo :: !(DynamicEditor a) !(DynamicEditorValue a) -> a | TC a

/**
 * `stringRepresenting dynamicEditor dynamicEditorValue = string`:
 *     `string` is the string representing `dynamicEditorValue` given `dynamicEditor`.
 */
stringCorrespondingTo :: !(DynamicEditor a) !(DynamicEditorValue a) -> String

/**
 * Element of a dynamic editor definition.
 */
:: DynamicEditorElement
	= DynamicCons      !DynamicCons
	  //* Represents a dynamic construtor.
	| DynamicConsGroup !String ![DynamicCons]
	  //* `DynamicConsGroup name conses` represents a group of `conses` with `name`.
	  //* Groups are used to structure the UI to make a large number of choices more accessible.

/**
 * A dynamic constructor.
 */
:: DynamicCons

/**
 * `functionCons id label function = dynamicCons`:
 *     `dynamicCons` is the dynamic function constructor with identity `id` and `label`.
 *     The value of the element generated is the result value of `function`.
 *     For all arguments of `function` the user is given the possibility to enter a dynamic value
 *     of the corresponding types.
 */
functionCons :: !DynamicConsId !String !a -> DynamicCons | TC a

/**
 * A variant of {{functionCons}} using a {{Dynamic}} value as function.
 * This variant is more powerful as dynamic values make it possible to use quantified type variables.
 */
functionConsDyn :: !DynamicConsId !String !Dynamic -> DynamicCons

/**
 * `listCons id label resultFor = dynamicCons`:
 *     `dynamicCons` is the dynamic list constructor with identity `id` and `label`.
 *     The user is given the possibility to enter a `list` of dynamic value of type `a`.
 *     The editor's result is given by `resultFor list`.
 */
listCons :: !DynamicConsId !String !([a] -> b) -> DynamicCons | TC a & TC b

/**
 * A variant of {{listCons}}  using a {{Dynamic}} value as function.
 * This variant is more powerful as dynamic values make it possible to use quantified type variables.
 * The dynamic argument must be of type `(a -> b, [b] -> c)`, which cannot be enforced by the type system!
 * All common type variables in `a` and `c` are unified.
 * The possible remaining variables in `a` do not have to be unifiable for all list elements,
 * as long as all values of type `b` to which they are mapped are unifiable.
 */
listConsDyn :: !DynamicConsId !String !Dynamic -> DynamicCons

/**
 * `customEditorCons id label editor = dynamicCons`:
 *     `dynamicCons` is the dynamic constructor with identity `id` and `label` corresponding to the iTasks `editor`.
 */
customEditorCons ::
	!DynamicConsId !String !(Editor a (Maybe a)) -> DynamicCons | TC, JSONEncode{|*|}, JSONDecode{|*|}, gText{|*|} a

instance tune DynamicConsOption DynamicCons

/**
 * Options to tune dynamic conses.
 */
:: DynamicConsOption
	= HideIfOnlyChoice          //* Hide the choice for this cons, if there are no other applicable conses.
	| UseAsDefault              //* As this cons as default, i.e. the cons is pre-selected in the UI.
	| ApplyCssClasses ![String] //* CSS classes applied to the UI if the cons is selected.
	| AddLabels ![Maybe String]
	  //* Labels for the cons arguments, if the list contains too many labels the rest of ignored.
