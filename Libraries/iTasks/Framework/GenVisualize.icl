implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap
import GenUpdate
import Void, Either
import Text, Html, ExtJS, JSON

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| vizType = VTextDisplay, idPrefix = "", currentPath = [0], label = Nothing, consBody = False, optional = False, valid = True, cons = False}

//Wrapper functions
visualizeAsEditor :: String DataMask a -> ([ExtJSDef],Bool) | gVisualize{|*|} a
visualizeAsEditor name mask x
	# (defs,vst=:{valid}) = gVisualize{|*|} val val {mkVSt & vizType =VEditorDefinition, idPrefix = name}
	= (coerceToExtJSDefs defs, valid)	
where
	val = VValue x mask
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & vizType = VHtmlDisplay})))
where
	val = VValue x []

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & vizType = VTextDisplay})))
where
	val = VValue x []

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & vizType = VHtmlLabel})))
where
	val = VValue x []
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & vizType = VTextLabel})))
where
	val = VValue x []
	
determineEditorUpdates	:: String DataMask DataMask a a -> ([ExtJSUpdate],Bool)	| gVisualize{|*|} a
determineEditorUpdates name omask nmask old new
//	# omask	= trace_n ("OLD MASK: " +++ printToString omask) omask
//	# nmask = trace_n ("NEW MASK: " +++ printToString nmask) nmask
	# (updates,vst=:{valid}) = gVisualize{|*|} (VValue old omask) (VValue new nmask) {mkVSt & vizType = VEditorUpdate, idPrefix = name}
	= (coerceToExtJSUpdates updates, valid)

//Bimap for visualization values
derive bimap VisualizationValue

//Generic visualizer
generic gVisualize a :: (VisualizationValue a) (VisualizationValue a) *VSt -> ([Visualization], *VSt)

gVisualize{|UNIT|} _ _ vst
	= ([],vst)

gVisualize{|PAIR|} fx fy old new vst
	= case (old,new) of
		(VValue (PAIR ox oy) omask, VValue (PAIR nx ny) nmask)
			# (vizx, vst) = fx (VValue ox omask) (VValue nx nmask) vst
			# (vizy, vst) = fy (VValue oy omask) (VValue ny nmask) vst
			= (vizx ++ vizy, vst)
		_
			# (vizx, vst) = fx VBlank VBlank vst
			# (vizy, vst) = fy VBlank VBlank vst
			= (vizx ++ vizy, vst)

gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,currentPath,consBody,valid}
	= case (old,new) of
		//Same structure:
		(VValue (LEFT ox) omask, VValue (LEFT nx) nmask)
			# oval = VValue ox omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
						# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})			
					| otherwise
						= fx oval nval vst
				_
					= fx oval nval vst
		(VValue (RIGHT oy) omask, VValue (RIGHT ny) nmask)
			# oval = VValue oy omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
						# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
					| otherwise
						= fy oval nval vst
				_
					= fy oval nval vst
		//Different structure:
		(VValue (LEFT ox) omask, VValue (RIGHT ny) nmask)
			# oval = VValue ox omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VValue (LEFT nx) nmask)
			# oval = VValue oy omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fy oval oval vst //Default case: ignore the new value
		
		//New value
		(VValue (LEFT ox) omask, VBlank)
			# oval = VValue ox omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VBlank)
			# oval = VValue oy omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fy oval oval vst //Default case: ignore the new value					
		//No value any more
		(VBlank, VValue (LEFT nx) nmask)
			# oval = VBlank
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VBlank, VValue (RIGHT ny) nmask)
			# oval = VBlank
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, consBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, consBody = consBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		//Default case
		_
			= fx VBlank VBlank vst		
where
	maskChanged dp m1 m2	= (isMasked dp m1 && not (isMasked dp m2)) || (not (isMasked dp m1) && isMasked dp m2)
	pathid					= dp2id idPrefix currentPath		


gVisualize{|CONS of d|} fx old new vst=:{vizType,idPrefix,currentPath,label,consBody,optional,valid}
	= case vizType of
		//Editor definition
		VEditorDefinition
			# (ox,nx) = case (old,new) of
					(VValue (CONS ox) omask,VValue (CONS nx) nmask)	= (VValue ox omask, VValue nx nmask)
					_												= (VBlank,VBlank)
			// Records
			| not (isEmpty d.gcd_fields)	
				# (vizBody,vst=:{valid}) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, consBody = False, optional = False}
				//Add a containing fieldset on the first level
				| dataPathLevel currentPath > 1						
					= ([ExtJSFragment (ExtJSFieldSet {ExtJSFieldSet | id = (dp2id idPrefix currentPath) +++ "-fs"
																	, title = title label
																	, items = coerceToExtJSDefs vizBody
																	, autoHeight = True, border = True})]
																	, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional, cons = False})
				| otherwise
					= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath, cons = False})
						
			//ADT's with only one constructor
			| d.gcd_type_def.gtd_num_conses == 1 
				# (vizBody,vst=:{valid}) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, optional = False}
				= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
			//ADT's with multiple constructors
			| otherwise
				# (vizBody, vst=:{valid})
					= if (showBody currentPath old)
						(fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, consBody = False, optional = False})
						([], vst)	 	
				| consBody //Normal ADT's without constructor selector
					= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional, cons = True})
				| otherwise	//Normal ADT's with constructor selector
					= ((consSelector d idPrefix currentPath old (label2s optional label)) ++ vizBody
					  ,{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional, cons = True})
		//Html display vizualization
		VHtmlDisplay
			= case (old,new) of
				(VValue (CONS ox) omask, VValue (CONS nx) nmask)
					# (vizBody, vst) = fx (VValue ox omask) (VValue nx nmask) {vst & label = Nothing, currentPath = shiftDataPath currentPath}
					//Records
					| not (isEmpty d.gcd_fields) 
						= ([HtmlFragment [TableTag [] (flatten (coerceToHtml vizBody))]],{VSt|vst & currentPath = stepDataPath currentPath})
					//Normal ADT's
					| otherwise
						= (vizCons ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= ([], {VSt|vst & currentPath = stepDataPath currentPath})
		_ 	//Other visualizations
			= case (old,new) of
				(VValue (CONS ox) omask, VValue (CONS nx) nmask)
					# (vizBody, vst=:{valid}) = fx (VValue ox omask) (VValue nx nmask) {vst & label = Nothing, currentPath = shiftDataPath currentPath, optional = False}
					//No validity check is needed when there is only one constructor
					| d.gcd_type_def.gtd_num_conses == 1
						= (vizCons ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
					//A validity check is used
					| otherwise
						= (vizCons ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional, cons = isEmpty d.gcd_fields})
				_
					= ([], {VSt|vst & currentPath = stepDataPath currentPath})		
where
	//When we have no title there is nothing to display :)
	title (Just t)	= t
	title Nothing	= ""
	
	//Do not show constructors that start with an underscore (_Tuple2,_Cons etc.)
	vizCons = if (d.gcd_name.[0] == '_') [] [TextFragment d.gcd_name]

	//Only show a body when you have a value and it is masked
	showBody dp VBlank			= False
	showBody dp (VValue _ dm)	= isMasked dp dm 

gVisualize{|OBJECT of d|} fx old new vst
	= case (old,new) of
		(VValue (OBJECT ox) omask, VValue (OBJECT nx) nmask)
			= fx (VValue ox omask) (VValue nx nmask) vst
		(VValue (OBJECT ox) omask, VBlank)
			= fx (VValue ox omask) VBlank vst
		(VBlank, VValue (OBJECT nx) nmask)
			= fx VBlank (VValue nx nmask) vst
		_
			= fx VBlank VBlank	vst

gVisualize{|FIELD of d|} fx old new vst=:{vizType}
	= case (old,new) of
		(VValue (FIELD ox) omask, VValue (FIELD nx) nmask)
			= case vizType of
				VHtmlDisplay
					# (vizBody,vst) 	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Nothing}
					= ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],vst)
				VTextDisplay
					# (vizBody,vst) 	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody], vst)
				_
					= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
		_
			= fx VBlank VBlank {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,idPrefix,label,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old , fieldLabel = label2s optional label, hideLabel = isNothing label, allowDecimals = False, numDecimals = 0})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, cons = False})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid, cons = False})
		
gVisualize{|Real|} old new vst=:{vizType,idPrefix,label,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = isNothing label, allowDecimals = True, numDecimals = 1000})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, cons = False})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid, cons = False})
		
gVisualize{|Char|} old new vst=:{vizType,idPrefix,label,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = isNothing label})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, cons = False})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid, cons = False})

gVisualize{|Bool|} old new vst=:{vizType,idPrefix,label,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSCheckBox {ExtJSCheckBox|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value, boxLabel = Nothing, fieldLabel = label2s optional label, hideLabel = isNothing label, checked = checked })]
								, {VSt|vst & currentPath = stepDataPath currentPath, cons = False})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, cons = False})		
where
	checked	= case old of
		VBlank			= False
		(VValue v mask) = if (isMasked currentPath mask) v False
	value	= if checked "true" "false"

gVisualize{|String|} old new vst=:{vizType,idPrefix,label,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = isNothing label})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, cons = False})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid, cons = False})


gVisualize{|Maybe|} fx old new vst=:{vizType,idPrefix,currentPath,optional,valid,consBody}
	= case vizType of
		VEditorDefinition
			= case (old,new) of
				(VValue (Just ox) omask, _)
					# oval = VValue ox omask
					# (viz, vst) = fx oval oval {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(VValue (Just ox) omask, VValue (Just nx) nmask)
					# (viz, vst) = fx (VValue ox omask) (VValue nx nmask) {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue (Just ox) omask, VValue Nothing nmask)
					# (viz, vst) = fx (VValue ox omask) VBlank {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue Nothing omask, VValue (Just nx) nmask)
					# (viz, vst) = fx VBlank (VValue nx nmask) {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {vst & optional = True}
					= (viz, {vst & optional = optional, currentPath = stepDataPath currentPath})
		_			
			= case old of
				(VValue Nothing m)	= ([TextFragment "-"],vst)
				(VValue (Just x) m)	= fx (VValue x m) (VValue x m) vst
				VBlank				= ([],vst)
where
	pathid = dp2id idPrefix currentPath		


gVisualize{|Dynamic|} old new vst
	= ([],vst)

derive gVisualize []
derive gVisualize Either, (,), (,,), (,,,), Void


instance toString (VisualizationValue a) | toString a
where
	toString VBlank			= ""
	toString (VValue x _)	= toString x
	
value2s :: DataPath (VisualizationValue a)	-> String | toString a
value2s dp VBlank = ""
value2s dp (VValue a dm)
	| isMasked dp dm	= toString a
	| otherwise			= ""

label2s :: Bool (Maybe String) -> Maybe String
label2s _		Nothing		= Nothing
label2s True	(Just l)	= Just (l +++ " (optional)")
label2s False	(Just l)	= Just l

stillValid :: DataPath (VisualizationValue a) Bool Bool -> Bool
stillValid dp val optional valid
	# dm = case val of (VValue _ m) = m ; _ = []
	| optional				= valid //Nothing changes
	| not (isMasked dp dm)	= False
	| otherwise				= valid	//A non-optional field must be masked to be valid

//	| optional				= trace_n (printToString dp +++ " OPT") valid 	//Nothing changes
//	| not (isMasked dp dm)	= trace_n (printToString dp +++ " BAD") False
//	| otherwise				= trace_n (printToString dp +++ " OK") valid	//A non-optional field must be masked to be valid

//import StdDebug, GenPrint

formatLabel :: String -> String
formatLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= [c \\ c <-: label]
	addspace []			= []
	addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

consSelector :: GenericConsDescriptor String DataPath (VisualizationValue a) (Maybe String) -> [Visualization]
consSelector d idPrefix dp value label
	# masked = case value of (VValue _ dm) = isMasked dp dm; _ = False
	//No choice needed with only one constructor
	| d.gcd_type_def.gtd_num_conses == 1 
		= []
	//Use radiogroup to choose a constructor
	//| d.gcd_type_def.gtd_num_conses <= MAX_CONS_RADIO 
	//	= [ExtJSFragment (ExtJSRadioGroup {ExtJSRadioGroup|name = name, id = id, items = items, fieldLabel = label, hideLabel = isNothing label})]
	//Use combobox to choose a constructor
	| otherwise
		= [ExtJSFragment (ExtJSComboBox {ExtJSComboBox|name = name, id = id, value = (if masked d.gcd_name ""), fieldLabel = label, hideLabel = isNothing label, store = store, triggerAction = "all", editable = False})]
where
	//items	= [ExtJSRadio {ExtJSRadio|name = name, value = c.gcd_name, boxLabel = Just c.gcd_name, checked = (masked && c.gcd_index == index), fieldLabel = Nothing, hideLabel = True} 
	//		  \\ c <- d.gcd_type_def.gtd_conses]
	store	= [("","Select...") : [(c.gcd_name,c.gcd_name) \\ c <- d.gcd_type_def.gtd_conses]]
	name	= dp2s dp
	id		= dp2id idPrefix dp
	index	= d.gcd_index
	
	
determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = [ExtJSUpdate (ExtJSRemove consid) \\ consid <- collectIds (coerceToExtJSDefs editor)]
where
	collectIds [] = []
	collectIds [ExtJSNumberField {ExtJSNumberField|id}:is] = [id:collectIds is]
	collectIds [ExtJSTextField {ExtJSTextField|id}:is] = [id:collectIds is]
	collectIds [ExtJSTextArea {ExtJSTextArea|id}:is] = [id:collectIds is]
	collectIds [ExtJSCheckBox {ExtJSCheckBox|id}:is] = [id:collectIds is]
	collectIds [ExtJSRadioGroup {ExtJSRadioGroup|id}:is] = [id:collectIds is]
	collectIds [ExtJSComboBox {ExtJSComboBox|id}:is] = [id:collectIds is]
	collectIds [ExtJSFieldSet {ExtJSFieldSet|id}:is] = [id:collectIds is]
	collectIds [_:is] = collectIds is
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = reverse [ExtJSUpdate (ExtJSAdd consid def) \\ def <- coerceToExtJSDefs editor]

//Coercion of visualizations
coerceToExtJSDefs :: [Visualization] -> [ExtJSDef]
coerceToExtJSDefs visualizations = [d \\ (ExtJSFragment d) <- visualizations]

coerceToExtJSUpdates :: [Visualization] -> [ExtJSUpdate]
coerceToExtJSUpdates visualizations = [u \\ (ExtJSUpdate u) <- visualizations]

coerceToStrings :: [Visualization] -> [String]
coerceToStrings visualizations = [s \\ (TextFragment s) <- visualizations]

coerceToHtml :: [Visualization] -> [[HtmlTag]]
coerceToHtml visualizations = [coerce h \\h <- visualizations | coercable h]
where
	coerce (TextFragment s)		= [Text s]
	coerce (HtmlFragment h)		= h
	
	coercable (TextFragment _)	= True
	coercable (HtmlFragment _)	= True
	coercable _					= False
