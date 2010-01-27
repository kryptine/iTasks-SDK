implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap, StdEnum
import GenUpdate, GenEq
import Void, Either
import Text, Html, JSON, TUIDefinition

from Types import emptyDoc

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| vizType = VTextDisplay, idPrefix = "", currentPath = [0], label = Nothing, useLabels = False, onlyBody = False, optional = False, valid = True, listMask = []}

//Wrapper functions
visualizeAsEditor :: String DataMask a -> ([TUIDef],Bool) | gVisualize{|*|} a
visualizeAsEditor name mask x
	# (defs,vst=:{valid}) = gVisualize{|*|} val val {mkVSt & vizType =VEditorDefinition, idPrefix = name}
	= (coerceToTUIDefs defs, valid)	
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
	
determineEditorUpdates	:: String DataMask DataMask ListMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a
determineEditorUpdates name omask nmask lmask old new
	//# omask = trace_n ("OLD MASK: " +++ printToString omask) omask
	//# nmask = trace_n ("NEW MASK: " +++ printToString nmask) nmask
	//# lmask = trace_n ("LST MASK: " +++ printToString lmask) lmask
	# (updates,vst=:{valid}) = (gVisualize{|*|} (VValue old omask) (VValue new nmask) {mkVSt & vizType = VEditorUpdate, idPrefix = name, listMask = lmask})
	= (coerceToTUIUpdates updates, valid)

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

gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,currentPath,onlyBody,valid}
	= case (old,new) of
		//Same structure:
		(VValue (LEFT ox) omask, VValue (LEFT nx) nmask)
			# oval = VValue ox omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
						# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})			
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
						# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
						# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
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
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VValue (LEFT nx) nmask)
			# oval = VValue oy omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fy oval oval vst //Default case: ignore the new value
		
		//New value
		(VValue (LEFT ox) omask, VBlank)
			# oval = VValue ox omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VBlank)
			# oval = VValue oy omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fy oval oval vst //Default case: ignore the new value					
		//No value any more
		(VBlank, VValue (LEFT nx) nmask)
			# oval = VBlank
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VBlank, VValue (RIGHT ny) nmask)
			# oval = VBlank
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (old,vst) = fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					# (new,vst) = fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, onlyBody = True, valid = valid}
					= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = VEditorUpdate, onlyBody = onlyBody})
				_
					= fx oval oval vst //Default case: ignore the new value
		//Default case
		_
			= fx VBlank VBlank vst		
where
	maskChanged dp m1 m2	= (isMasked dp m1 && not (isMasked dp m2)) || (not (isMasked dp m1) && isMasked dp m2)
	pathid					= dp2id idPrefix currentPath		


gVisualize{|CONS of d|} fx old new vst=:{vizType,idPrefix,currentPath,label,useLabels,onlyBody,optional,valid}
	= case vizType of
		//Editor definition
		VEditorDefinition
			# (ox,nx) = case (old,new) of
					(VValue (CONS ox) omask,VValue (CONS nx) nmask)	= (VValue ox omask, VValue nx nmask)
					_												= (VBlank,VBlank)
			// Records
			| not (isEmpty d.gcd_fields)	
				# (vizBody,vst=:{valid}) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, onlyBody = False, useLabels = True, optional = False}
				//Add a containing fieldset on the first level
				| dataPathLevel currentPath	> 1				
					= ([TUIFragment (TUIFieldSet {TUIFieldSet | id = (dp2id idPrefix currentPath) +++ "-fs"
																	, layout = Just "form"
																	, title = title label
																	, items = coerceToTUIDefs vizBody
																	, autoHeight = True, border = isJust label
																	, fieldLabel = Nothing, hideLabel = True})]
																	, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
				| otherwise
					= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath})
						
			//ADT's with only one constructor
			| d.gcd_type_def.gtd_num_conses == 1 
				# (vizBody,vst=:{valid}) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, optional = False}
				= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
			//ADT's with multiple constructors
			| otherwise
				# (vizBody, vst=:{valid})
					= if (showBody currentPath old)
						(fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, onlyBody = False, optional = False})
						([], vst)	 	
				| onlyBody //Normal ADT's without constructor selector
					= (vizBody, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional})
				| otherwise	//Normal ADT's with constructor selector
					= ((consSelector d idPrefix currentPath old (label2s optional label) useLabels) ++ vizBody, 
						{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional})
					
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
					# useLabels = not (isEmpty d.gcd_fields) || useLabels
					# (vizBody, vst=:{valid}) = fx (VValue ox omask) (VValue nx nmask) {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = useLabels, optional = False}
					//No validity check is needed when there is only one constructor
					| d.gcd_type_def.gtd_num_conses == 1
						= (vizCons ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
					//A validity check is used
					| otherwise
						= (vizCons ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid, optional = optional})
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
					= ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],{VSt | vst & label = Nothing})
				VTextDisplay
					# (vizBody,vst) 	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody], {VSt | vst & label = Nothing})
				_
					# (vizBody,vst)		= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= (vizBody, {VSt | vst & label = Nothing})
		_
			= fx VBlank VBlank {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUINumberField {TUINumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old , fieldLabel = label2s optional label, hideLabel = not useLabels, allowDecimals = False, numDecimals = 0})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})
		
gVisualize{|Real|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUINumberField {TUINumberField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = not useLabels, allowDecimals = True, numDecimals = 1000})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})
		
gVisualize{|Char|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUITextField {TUITextField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = not useLabels})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})

gVisualize{|Bool|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUICheckBox {TUICheckBox|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value, boxLabel = Nothing, fieldLabel = label2s optional label, hideLabel = not useLabels, checked = checked })]
								, {VSt|vst & currentPath = stepDataPath currentPath})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath})		
where
	checked	= case old of
		VBlank			= False
		(VValue v mask) = if (isMasked currentPath mask) v False
	value	= if checked "true" "false"

gVisualize{|String|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUITextField {TUITextField|name = dp2s currentPath, id = dp2id idPrefix currentPath, value = value2s currentPath old, fieldLabel = label2s optional label, hideLabel = not useLabels})]
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
		_					= ([TextFragment (toString old)],{VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})


gVisualize{|Maybe|} fx old new vst=:{vizType,idPrefix,currentPath,optional,valid,onlyBody}
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

gVisualize{|(,)|} f1 f2 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2) = case old of (VValue (o1,o2) omask) = (VValue o1 omask, VValue o2 omask) ; _ = (VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,vst) = f2 v2 v2 vst
			= ([TUIFragment (TUIPanel {TUIPanel | layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = label2s optional label,
											 items = [ 
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz1},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz2}
											 ]})]			 
			  ,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2) omask, VValue(n1,n2) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					= (viz1 ++ [TextFragment ", "] ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					= (viz1 ++ [TextFragment ", "] ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})			

gVisualize{|(,,)|} f1 f2 f3 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3) = case old of (VValue (o1,o2,o3) omask) = (VValue o1 omask, VValue o2 omask, VValue o3 omask) ; _ = (VBlank,VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,vst) = f2 v2 v2 vst
			# (viz3,vst) = f3 v3 v3 vst
			= ([TUIFragment (TUIPanel {TUIPanel | layout = "", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = label2s optional label,
											 items = [ 
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz1},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz2},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz3}
											 ]})]			 
			  ,{VSt|vst & currentPath = stepDataPath currentPath, useLabels=oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3) omask, VValue(n1,n2,n3) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					= (viz1 ++ [TextFragment ", "] ++ viz2 ++ [TextFragment ", "] ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					= (viz1 ++ [TextFragment ", "] ++ viz2 ++ [TextFragment ", "] ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})	

gVisualize{|(,,,)|} f1 f2 f3 f4 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	# oldLabel = useLabels
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3,v4) = case old of (VValue (o1,o2,o3,o4) omask) = (VValue o1 omask, VValue o2 omask, VValue o3 omask,VValue o4 omask) ; _ = (VBlank,VBlank,VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,vst) = f2 v2 v2 vst
			# (viz3,vst) = f3 v3 v3 vst
			# (viz4,vst) = f4 v4 v4 vst
			= ([TUIFragment (TUIPanel {TUIPanel | layout="", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = label2s optional label,
											 items = [ 
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz1},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz2},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz3},
											 	TUIPanel {TUIPanel| layout = "form", buttons = [], autoHeight = True, autoWidth = True, border = False, bodyCssClass = "", fieldLabel = Nothing, items = coerceToTUIDefs viz4}
											 ]})]			 
			  ,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3,o4) omask, VValue(n1,n2,n3,n4) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					# (viz4,vst) = f4 (VValue o4 omask) (VValue n4 nmask) vst
					= (viz1 ++ [TextFragment ", "] ++ viz2 ++ [TextFragment ", "] ++ viz3 ++ [TextFragment ", "] ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					# (viz4,vst) = f4 VBlank VBlank vst
					= (viz1 ++ [TextFragment ", "] ++ viz2 ++ [TextFragment ", "] ++ viz3 ++ [TextFragment ", "] ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})

gVisualize {|[]|} fx old new vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,listMask}
	= case vizType of
		VHtmlDisplay
			= case old of
				(VValue [] omask)
					= ([HtmlFragment [ITag [] [(Text "Empty list")]]],{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue ov omask)
					# (viz,vst) = vizStatic fx ov omask {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment [UlTag [ClassAttr "listDisplay"] [(LiTag [ClassAttr "listDisplay"] (flatten (coerceToHtml x))) \\ x <- viz]]],{VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([HtmlFragment [(Text "-")]],{VSt | vst & currentPath = stepDataPath currentPath})			
		VEditorDefinition
			= case old of
				(VValue [] omask)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s currentPath, id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath}) 
				(VValue ov omask)
					# (viz, vst) = vizEditor fx ov omask 0 (dp2id idPrefix currentPath) (dp2s currentPath) {VSt | vst & currentPath = shiftDataPath currentPath, vizType=VEditorDefinition}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s currentPath, id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s currentPath, id= dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(_, VBlank) 
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s currentPath, id= dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath})
				//(VValue ov omask, VBlank)
				//	= ([TUIUpdate (TUIRemove (dp2id idPrefix currentPath))], {VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank,(VValue nv nmask)) 
					# (viz, vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s currentPath) {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, optional = False}
					# vst = {VSt | vst & optional = optional}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s currentPath, id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue [] omask, VValue nv nmask)
					# (viz, vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s currentPath) {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorDefinition, optional=False}
					# vst = {VSt | vst & optional = optional}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s currentPath, id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate})
				(VValue ov omask, VValue [] nmask)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s currentPath, id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels})],{VSt | vst & currentPath = stepDataPath currentPath})  
				(VValue ov omask, VValue nv nmask)						
					# (nupd,vst) = vizUpdate fx ov nv omask nmask {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorUpdate}
					# (nviz,vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s currentPath) {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorDefinition, optional=False}
					# vst 		 = {VSt | vst & optional = optional}
					# lo  		 = length ov
					# ln  		 = length nv
					# lmask      = [(dp,idx) \\ (dp,idx) <- listMask | dp == currentPath]
					# idx        = if(length lmask > 0) (snd (last lmask)) []
					# idx 		 = [x \\ x <- idx | x < length ov && x < length nv]
					# rplc 		 = determineReplacements nviz idx
					# rem  		 = determineRemovals lo ln (dp2id idPrefix currentPath)
					# add  		 = determineAdditions nviz lo ln (dp2id idPrefix currentPath)
					= (nupd++rplc++add++rem,{VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate})	
		_
			= case old of
				(VValue [] omask)
					= ([TextFragment "Empty list"],{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue ov omask)
					# (viz,vst) = vizStatic fx ov omask {VSt | vst & currentPath = shiftDataPath currentPath}
					# strings   = flatten [(coerceToStrings x) \\ x <-viz]
					= ([TextFragment (toTextFrag strings)], {VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([TextFragment "-"],{VSt | vst & currentPath = stepDataPath currentPath})	 					
where
	vizEditor fx []     mask index pfx name vst = ([],vst)
	vizEditor fx [x:xs] mask index pfx name vst=:{label}
	# (vx,vst) 	= fx (VValue x mask) (VValue x mask) {VSt | vst & label = Nothing} //Don't display any labels.
	# tx		= (TUIListItem {TUIListItem | items=coerceToTUIDefs vx, index=index, name=name, id=pfx+++"_"+++toString index})
	# (txs,vst) = vizEditor fx xs mask (index+1) pfx name vst
	= ([tx:txs],{VSt | vst & label = label})
	
	vizUpdate fx [o:os] [n:ns] omask nmask vst
		# (ux,vst)  = fx (VValue o omask) (VValue n nmask) vst
		# (uxs,vst) = vizUpdate fx os ns omask nmask vst
		= (ux ++ uxs,vst)
	vizUpdate fx _      _      omask nmask vst = ([],vst)
		
	vizStatic fx []     mask vst = ([],vst)
	vizStatic fx [x:xs] mask vst
	# (vx,vst) = fx (VValue x mask) (VValue x mask) vst
	# (vxs,vst) = vizStatic fx xs mask vst
	= ([vx:vxs],vst)
	
	toTextFrag [] = ""
	toTextFrag [x] = x
	toTextFrag [x:xs] = x+++","+++toTextFrag xs

	determineReplacements items [] = []
	determineReplacements items idx
		# list = [item \\ item <- items & i <- [0..] | isMember i  idx]
		= [TUIUpdate (TUIReplace (fromJust(getId x)) x) \\ x <- list | isJust (getId x)]
	
	determineRemovals lo ln pfx
	| lo > ln
		# idx = [ln..(lo-1)]
		= [TUIUpdate (TUIRemove (pfx+++"_"+++toString x)) \\ x <- idx]
	| otherwise = []
	
	determineAdditions nviz lo ln pfx
	| lo < ln
		# el = [x \\ x <- nviz & i <- [0..] | i >= lo]
		= [TUIUpdate (TUIAdd (pfx+++"_"+++toString(lo-1)) x) \\ x <- (reverse el)]
	| otherwise = []
		
//Document Type
gVisualize {|Document|} old new vst=:{vizType, label, idPrefix, currentPath, valid, optional, useLabels}
= case vizType of
	VHtmlDisplay
		= case old of
			(VBlank) = ([HtmlFragment [(Text "No Document.")]],vst)
			(VValue ov omask) = ([HtmlFragment [ATag [(HrefAttr (buildLink ov)),(TargetAttr "_blank"),(NameAttr "x-form-document-link")] [(Text ov.fileName)], (Text (" ("+++printByteSize ov.size+++")"))]],{VSt | vst & currentPath = stepDataPath currentPath})
	VTextDisplay
		= case old of
			(VBlank) = ([TextFragment "No Document."],vst)
			(VValue ov omask) = ([TextFragment ov.fileName], {VSt | vst & currentPath = stepDataPath currentPath})
	_ 
		= case new of 
			(VValue nval nmask)
				= ([TUIFragment (TUIDocument {TUIDocument | allowUpload = True, id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = (toJSON nval), fieldLabel = label2s optional label, hideLabel = not useLabels})],
				  {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid nval optional valid})
			(VBlank)
				= case old of
					(VValue oval omask)
						= ([TUIFragment (TUIDocument {TUIDocument | allowUpload = True, id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = (toJSON oval), fieldLabel = label2s optional label, hideLabel = not useLabels})],
						{VSt | vst & currentPath = stepDataPath currentPath, valid = isValid oval optional valid})
					(VBlank)
						= ([TUIFragment (TUIDocument {TUIDocument | allowUpload = True, id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = (toJSON emptyDoc), fieldLabel = label2s optional label, hideLabel = not useLabels})],
						{VSt | vst & currentPath = stepDataPath currentPath, valid = isValid emptyDoc optional valid})	
where
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
	printByteSize size
	| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
	| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
	| otherwise 	  = toString size +++ " byte"
	
	buildLink doc = "/document/download/link/"+++doc.taskId+++"/"+++toString doc.Document.index
	
	isValid :: Document Bool Bool -> Bool
	isValid doc optional valid
	| optional 				= valid
	| doc.fileName <> "" 	= valid
	| otherwise 			= False	
			
derive gVisualize Either, Void

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

formatLabel :: String -> String
formatLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= [c \\ c <-: label]
	addspace []			= []
	addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

consSelector :: GenericConsDescriptor String DataPath (VisualizationValue a) (Maybe String) Bool -> [Visualization]
consSelector d idPrefix dp value label useLabels
	# masked = case value of (VValue _ dm) = isMasked dp dm; _ = False
	//No choice needed with only one constructor
	| d.gcd_type_def.gtd_num_conses == 1 
		= []
	//Use radiogroup to choose a constructor
	| d.gcd_type_def.gtd_num_conses <= MAX_CONS_RADIO 
		# items	= [TUIRadio {TUIRadio|name = name, value = c.gcd_name, boxLabel = Just c.gcd_name, checked = (masked && c.gcd_index == index), fieldLabel = Nothing, hideLabel = True} 
				   \\ c <- d.gcd_type_def.gtd_conses]
		= [TUIFragment (TUIRadioGroup {TUIRadioGroup|name = name, id = id, items = items, fieldLabel = label, hideLabel = not useLabels})]
	//Use combobox to choose a constructor
	| otherwise
		= [TUIFragment (TUIComboBox {TUIComboBox|name = name, id = id, value = (if masked d.gcd_name ""), fieldLabel = label, hideLabel = not useLabels, store = store, triggerAction = "all", editable = False})]
where
	
	store	= [("","Select...") : [(c.gcd_name,c.gcd_name) \\ c <- d.gcd_type_def.gtd_conses]]
	name	= dp2s dp
	id		= dp2id idPrefix dp
	index	= d.gcd_index
	
	
determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = [TUIUpdate (TUIRemove consid) \\ consid <- collectIds (coerceToTUIDefs editor)]
where
	collectIds [] = []
	collectIds [TUINumberField {TUINumberField|id}:is] = [id:collectIds is]
	collectIds [TUITextField {TUITextField|id}:is] = [id:collectIds is]
	collectIds [TUITextArea {TUITextArea|id}:is] = [id:collectIds is]
	collectIds [TUICheckBox {TUICheckBox|id}:is] = [id:collectIds is]
	collectIds [TUIRadioGroup {TUIRadioGroup|id}:is] = [id:collectIds is]
	collectIds [TUIComboBox {TUIComboBox|id}:is] = [id:collectIds is]
	collectIds [TUIFieldSet {TUIFieldSet|id}:is] = [id:collectIds is]
	collectIds [_:is] = collectIds is
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = reverse [TUIUpdate (TUIAdd consid def) \\ def <- coerceToTUIDefs editor]

//Coercion of visualizations
coerceToTUIDefs :: [Visualization] -> [TUIDef]
coerceToTUIDefs visualizations = [d \\ (TUIFragment d) <- visualizations]

coerceToTUIUpdates :: [Visualization] -> [TUIUpdate]
coerceToTUIUpdates []				  = []
coerceToTUIUpdates [(TUIUpdate u):vs] = [u:coerceToTUIUpdates vs]
coerceToTUIUpdates [(TUIFragment d):vs]
= case getId d of
	(Just id) 	= [(TUIReplace id d):coerceToTUIUpdates vs]
	Nothing		= coerceToTUIUpdates vs
coerceToTUIUpdates [v:vs]			= coerceToTUIUpdates vs

getId :: TUIDef -> Maybe TUIId
getId (TUILabel)				= Nothing
getId (TUIButton d)				= Just d.TUIButton.id
getId (TUINumberField d)		= Just d.TUINumberField.id				
getId (TUITextField d)			= Just d.TUITextField.id
getId (TUITextArea d)			= Just d.TUITextArea.id
getId (TUIComboBox d)			= Just d.TUIComboBox.id
getId (TUICheckBox d)			= Just d.TUICheckBox.id
getId (TUICheckBoxGroup d)		= Just d.TUICheckBoxGroup.id
getId (TUIRadio d)				= Nothing
getId (TUIRadioGroup d)			= Just d.TUIRadioGroup.id
getId (TUITimeField d)			= Just d.TUITimeField.id
getId (TUIDateField d)			= Just d.TUIDateField.id
getId (TUIHtmlEditor)			= Nothing
getId (TUIFieldSet d)			= Just d.TUIFieldSet.id
getId (TUIPanel d)				= Nothing
getId (TUIBox d)				= Nothing
getId (TUIHtmlPanel d)			= Just d.TUIHtmlPanel.id
getId (TUIList d)				= Just d.TUIList.id
getId (TUIListItem d)			= Just d.TUIListItem.id
getId (TUIDocument d)			= Just d.TUIDocument.id
getId (TUICustom d)				= Nothing
getId _							= abort "unknown TUI Definition"

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