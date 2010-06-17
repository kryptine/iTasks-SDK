implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum
import GenUpdate, GenEq
import Void, Either
import Text, Html, JSON, TUIDefinition
from Types import emptyDoc

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| origVizType = VTextDisplay, vizType = VTextDisplay, idPrefix = "", currentPath = shiftDataPath initialDataPath, label = Nothing, 
		useLabels = False, selectedConsIndex = -1, optional = False, valid = True, renderAsStatic = False, errorMask = [], hintMask = [], updates = []}

//Wrapper functions
visualizeAsEditor :: String (Maybe SubEditorIndex) DataMask a -> ([TUIDef],Bool) | gVisualize{|*|} a & gHint{|*|} a & gError{|*|} a
visualizeAsEditor name mbSubIdx mask x
	# emask = determineErrors x mask
	# hmask = determineHints x mask
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType = VEditorDefinition, idPrefix = name, errorMask = emask, hintMask = hmask}
	# vst = case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (defs,vst=:{valid}) = gVisualize{|*|} val val vst
	= (coerceToTUIDefs defs, valid)	
where
	val = VValue x mask
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))
where
	val = VValue x initialDataMask

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))
where
	val = VValue x initialDataMask

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
where
	val = VValue x initialDataMask
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))
where
	val = VValue x initialDataMask
	
determineEditorUpdates	:: String (Maybe SubEditorIndex) [DataPath] DataMask DataMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a & gHint{|*|} a & gError{|*|} a
determineEditorUpdates name mbSubIdx updatedPaths omask nmask old new
	//# omask = trace_n ("OLD MASK: " +++ printToString omask) omask
	//# nmask = trace_n ("NEW MASK: " +++ printToString nmask) nmask
	# emask = determineErrors new nmask
	# hmask = determineHints new nmask
	# vst 	= {mkVSt & vizType = VEditorUpdate, idPrefix = name, errorMask = emask, hintMask = hmask, updates = updatedPaths}
	# vst 	= case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (updates,vst=:{valid}) = (gVisualize{|*|} (VValue old omask) (VValue new nmask) vst)
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

gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,currentPath,valid}
	= case (old,new) of
		//Same structure:
		(VValue (LEFT ox) omask, VValue (LEFT nx) nmask)
			# oval = VValue ox omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						# (new,vst)			= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})			
					| otherwise
						# (upd,vst) = fx oval nval {vst & vizType = VEditorUpdate}
						= (upd,vst)
				_
					= fx oval nval vst
		(VValue (RIGHT oy) omask, VValue (RIGHT ny) nmask)
			# oval = VValue oy omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						# (new,vst)			= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
					| otherwise
						# (upd,vst) = fy oval nval {vst & vizType = VEditorUpdate}
						= (upd,vst)
				_
					= fy oval nval vst
		//Different structure:
		(VValue (LEFT ox) omask, VValue (RIGHT ny) nmask)
			# oval = VValue ox omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VValue (LEFT nx) nmask)
			# oval = VValue oy omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value
		
		//No value any more
		(VValue (LEFT ox) omask, VBlank)
			# oval = VValue ox omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VBlank)
			# oval = VValue oy omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value					
		
		//New value
		(VBlank, VValue (LEFT nx) nmask)
			# oval = VBlank
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VBlank, VValue (RIGHT ny) nmask)
			# oval = VBlank
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		//Default case
		_
			= fx VBlank VBlank vst		
where
	maskChanged dp m1 m2	= (isMasked dp m1 && not (isMasked dp m2)) || (not (isMasked dp m1) && isMasked dp m2)
	//pathid					= dp2id idPrefix (dataPathSetConsFlag currentPath)
	pathid					= dp2id idPrefix currentPath

gVisualize{|CONS of d|} fx old new vst=:{vizType,idPrefix,currentPath,label,useLabels,optional,valid,errorMask,hintMask}
	= case vizType of
		VEditorDefinition
			# (ox,nx) = case (old,new) of (VValue (CONS ox) omask,VValue (CONS nx) nmask)	= (VValue ox omask, VValue nx nmask); _ = (VBlank,VBlank)
			//records
			| not (isEmpty d.gcd_fields)
				= case ox of 
					VBlank 
						# errMsg = getErrorMessage currentPath oldM errorMask
						# hntMsg = getHintMessage currentPath oldM hintMask
						= ([TUIFragment (TUIRecordContainer  {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = [], optional = optional, hasValue = False, errorMsg = errMsg, hintMsg = hntMsg})]
						  , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
					_
						# (viz,vst) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						//first translate any labels.. then determine errors
						# errMsg = getErrorMessage currentPath oldM errorMask
						# hntMsg = getHintMessage currentPath oldM hintMask
						= ([TUIFragment (TUIRecordContainer {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = coerceToTUIDefs viz, optional = optional, hasValue = True, errorMsg = errMsg, hintMsg = hntMsg})]
						 , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
			//ADT's
			| otherwise
				# (viz,vst) = fx ox nx {VSt | vst & currentPath = shiftDataPath currentPath}
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional, selectedConsIndex= d.gcd_index})
		//Structure update
		VEditorUpdate
			// records
			| not (isEmpty d.gcd_fields)
				= case (old,new) of 
					(VValue (CONS ox) omask, VBlank)
						// remove components
						# (viz,vst=:{valid}) = fx (VValue ox omask) (VValue ox omask) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						= ([getErrorUpdate fsid currentPath newM errorMask, getHintUpdate fsid currentPath newM hintMask:(determineRemovals viz)]
						, {VSt | vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels})
					(VBlank, VValue (CONS nx) nmask)
						// add components
						# (viz,vst=:{valid}) = fx (VValue nx nmask) (VValue nx nmask) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						= ([getErrorUpdate fsid currentPath newM errorMask, getHintUpdate fsid currentPath newM hintMask:(determineChildAdditions ((dp2id idPrefix currentPath) +++ "-fs") viz)]
						, {VSt | vst & currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels});				
					(VValue (CONS ox) omask, VValue (CONS nx) nmask) 
						# (vizBody,vst=:{valid}) = fx (VValue ox omask) (VValue nx nmask) {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False} 
						= ([getErrorUpdate fsid currentPath newM errorMask, getHintUpdate fsid currentPath newM hintMask:vizBody]
						, {VSt|vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels})
					_
						= ([],{VSt | vst & currentPath = stepDataPath currentPath})
			//ADT's
			| otherwise
				# (viz,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
				= ([getErrorUpdate id currentPath newM errorMask, getHintUpdate id currentPath newM hintMask:viz],{VSt | vst & currentPath = stepDataPath currentPath, optional = optional})
		//Cons selector	update	
		VConsSelectorUpdate = (consSelectorUpdate new, vst)
		//Html display vizualization
		VHtmlDisplay
			= case (old,new) of
				(VValue (CONS ox) omask, VValue (CONS nx) nmask)
					# (vizBody,vst) = fx (VValue ox omask) (VValue nx nmask) {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath}
					//Records
					| not (isEmpty d.gcd_fields) 
						= ([HtmlFragment [TableTag [] (flatten (coerceToHtml vizBody))]], {VSt|vst & currentPath = stepDataPath currentPath})
					//Normal ADT's
					| otherwise
						= (vizCons ++ [TextFragment " "] ++ vizBody, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= ([],{VSt|vst & currentPath = stepDataPath currentPath})
		//Other visualizations
		_	
			# (viz,vst) = fx oldV newV {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath}
			= (viz,{VSt|vst & currentPath = stepDataPath currentPath})
where
	oldV 	= case old of (VValue (CONS ox) omask) = VValue ox omask; _ = VBlank
	newV 	= case new of (VValue (CONS nx) nmask) = VValue nx nmask; _ = VBlank  
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	
	fsid = (dp2id idPrefix currentPath)+++"-fs"
	cId = (dp2id idPrefix currentPath)+++"c"
	id = (dp2id idPrefix currentPath)
	
	//Do not show constructors that start with an underscore (_Tuple2,_Cons etc.)
	vizCons = if (d.gcd_name.[0] == '_') [] [TextFragment d.gcd_name]

	//Only show a body when you have a value and it is masked
	showBody dp VBlank			= False
	showBody dp (VValue _ dm)	= isMasked dp dm
	
	consSelectorUpdate VBlank = []
	consSelectorUpdate (VValue _ dm)
		| (isMasked currentPath dm && isEmpty  d.gcd_fields && d.gcd_type_def.gtd_num_conses > 1)
			= [TUIUpdate (TUISetValue cId d.gcd_name)]	
		| otherwise
			= []

gVisualize{|OBJECT of d|} fx old new vst=:{vizType,idPrefix,label,currentPath,selectedConsIndex = oldSelectedConsIndex,useLabels,valid,optional,renderAsStatic,errorMask,hintMask}
	//ADT's with multiple constructors
	| d.gtd_num_conses > 1
		= case vizType of 
			VEditorDefinition
				# (items,vst=:{selectedConsIndex}) 	= fx oldV newV {vst & useLabels = False, optional = False}
				# consValues = [gdc.gcd_name \\ gdc <- d.gtd_conses]
				# errMsg = getErrorMessage currentPath  oldM errorMask
				# hntMsg = getHintMessage currentPath oldM hintMask
				= ([TUIFragment (TUIConstructorControl {TUIConstructorControl
														| id = id
														, name = dp2s currentPath
														, fieldLabel = label
														, consSelIdx = if(isMasked currentPath oldM) selectedConsIndex -1
														, consValues = consValues
														, items = if(isMasked currentPath oldM) (coerceToTUIDefs items) []
														, staticDisplay = renderAsStatic
														, errorMsg = errMsg
														, hintMsg = hntMsg
														})]
				  ,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional, valid= if optional True (isMasked currentPath oldM)})
			VEditorUpdate
				| not (isMasked currentPath newM) //reset the constructor
				# (rem,vst=:{valid})	= fx oldV oldV {vst & vizType = VEditorDefinition, currentPath = currentPath}
				= ([getErrorUpdate id currentPath newM errorMask, getHintUpdate id currentPath newM hintMask, TUIUpdate (TUISetValue cId ""):determineRemovals rem]
					,{vst & vizType = vizType, currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, valid = isOptional valid})		
				| otherwise
				# (upd,vst=:{valid}) = fx oldV newV {vst & useLabels = False, optional = False}
				= ([getErrorUpdate id currentPath newM errorMask, getHintUpdate id currentPath newM hintMask:upd]
					,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, valid = hasErrors currentPath newM errorMask valid, optional = optional})			
			_
				# (viz,vst) = fx oldV newV vst
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath})
	//Everything else
	| otherwise
		= case (old,new) of
			(VValue (OBJECT ox) omask, VValue (OBJECT nx) nmask)
				= fx (VValue ox omask) (VValue nx nmask) vst
			(VValue (OBJECT ox) omask, VBlank)
				= fx (VValue ox omask) VBlank vst
			(VBlank, VValue (OBJECT nx) nmask)
				= fx VBlank (VValue nx nmask) vst
			_
				= fx VBlank VBlank	vst
where
	id		= dp2id idPrefix currentPath
	cId 	= (dp2id idPrefix currentPath)+++"c"
	oldV 	= case old of (VValue (OBJECT ox) omask) = (VValue ox omask); _ = VBlank
	newV 	= case new of (VValue (OBJECT nx) nmask) = (VValue nx nmask); _ = VBlank
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	
	isOptional valid = if optional True valid 
	
	hasErrors cp dm em valid
		| getErrorCount cp dm em > 0 = False
		| otherwise = valid

gVisualize{|FIELD of d|} fx old new vst=:{vizType,currentPath}
	# vst = determineIndexOfLabels d.gfd_name vst	
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
					# (vizBody,vst)	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= (vizBody, {VSt | vst & label = Nothing})
		_
			= fx VBlank VBlank {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,idPrefix,label,currentPath,updates,useLabels,optional,valid,renderAsStatic,errorMask}
	= case vizType of
		VEditorDefinition
			# (errMsg,hntMsg,vst) = getErrorNHintMessages oldM vst
			= ([TUIFragment (TUIIntControl {TUIBasicControl | name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV == newV) (restoreField currentPath updates id oldV) [TUIUpdate (TUISetValue id newV)]			
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# (msg,vst) = updateErrorNHintMessages newM vst
			= (upd++msg, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_	
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old
	
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
		
gVisualize{|Real|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,updates,errorMask}
	= case vizType of
		VEditorDefinition	
		# (errMsg,hntMsg,vst) = getErrorNHintMessages oldM vst
		= ([TUIFragment (TUIRealControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
			, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = if (oldV == newV) (restoreField currentPath updates id oldV) [TUIUpdate (TUISetValue id newV)]
			# (msg,vst) = updateErrorNHintMessages newM vst
			= (upd++msg, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old
	
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
		
gVisualize{|Char|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,updates}
	= case vizType of
		VEditorDefinition	
			# (errMsg,hntMsg,vst) = getErrorNHintMessages oldM vst
			= ([TUIFragment (TUICharControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV == newV) (restoreField currentPath updates id oldV) [TUIUpdate (TUISetValue id newV)]
			# (msg,vst) = updateErrorNHintMessages newM vst
			= (upd++msg, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_		
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old
	
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []

gVisualize{|Bool|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,updates}
	= case vizType of
		VEditorDefinition
			# (errMsg,hntMsg,vst) = getErrorNHintMessages oldM vst
			= ([TUIFragment (TUIBoolControl {TUIBasicControl|name = dp2s currentPath, id = id, value = toString checkedOld , fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, {VSt|vst & currentPath = stepDataPath currentPath})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (checkedOld <> checkedNew) [TUIUpdate(TUISetValue id (toString checkedNew))] (restoreField currentPath updates id (toString checkedOld))
			# (msg,vst) = updateErrorNHintMessages newM vst
			= (upd++msg
				, {VSt|vst & currentPath = stepDataPath currentPath})
		_	
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath})		
where
	id			= dp2id idPrefix currentPath
	checkedOld	= checked old
	checkedNew	= checked new
	checked b	= case b of
		VBlank			= False
		(VValue v mask) = if (isMasked currentPath mask) v False
		
	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []
	
gVisualize{|String|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,updates}
	= case vizType of
		VEditorDefinition	
		# (errMsg,hntMsg,vst) = getErrorNHintMessages oldM vst
		=	([TUIFragment (TUIStringControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
			, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV == newV) (restoreField currentPath updates id oldV) [TUIUpdate (TUISetValue id newV)]
			# (msg,vst) = updateErrorNHintMessages newM vst
			= (upd++msg		
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_			
			=	([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old

	oldM	= case old of (VValue _ omask) = omask ; _ = []
	newM	= case new of (VValue _ nmask) = nmask ; _ = []

gVisualize{|Maybe|} fx old new vst=:{vizType,idPrefix,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition
			= case (old,new) of
				(VValue (Just ox) omask, _)
					# oval = VValue ox omask
					# (viz, vst) = fx oval oval {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(VValue (Just ox) omask, VValue (Just nx) nmask)
					# (viz, vst) = fx (VValue ox omask) (VValue nx nmask) {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue (Just ox) omask, VValue Nothing nmask)
					# (viz, vst) = fx (VValue ox omask) VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue Nothing omask, VValue (Just nx) nmask)
					# (viz, vst) = fx VBlank (VValue nx nmask) {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
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
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2]})]		 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2) omask, VValue(n1,n2) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					= (viz1 ++ separator ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator = case vizType of
		VHtmlDisplay	= []
		_				= [TextFragment ", "]

gVisualize{|(,,)|} f1 f2 f3 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3) = case old of (VValue (o1,o2,o3) omask) = (VValue o1 omask, VValue o2 omask, VValue o3 omask) ; _ = (VBlank,VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,vst) = f2 v2 v2 vst
			# (viz3,vst) = f3 v3 v3 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2,viz3]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels=oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3) omask, VValue(n1,n2,n3) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator = case vizType of
		VHtmlDisplay	= []
		_				= [TextFragment ", "]

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
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2,viz3,viz4]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3,o4) omask, VValue(n1,n2,n3,n4) nmask)
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					# (viz4,vst) = f4 (VValue o4 omask) (VValue n4 nmask) vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3 ++ separator ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					# (viz4,vst) = f4 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3 ++ separator ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator = case vizType of
		VHtmlDisplay	= []
		_				= [TextFragment ", "]

gVisualize {|[]|} fx old new vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,renderAsStatic, errorMask, hintMask}
	= case vizType of
		VEditorDefinition
			# errMsg 				= getErrorMessage currentPath oldM errorMask
			# hntMsg 				= getHintMessage currentPath oldM hintMask
			# (items,vst=:{valid}) 	= TUIDef fx oldV oldM 0 {VSt | vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			= ([TUIFragment (TUIListContainer {TUIListContainer | items = items, name = name, id = id, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})],
			  {VSt | vst & currentPath = stepDataPath currentPath, label = label, useLabels = useLabels, valid=isValid currentPath oldM errorMask valid})
		VEditorUpdate
			# (updates,vst) 		= TUIUpd fx oldV newV oldM newM {VSt | vst & currentPath = shiftDataPath currentPath}
			# (newDefs,vst=:{valid})= TUIDef fx newV newM 0 {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (oldDefs,vst)			= TUIDef fx oldV oldM 0 {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (replacements)		= determineChanges oldDefs newDefs 0
			# err 					= getErrorUpdate id currentPath newM errorMask
			# hnt 					= getHintUpdate id currentPath newM hintMask
			= ([err,hnt:replacements ++ updates],
			  {VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate, label = label, useLabels = useLabels, valid=isValid currentPath newM errorMask valid})
		VHtmlDisplay
			= case oldV of
				[] 
					= ([HtmlFragment [UlTag [] [LiTag [ClassAttr "list-item-light"] [(Text "Empty list")]]]],{VSt | vst & currentPath = stepDataPath currentPath})
				_		
					# (items,vst) = staticDef fx oldV oldM {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment [UlTag [] [(LiTag [ClassAttr (itemCls i)] (flatten (coerceToHtml x))) \\ x <- items & i <- [0..]]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay
			= case oldV of
				[]
					= ([TextFragment "[]"],{VSt | vst & currentPath = stepDataPath currentPath})
				_
					# (items,vst) = staticDef fx oldV oldM {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([TextFragment ("["+++join ", " (flatten  [(coerceToStrings x) \\ x <-items])+++"]")],{VSt | vst & currentPath = stepDataPath currentPath})
		_
			= ([],
			  {VSt | vst & currentPath = stepDataPath currentPath})
where
	oldV = case old of (VValue ol om) = ol; _ = []
	oldM = case old of (VValue ol om) = om; _ = []
	newV = case new of (VValue nl nm) = nl; _ = []
	newM = case new of (VValue nl nm) = nm; _ = []

	id 			= dp2id idPrefix currentPath
	name 		= dp2s currentPath
	itemId idx 	= id+++"#"+++toString idx

	itemCls i
		| isEven i  = "list-item-light"
		| otherwise = "list-item-dark"

	TUIDef fx []     om idx vst=:{VSt | optional}
		| renderAsStatic
		= ([],vst)
		| otherwise
		# (dx,vst)  = fx VBlank VBlank {VSt | vst & optional = True}
		= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}],{VSt | vst & optional = optional})
	TUIDef fx [x:xs] om idx vst
		# (dx, vst) = fx (VValue x om) (VValue x om) vst
		# (dxs,vst) = TUIDef fx xs om (inc idx) vst
		= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}:dxs],vst)
		
	TUIUpd fx [o:os] [n:ns] om nm vst
		# (u,  vst) = fx (VValue o om) (VValue n nm) vst
		# (us, vst) = TUIUpd fx os ns om nm vst
		= (u++us,vst)
	TUIUpd _  _      _      _  _  vst = ([],vst)
	
	staticDef fx []     om vst = ([],vst)
	staticDef fx [o:os] om vst
		# (hx, vst) = fx (VValue o om) (VValue o om) vst
		# (hxs,vst) = staticDef fx os om vst
		= ([hx:hxs],vst);
			
	determineChanges []     []     idx = []
	determineChanges [o:os] []     idx = [TUIUpdate (TUIRemove (itemId idx)):determineChanges os [] (idx+1)]
	determineChanges []     [n:ns] idx = [TUIUpdate (TUIAdd (itemId (idx-1)) n):determineChanges [] ns (idx+1)]
	determineChanges [o:os] [n:ns] idx
		| o =!= n   = [TUIUpdate (TUIReplace (fromJust (getId n)) n):determineChanges os ns (idx+1)]
		| otherwise = determineChanges os ns (idx+1)
	
	isValid cp dm em valid
		| getErrorCount cp dm em > 0 = False
		| otherwise = valid
	
//Document Type
gVisualize {|Document|} old new vst=:{vizType, label, idPrefix, currentPath, valid, optional, useLabels,renderAsStatic, errorMask, updates}
= case vizType of
	VEditorDefinition
		# (errMsg,hntMsg,vst) = getErrorNHintMessages omask vst
		= ([TUIFragment (TUIDocumentControl {TUIDocumentControl |id = id, name = dp2s currentPath, docInfo = toString(toJSON oval), fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})],
		  {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid oval currentPath omask errorMask optional valid})
	VEditorUpdate
		# (msg,vst) = updateErrorNHintMessages nmask vst
		| oval =!= nval
		= ([TUIUpdate (TUISetValue id (toString (toJSON nval))):msg],
		  {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid nval currentPath nmask errorMask optional valid})
		| otherwise = ((restoreField currentPath updates id (toString (toJSON oval)))++msg,{VSt | vst & currentPath = stepDataPath currentPath})
	VHtmlDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue ov=:{content} omask) = case content of
				EmptyDocument = noDocument vst
				(DocumentContent info)
					# downLink = ATag [HrefAttr (buildLink info),TargetAttr "_blank",IdAttr id,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
					# prevLink = ATag [HrefAttr "#", IdAttr id, NameAttr "x-form-document-preview-link"][ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
					= ([HtmlFragment [(Text (info.fileName+++" ("+++printByteSize info.size+++") ")),RawText "&nbsp;",downLink,prevLink]],
				  		{VSt | vst & currentPath = stepDataPath currentPath})
	VTextDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue {content} omask) = case content of
				EmptyDocument			= noDocument vst
				(DocumentContent info)	= ([TextFragment info.fileName],
			 	  							{VSt | vst & currentPath = stepDataPath currentPath})
where
	id = dp2id idPrefix currentPath
	
	oval = case old of (VValue o om) = o; _ = emptyDoc
	nval = case new of (VValue n nm) = n; _ = emptyDoc
	
	omask = case old of (VValue o om) = om; _ = []
	nmask = case new of (VValue n nm) = nm; _ = []
	
	fixReal r = (toReal (toInt (r*100.0)))/100.0
		
	printByteSize size
	| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
	| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
	| otherwise 	  = toString size +++ " byte"
	
	noDocument vst = ([TextFragment "No Document."],vst)
	buildLink info
		# location = case info.dataLocation of
			LocalLocation taskId	= taskId
			SharedLocation sid _	= "shared_" +++ sid
		= "/document/download/link/" +++ location +++ "/" +++ toString info.DocumentInfo.index
	
	isValid :: Document DataPath DataMask ErrorMask Bool Bool -> Bool
	isValid doc cp dm em optional valid
	| optional || not (isEmptyDoc doc)	= valid
	| getErrorCount cp dm em > 0		= False
	| otherwise							= False

//Hidden type
gVisualize{|Hidden|} fx old new vst=:{VSt | currentPath}
	= ([],{VSt | vst & currentPath = stepDataPath currentPath})

gVisualize{|HtmlDisplay|} fx old new vst=:{VSt | origVizType, vizType, currentPath, renderAsStatic}
	= case origVizType of
		VHtmlDisplay
			# (def,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
			= (def,{VSt | vst & currentPath = stepDataPath currentPath})
		_
			# (def,vst) = fx oldV newV {VSt | vst &  renderAsStatic = True, currentPath = shiftDataPath currentPath}
			= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
where 
	oldV = case old of (VValue (HtmlDisplay ov) om) = (VValue ov om); _ = VBlank
	newV = case new of (VValue (HtmlDisplay nv) nm) = (VValue nv nm); _ = VBlank

gVisualize{|Editable|} fx old new vst=:{VSt | vizType, currentPath, renderAsStatic}
	# (def,vst) = fx oldV newV {VSt | vst & renderAsStatic = False, currentPath = shiftDataPath currentPath}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
where 
	oldV = case old of (VValue (Editable ov) om) = (VValue ov om); _ = VBlank
	newV = case new of (VValue (Editable nv) nm) = (VValue nv nm); _ = VBlank

gVisualize{|VisualizationHint|} fx old new vst=:{VSt | idPrefix, vizType, origVizType, currentPath, renderAsStatic}
	= case origVizType of
		VHtmlDisplay
			= case old of
				(VValue (VHHidden _) _) 
					= ([],{VSt | vst & currentPath = stepDataPath currentPath})
				_
					# (viz,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, vizType = VHtmlDisplay}
					= (viz,{vst & currentPath = stepDataPath currentPath, vizType = vizType})
		VEditorUpdate
			= case (old,new) of		
				//_, hidden -> replace with hidden
				(_,(VValue (VHHidden _) _))
					# path = shiftDataPath currentPath
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s shiftPath, id = dp2id idPrefix shiftPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,{VSt | vst & currentPath = stepDataPath currentPath})
				//hidden, html -> replace with static
				((VValue (VHHidden _) _),(VValue (VHHtmlDisplay _) _))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//hidden, edit = replace with editable
				((VValue (VHHidden _) _),(VValue (VHEditable _) _))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//html, edit -> replace
				((VValue (VHHtmlDisplay _) _),(VValue (VHEditable _) _))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//edit, html -> replace
				((VValue (VHEditable _) _),(VValue (VHHtmlDisplay _) _))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//_ -> update	
				_	
					# (upd,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
					= (upd,{VSt | vst & currentPath = stepDataPath currentPath})	
		_
			= case old of
				(VValue (VHHidden _) _)
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s shiftPath, id = dp2id idPrefix shiftPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue (VHHtmlDisplay _) _)
					# (viz,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				(VValue (VHEditable _) _)
					# (viz,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})			
where
	oldV = case old of (VValue (VHEditable ox) omask) = (VValue ox omask); (VValue (VHHtmlDisplay ox) omask) = (VValue ox omask); _ = VBlank
	newV = case new of (VValue (VHEditable nx) nmask) = (VValue nx nmask); (VValue (VHHtmlDisplay nx) nmask) = (VValue nx nmask); _ = VBlank
	
	id = dp2id idPrefix currentPath	
	shiftPath = shiftDataPath currentPath	
	
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

labelAttr :: !Bool !(Maybe String) -> Maybe String
labelAttr False	_		= Nothing
labelAttr True	Nothing	= Just ""
labelAttr True	l		= l 

//OBSOLETE
label2s :: Bool (Maybe String) -> Maybe String
label2s _		Nothing		= Nothing
label2s True	(Just l)	= Just (l +++ " (optional)")
label2s False	(Just l)	= Just l

stillValid :: DataPath ErrorMask (VisualizationValue a) Bool Bool -> Bool
stillValid dp em val optional valid
	# dm = case val of (VValue _ m) = m ; _ = initialDataMask
	| getErrorCount dp dm em > 0= False //If there is an error, the form is invalid. Regardless any optional fields.
	| optional					= valid //Nothing changes
	| not (isMasked dp dm)		= False
	| otherwise					= valid	//A non-optional field must be masked to be valid

//	| optional				= trace_n (printToString dp +++ " OPT") valid 	//Nothing changes
//	| not (isMasked dp dm)	= trace_n (printToString dp +++ " BAD") False
//	| otherwise				= trace_n (printToString dp +++ " OK") valid	//A non-optional field must be masked to be valid

formatLabel :: String -> String
formatLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= [c \\ c <-: label]
	addspace []			= []
	addspace [c:cs]
		| c == '_'			= [' ':addspace cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = ([TUIUpdate (TUIRemove (fromJust (getId consid))) \\ consid <- (coerceToTUIDefs editor) | isJust (getId consid)])
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = reverse [TUIUpdate (TUIAdd consid def) \\ def <- coerceToTUIDefs editor]

determineChildAdditions :: String [Visualization] -> [Visualization]
determineChildAdditions consid editor = [TUIUpdate (TUIAddTo consid def) \\ def <- coerceToTUIDefs editor]

import StdDebug
derive gPrint LabelOrNumber
derive gPrint MessagePredicate

// === Error & Hint Utility functions ===
getErrorNHintMessages :: !DataMask !*VSt -> (String, String, *VSt)
getErrorNHintMessages mask vst=:{renderAsStatic,optional,currentPath,errorMask,hintMask}
	# errMsg = if(renderAsStatic || (optional && not (isMasked currentPath mask))) "" (getErrorMessage currentPath mask errorMask)
	# hntMsg = if(renderAsStatic) "" (getHintMessage currentPath mask hintMask)
	= (errMsg,hntMsg,vst)
			
updateErrorNHintMessages :: !DataMask !*VSt -> ([Visualization], *VSt)
updateErrorNHintMessages mask vst=:{renderAsStatic, optional, currentPath, idPrefix, errorMask, hintMask}
	//# upd = if(renderAsStatic || (optional && not (isMasked currentPath mask))) [] [TUIUpdate (TUISetError id (getErrorMessage currentPath mask errorMask))]
	# upd = if(renderAsStatic) [] [TUIUpdate (TUISetError id (getErrorMessage currentPath mask errorMask))]
	# upd = if(renderAsStatic) upd [TUIUpdate (TUISetHint id (getHintMessage currentPath mask hintMask)):upd]
	= (upd,vst)
where
	id = dp2id idPrefix currentPath
	
getHintUpdate :: TUIId DataPath DataMask HintMask -> Visualization
getHintUpdate id cp dm hm = TUIUpdate (TUISetHint id (getHintMessage cp dm hm))

getErrorUpdate :: TUIId DataPath DataMask ErrorMask -> Visualization
getErrorUpdate id cp dm em = TUIUpdate (TUISetError id (getErrorMessage cp dm em))

determineIndexOfLabels :: !String !*VSt -> *VSt
determineIndexOfLabels label vst=:{VSt | errorMask,hintMask,currentPath}
	# curPath 	= [Label label:[Unlabeled i \\ i <- tl (dataPathList currentPath)]]
	# pos		= hd (dataPathList currentPath)
	# hntMask	= [(translateLDP curPath ldp pos,p,msg) \\ (ldp,p,msg) <- hintMask]
	# errMask	= [(translateLDP curPath ldp pos,p,msg) \\ (ldp,p,msg) <- errorMask]	
	= {VSt | vst & hintMask = hntMask, errorMask = errMask}
where
	translateLDP currPath maskPath pos
		# currPath = reverse currPath
		# maskPath = reverse maskPath
		| tlEqual currPath maskPath
			# maskPath = updateAt ((length currPath)-1) (Unlabeled pos) maskPath
			= reverse maskPath
		| otherwise
			= reverse maskPath

	tlEqual _		[]		= False
	tlEqual [c] 	[m:_] 	= c == m
	tlEqual [c:cs]	[m:ms] 	= (c == m) && tlEqual cs ms 

instance == LabelOrNumber
where
	(==) (Unlabeled a) (Unlabeled b) = a == b
	(==) (Label a) (Label b) = a == b
	(==) _ _ = False



//Sends the TUIRestore-update if a field has received an update, but it should not be updated.
restoreField :: DataPath [DataPath] String String -> [Visualization]
restoreField currpath updates id val  = if (isMember currpath updates) [TUIUpdate (TUISetValue id val)] []

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
getId (TUIStringControl d)		= Just d.TUIBasicControl.id
getId (TUICharControl d)		= Just d.TUIBasicControl.id
getId (TUIIntControl d)			= Just d.TUIBasicControl.id
getId (TUIRealControl d)		= Just d.TUIBasicControl.id
getId (TUIBoolControl d)		= Just d.TUIBasicControl.id
getId (TUINoteControl d)		= Just d.TUIBasicControl.id
getId (TUIDateControl d)		= Just d.TUIBasicControl.id
getId (TUITimeControl d)		= Just d.TUIBasicControl.id
getId (TUICurrencyControl d)	= Just d.TUICurrencyControl.id
getId (TUIUserControl d)		= Just d.TUIBasicControl.id
getId (TUIPasswordControl d)	= Just d.TUIBasicControl.id
getId (TUIDocumentControl d)	= Just d.TUIDocumentControl.id
getId (TUIConstructorControl d)	= Just d.TUIConstructorControl.id
getId (TUIListItemControl d)	= Just d.TUIListItemControl.id
getId (TUITupleContainer d)		= Just d.TUITupleContainer.id
getId (TUIRecordContainer d)	= Just d.TUIRecordContainer.id
getId (TUIListContainer d)		= Just d.TUIListContainer.id

getId (TUILabel)				= Nothing
getId (TUIButton d)				= Just d.TUIButton.id
getId (TUINumberField d)		= Just d.TUINumberField.id				
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
getId (TUICustom d)				= Nothing
getId (TUITuple d)				= Just d.TUITuple.id
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