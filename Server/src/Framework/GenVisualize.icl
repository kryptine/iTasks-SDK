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
		useLabels = False, selectedConsIndex = -1, optional = False, valid = True, listMask = initialListMask, renderAsStatic = False, errorMask = [], hintMask = []}

//Wrapper functions
visualizeAsEditor :: String (Maybe SubEditorIndex) DataMask a -> ([TUIDef],Bool) | gVisualize{|*|} a & gHint{|*|} a & gError{|*|} a
visualizeAsEditor name mbSubIdx mask x
	# emask = determineErrors x mask
	# hmask = determineHints x mask
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType = VEditorDefinition, idPrefix = name, errorMask = emask, hintMask = hmask}
	# vst = case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (defs,rh,vst=:{valid}) = gVisualize{|*|} val val vst
	= (coerceToTUIDefs defs, valid)	
where
	val = VValue x mask
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst3 (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))
where
	val = VValue x initialDataMask

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst3 (gVisualize{|*|} val val {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))
where
	val = VValue x initialDataMask

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst3 (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
where
	val = VValue x initialDataMask
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst3 (gVisualize{|*|} val val {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))
where
	val = VValue x initialDataMask
	
determineEditorUpdates	:: String (Maybe SubEditorIndex) DataMask DataMask ListMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a & gHint{|*|} a & gError{|*|} a
//visualizeAsEditor name mbSubIdx mask x
determineEditorUpdates name mbSubIdx omask nmask lmask old new
	//# omask = trace_n ("OLD MASK: " +++ printToString omask) omask
	//# nmask = trace_n ("NEW MASK: " +++ printToString nmask) nmask
	//# lmask = trace_n ("LST MASK: " +++ printToString lmask) lmask
	# emask = determineErrors new nmask
	# hmask = determineHints new nmask
	# vst 	= {mkVSt & vizType = VEditorUpdate, idPrefix = name, listMask = lmask, errorMask = emask, hintMask = hmask}
	# vst 	= case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (updates,rh,vst=:{valid}) = (gVisualize{|*|} (VValue old omask) (VValue new nmask) vst)
	= (coerceToTUIUpdates updates, valid)

//Bimap for visualization values
derive bimap VisualizationValue

//Generic visualizer
generic gVisualize a :: (VisualizationValue a) (VisualizationValue a) *VSt -> ([Visualization], RenderingHint, *VSt)

gVisualize{|UNIT|} _ _ vst
	= ([],0,vst)

gVisualize{|PAIR|} fx fy old new vst
	= case (old,new) of
		(VValue (PAIR ox oy) omask, VValue (PAIR nx ny) nmask)
			# (vizx, rhx, vst) = fx (VValue ox omask) (VValue nx nmask) vst
			# (vizy, rhy, vst) = fy (VValue oy omask) (VValue ny nmask) vst
			= (vizx ++ vizy, rhx+rhy, vst)
		_
			# (vizx, rhx, vst) = fx VBlank VBlank vst
			# (vizy, rhy, vst) = fy VBlank VBlank vst
			= (vizx ++ vizy, rhx+rhy, vst)

gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,currentPath,valid}
	= case (old,new) of
		//Same structure:
		(VValue (LEFT ox) omask, VValue (LEFT nx) nmask)
			# oval = VValue ox omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (consSelUpd,_,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,rho,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						# (new,rhn,vst)			= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})			
					| otherwise
						# (upd,rh,vst) = fx oval nval {vst & vizType = VEditorUpdate}
						= (upd,rh,vst)
				_
					= fx oval nval vst
		(VValue (RIGHT oy) omask, VValue (RIGHT ny) nmask)
			# oval = VValue oy omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					| maskChanged currentPath omask nmask
						# (consSelUpd,_,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,rho,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						# (new,rhn,vst)			= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})
					| otherwise
						# (upd,rh,vst) = fy oval nval {vst & vizType = VEditorUpdate}
						= (upd,rh,vst)
				_
					= fy oval nval vst
		//Different structure:
		(VValue (LEFT ox) omask, VValue (RIGHT ny) nmask)
			# oval = VValue ox omask
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,_,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,rho,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VValue (LEFT nx) nmask)
			# oval = VValue oy omask
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,_,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,rho,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value
		
		//No value any more
		(VValue (LEFT ox) omask, VBlank)
			# oval = VValue ox omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,rho,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new, rhn, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy) omask, VBlank)
			# oval = VValue oy omask
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,rho,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new, rhn, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value					
		
		//New value
		(VBlank, VValue (LEFT nx) nmask)
			# oval = VBlank
			# nval = VValue nx nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,_,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,rho,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VBlank, VValue (RIGHT ny) nmask)
			# oval = VBlank
			# nval = VValue ny nmask
			= case vizType of
				VEditorUpdate
					# (consSelUpd,_,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,rho,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,rhn,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, rhn, {vst & vizType = VEditorUpdate})
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
						# errMsg = getErrorMessage currentPath errorMask
						# hntMsg = getHintMessage currentPath hintMask
						= ([TUIFragment (TUIRecordContainer  {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = [], optional = optional, hasValue = False, errorMsg = errMsg, hintMsg = hntMsg})]
						  , 0 //fieldset is always full width (rh = 0)
						  , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
					_
						# (viz,rh,vst) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						//first translate any labels.. then determine errors
						# errMsg = getErrorMessage currentPath errorMask
						# hntMsg = getHintMessage currentPath hintMask
						= ([TUIFragment (TUIRecordContainer {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = coerceToTUIDefs viz, optional = optional, hasValue = True, errorMsg = errMsg, hintMsg = hntMsg})]
						  , 0 //fieldset is always full width (rh = 0)
						  , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
			//ADT's
			| otherwise
				# (viz,rh,vst) = fx ox nx {VSt | vst & currentPath = shiftDataPath currentPath}
				= (viz,rh,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional, selectedConsIndex= d.gcd_index})
		//Structure update
		VEditorUpdate
			// records
			| not (isEmpty d.gcd_fields)
				= case (old,new) of 
					(VValue (CONS ox) omask, VBlank)
						// remove components
						# (viz,rh,vst=:{valid}) = fx (VValue ox omask) (VValue ox omask) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						= ([getErrorUpdate fsid currentPath errorMask, getHintUpdate fsid currentPath hintMask:(determineRemovals viz)],rh,{VSt | vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels});
					(VBlank, VValue (CONS nx) nmask)
						// add components
						# (viz,rh,vst=:{valid}) = fx (VValue nx nmask) (VValue nx nmask) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False}
						= ([getErrorUpdate fsid currentPath errorMask, getHintUpdate fsid currentPath hintMask:(determineChildAdditions ((dp2id idPrefix currentPath) +++ "-fs") viz)],rh,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels});				
					(VValue (CONS ox) omask, VValue (CONS nx) nmask) 
						# (vizBody,rh, vst=:{valid}) = fx (VValue ox omask) (VValue nx nmask) {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False} 
						= ([getErrorUpdate fsid currentPath errorMask, getHintUpdate fsid currentPath hintMask:vizBody], 0, {VSt|vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels})
					_
						= ([],0,{VSt | vst & currentPath = stepDataPath currentPath})
			//ADT's
			| otherwise
				# (viz,rh,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
				= ([getErrorUpdate id currentPath errorMask, getHintUpdate id currentPath hintMask:viz],rh,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional})
		//Cons selector	update	
		VConsSelectorUpdate = (consSelectorUpdate new, undef, vst)
		//Html display vizualization
		VHtmlDisplay
			= case (old,new) of
				(VValue (CONS ox) omask, VValue (CONS nx) nmask)
					# (vizBody,rh, vst) = fx (VValue ox omask) (VValue nx nmask) {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath}
					//Records
					| not (isEmpty d.gcd_fields) 
						= ([HtmlFragment [TableTag [] (flatten (coerceToHtml vizBody))]],rh,{VSt|vst & currentPath = stepDataPath currentPath})
					//Normal ADT's
					| otherwise
						= (vizCons ++ [TextFragment " "] ++ vizBody,rh,{VSt|vst & currentPath = stepDataPath currentPath})
				_
					= ([],0,{VSt|vst & currentPath = stepDataPath currentPath})
		//Other visualizations
		_	
			# (viz,rh,vst) = fx oldV newV {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath}
			= (viz,0,{VSt|vst & currentPath = stepDataPath currentPath})
where
	oldV = case old of (VValue (CONS ox) omask) = VValue ox omask; _ = VBlank
	newV = case new of (VValue (CONS nx) nmask) = VValue nx nmask; _ = VBlank  
	
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
				# (items,rh,vst=:{selectedConsIndex}) 	= fx oldV newV {vst & useLabels = False, optional = False}
				# consValues = [gdc.gcd_name \\ gdc <- d.gtd_conses]
				# errMsg = getErrorMessage currentPath errorMask
				# hntMsg = getHintMessage currentPath hintMask
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
				  ,0
				  ,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional, valid= if optional True (isMasked currentPath oldM)})
			VEditorUpdate
				| not (isMasked currentPath newM) //reset the constructor
				# (rem,rh,vst=:{valid})	= fx oldV oldV {vst & vizType = VEditorDefinition, currentPath = currentPath}
				= ([getErrorUpdate id currentPath errorMask, getHintUpdate id currentPath hintMask, TUIUpdate (TUISetValue cId ""):determineRemovals rem]
					,rh
					,{vst & vizType = vizType, currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, valid = isOptional valid})		
				| otherwise
				# (upd,rh,vst) = fx oldV newV {vst & useLabels = False, optional = False}
				= ([getErrorUpdate id currentPath errorMask, getHintUpdate id currentPath hintMask:upd]
					,rh
					,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional})			
			_
				# (viz,rh,vst) = fx oldV newV vst
				= (viz,rh,{VSt | vst & currentPath = stepDataPath currentPath})
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

gVisualize{|FIELD of d|} fx old new vst=:{vizType,hintMask,errorMask,currentPath}
	# vst = determineIndexOfLabels d.gfd_name vst	
	= case (old,new) of
		(VValue (FIELD ox) omask, VValue (FIELD nx) nmask)
			= case vizType of
				VHtmlDisplay
					# (vizBody,rh,vst) 	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Nothing}
					= ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],rh,{VSt | vst & label = Nothing})
				VTextDisplay
					# (vizBody,rh,vst) 	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody],rh, {VSt | vst & label = Nothing})
				_
					# (vizBody,rh,vst)	= fx (VValue ox omask) (VValue nx nmask) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= (vizBody,rh, {VSt | vst & label = Nothing})
		_
			= fx VBlank VBlank {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition
			# hntMsg = if (renderAsStatic) "" (getHintMessage currentPath hintMask)								
			# errMsg = if (renderAsStatic || (optional && oldV == "")) "" (getErrorMessage currentPath errorMask)
			= ([TUIFragment (TUIIntControl {TUIBasicControl | name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV == newV) [] [TUIUpdate (TUISetValue id newV)]
			# upd = if (renderAsStatic) upd [getHintUpdate id currentPath hintMask:upd]
			# upd = if (renderAsStatic || (optional && newV == "")) upd [getErrorUpdate id currentPath errorMask:upd]
			= (upd, 1, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_	
			= ([TextFragment (toString old)]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old
	
gVisualize{|Real|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
		# errMsg = getErrorMessage currentPath errorMask
		# hntMsg = getHintMessage currentPath hintMask
		= ([TUIFragment (TUIRealControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
			, 1
			, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV == newV) [] [TUIUpdate (TUISetValue id newV)]
			# upd = if (renderAsStatic) upd [getHintUpdate id currentPath hintMask:upd]
			# upd = if (renderAsStatic || (optional && newV == "")) upd [getErrorUpdate id currentPath errorMask:upd]
			= (upd, 1, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old
		
gVisualize{|Char|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath errorMask
			# hntMsg = getHintMessage currentPath hintMask
			= ([TUIFragment (TUICharControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV == newV) [] [TUIUpdate (TUISetValue id newV)]
			# upd = if (renderAsStatic) upd [getHintUpdate id currentPath hintMask: upd]
			# upd = if (renderAsStatic || (optional && newV == "")) upd [getErrorUpdate id currentPath errorMask:upd]
			= (upd, 1, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_		
			= ([TextFragment (toString old)]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old

gVisualize{|Bool|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition
			# errMsg = getErrorMessage currentPath errorMask
			# hntMsg = getHintMessage currentPath hintMask
			= ([TUIFragment (TUIBoolControl {TUIBasicControl|name = dp2s currentPath, id = id, value = toString checkedOld , fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (checkedOld <> checkedNew) [TUIUpdate(TUISetValue id (toString checkedNew))] []
			# err = getErrorUpdate id currentPath errorMask
			# hnt = getHintUpdate id currentPath hintMask
			= ([err,hnt:upd]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath})
		_	
			= ([TextFragment (toString old)]
				, 1
				, {VSt|vst & currentPath = stepDataPath currentPath})		
where
	id			= dp2id idPrefix currentPath
	checkedOld	= checked old
	checkedNew	= checked new
	checked b	= case b of
		VBlank			= False
		(VValue v mask) = if (isMasked currentPath mask) v False
	
gVisualize{|String|} old new vst=:{vizType,idPrefix,label,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
		# errMsg = getErrorMessage currentPath errorMask
		# hntMsg = getHintMessage currentPath hintMask
		=	([TUIFragment (TUIStringControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
			, 3
			, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			//always include the most recent error and/or hint, regardless whether the value has changed, because other fields may have changed the context
			# upd = if (oldV <> newV) [TUIUpdate (TUISetValue id newV)] []
			# err = getErrorUpdate id currentPath errorMask
			# hnt = getHintUpdate id currentPath hintMask
			= ([err,hnt:upd]		
				, 3
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
		_			
			=	([TextFragment (toString old)]
				, 3
				, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath errorMask new optional valid})
where
	id		= dp2id idPrefix currentPath
	newV	= value2s currentPath new
	oldV	= value2s currentPath old

gVisualize{|Maybe|} fx old new vst=:{vizType,idPrefix,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition
			= case (old,new) of
				(VValue (Just ox) omask, _)
					# oval = VValue ox omask
					# (viz, rh, vst) = fx oval oval {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, rh, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(VValue (Just ox) omask, VValue (Just nx) nmask)
					# (viz, rh, vst) = fx (VValue ox omask) (VValue nx nmask) {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue (Just ox) omask, VValue Nothing nmask)
					# (viz, rh, vst) = fx (VValue ox omask) VBlank {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue Nothing omask, VValue (Just nx) nmask)
					# (viz, rh, vst) = fx VBlank (VValue nx nmask) {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, rh, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, rh, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		_			
			= case old of
				(VValue Nothing m)	= ([TextFragment "-"],0,vst)
				(VValue (Just x) m)	= fx (VValue x m) (VValue x m) vst
				VBlank				= ([],0,vst)
where
	pathid = dp2id idPrefix currentPath		

gVisualize{|Dynamic|} old new vst
	= ([],0,vst)

gVisualize{|(,)|} f1 f2 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2) = case old of (VValue (o1,o2) omask) = (VValue o1 omask, VValue o2 omask) ; _ = (VBlank,VBlank)
			# (viz1,rh1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,rh2,vst) = f2 v2 v2 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, definitions = [coerceToTUIDefs viz1, coerceToTUIDefs viz2]})]		 
			  , 0
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2) omask, VValue(n1,n2) nmask)
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,rh2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					= (viz1 ++ separator ++ viz2,6,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,rh2,vst) = f2 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2,6,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator = case vizType of
		VHtmlDisplay	= []
		_				= [TextFragment ", "]

gVisualize{|(,,)|} f1 f2 f3 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional}
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3) = case old of (VValue (o1,o2,o3) omask) = (VValue o1 omask, VValue o2 omask, VValue o3 omask) ; _ = (VBlank,VBlank,VBlank)
			# (viz1,rh1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,rh2,vst) = f2 v2 v2 vst
			# (viz3,rh3,vst) = f3 v3 v3 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, definitions = [coerceToTUIDefs viz1,coerceToTUIDefs viz2,coerceToTUIDefs viz3]})]			 
			  , 0
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels=oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3) omask, VValue(n1,n2,n3) nmask)
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,rh2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,rh3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3,4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,rh2,vst) = f2 VBlank VBlank vst
					# (viz3,rh3,vst) = f3 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3,4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
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
			# (viz1,rh1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
			# (viz2,rh2,vst) = f2 v2 v2 vst
			# (viz3,rh3,vst) = f3 v3 v3 vst
			# (viz4,rh4,vst) = f4 v4 v4 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, definitions = [coerceToTUIDefs viz1,coerceToTUIDefs viz2,coerceToTUIDefs viz3,coerceToTUIDefs viz4]})]			 
			  , 0
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3,o4) omask, VValue(n1,n2,n3,n4) nmask)
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 (VValue o1 omask) (VValue n1 nmask) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing}
					# (viz2,rh2,vst) = f2 (VValue o2 omask) (VValue n2 nmask) vst
					# (viz3,rh3,vst) = f3 (VValue o3 omask) (VValue n3 nmask) vst
					# (viz4,rh4,vst) = f4 (VValue o4 omask) (VValue n4 nmask) vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3 ++ separator ++ viz4,4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
				_
					# oldLabels = useLabels
					# (viz1,rh1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,rh2,vst) = f2 VBlank VBlank vst
					# (viz3,rh3,vst) = f3 VBlank VBlank vst
					# (viz4,rh4,vst) = f4 VBlank VBlank vst
					= (viz1 ++ separator ++ viz2 ++ separator ++ viz3 ++ separator ++ viz4,4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator = case vizType of
		VHtmlDisplay	= []
		_				= [TextFragment ", "]
	
gVisualize {|[]|} fx old new vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,listMask,renderAsStatic}
	= case vizType of		
		VEditorDefinition
			= case old of
				(VValue [] omask)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s (dataPathSetConsFlag currentPath), id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath}) 
				(VValue ov omask)
					# (viz,rh,vst) = vizEditor fx ov omask 0 (dp2id idPrefix currentPath) (dp2s (dataPathSetConsFlag currentPath)) {VSt | vst & currentPath = shiftDataPath currentPath, vizType=VEditorDefinition}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s (dataPathSetConsFlag currentPath), id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s (dataPathSetConsFlag currentPath), id= dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(_, VBlank) 
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s (dataPathSetConsFlag currentPath), id= dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					0,
					{VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank,(VValue nv nmask)) 
					# (viz, rh, vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s (dataPathSetConsFlag currentPath)) {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, optional = False}
					# vst = {VSt | vst & optional = optional}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s (dataPathSetConsFlag currentPath), id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath})
				(VValue [] omask, VValue nv nmask)
					# (viz, rh, vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s (dataPathSetConsFlag currentPath)) {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorDefinition, optional=False}
					# vst = {VSt | vst & optional = optional}
					= ([TUIFragment (TUIList {TUIList | items = viz, name = dp2s (dataPathSetConsFlag currentPath), id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate})
				(VValue ov omask, VValue [] nmask)
					= ([TUIFragment (TUIList {TUIList | items = [], name = dp2s (dataPathSetConsFlag currentPath), id = dp2id idPrefix currentPath, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic})],
					  0,
					  {VSt | vst & currentPath = stepDataPath currentPath})  
				(VValue ov omask, VValue nv nmask)						
					# (nupd,rh,vst) = vizUpdate fx ov nv omask nmask {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorUpdate}
					# (nviz,rh,vst) = vizEditor fx nv nmask 0 (dp2id idPrefix currentPath) (dp2s (dataPathSetConsFlag currentPath)) {VSt | vst & currentPath = shiftDataPath currentPath, vizType = VEditorDefinition, optional=False}
					# vst			= {VSt | vst & optional = optional, useLabels = useLabels, label = label}
					# lo			= length ov
					# ln  			= length nv
					# (lmask,idx)	= updateListMask listMask currentPath
					# idx 		 	= [x \\ x <- idx | x < length ov && x < length nv]
					# rplc 		 	= determineReplacements nviz idx
					# rem  			= determineRemovals lo ln (dp2id idPrefix currentPath)
					# add  		 	= determineAdditions nviz lo ln (dp2id idPrefix currentPath)
					= (nupd++rplc++add++rem,0,{VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate})	
		VHtmlDisplay
			= case old of
				(VValue [] omask)
					= ([HtmlFragment [ITag [] [(Text "Empty list")]]],0,{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue ov omask)
					# (viz,rh,vst) = vizStatic fx ov omask {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment [UlTag [ClassAttr "listDisplay"] [(LiTag [ClassAttr "listDisplay"] (flatten (coerceToHtml x))) \\ x <- viz]]],0,{VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([HtmlFragment [(Text "-")]],0,{VSt | vst & currentPath = stepDataPath currentPath})	
		_
			= case old of
				(VValue [] omask)
					= ([TextFragment "Empty list"],0,{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue ov omask)
					# (viz,rh,vst) = vizStatic fx ov omask {VSt | vst & currentPath = shiftDataPath currentPath}
					# strings      = flatten [(coerceToStrings x) \\ x <-viz]
					= ([TextFragment (toTextFrag strings)],0,{VSt | vst & currentPath = stepDataPath currentPath})
				(VBlank)
					= ([TextFragment "-"],0,{VSt | vst & currentPath = stepDataPath currentPath})	 					
where
	vizEditor fx []     mask index pfx name vst = ([],[],vst)
	vizEditor fx [x:xs] mask index pfx name vst=:{label,useLabels}
	# (vx,rh,vst) 	= fx (VValue x mask) (VValue x mask) {VSt | vst & label = Nothing, useLabels = False} //Don't display any labels.
	# tx			= (TUIListItem {TUIListItem | items=coerceToTUIDefs vx, index=index, name = name, id=pfx+++"_"+++toString index})
	# (txs,rhs,vst)	= vizEditor fx xs mask (index+1) pfx name vst
	= ([tx:txs],[rh:rhs],{VSt | vst & label = label, useLabels = useLabels})
	
	vizUpdate fx [o:os] [n:ns] omask nmask vst
		# (ux,rh,vst)   = fx (VValue o omask) (VValue n nmask) {VSt | vst & label = Nothing, useLabels = False}
		# (uxs,rhs,vst) = vizUpdate fx os ns omask nmask vst
		= (ux ++ uxs,[rh:rhs],vst)
	vizUpdate fx _      _      omask nmask vst = ([],[],vst)
		
	vizStatic fx []     mask vst = ([],[],vst)
	vizStatic fx [x:xs] mask vst
	# (vx,rh,vst)   = fx (VValue x mask) (VValue x mask) {VSt | vst & label = Nothing, useLabels = False}
	# (vxs,rhs,vst) = vizStatic fx xs mask vst
	= ([vx:vxs],[rh:rhs],vst)
	
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

//UserName type
gVisualize{|UserName|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic,errorMask,hintMask}
	= case vizType of
		VEditorDefinition	
			# errMsg = getErrorMessage currentPath errorMask
			# hntMsg = getHintMessage currentPath hintMask
			= ([TUIFragment (TUIUsernameControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask old optional valid})
		VEditorUpdate
			# upd = [TUIUpdate (TUISetValue id newV)]
			# err = getErrorUpdate id currentPath errorMask
			# hnt = getHintUpdate id currentPath hintMask
			= ([err,hnt:upd]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
		_					
			= ([TextFragment (toString old)]
				, 2
				, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath errorMask new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	id			= dp2id idPrefix currentPath
	oldV		= value2s currentPath old
	newV		= value2s currentPath new
	
//Document Type
gVisualize {|Document|} old new vst=:{vizType, label, idPrefix, currentPath, valid, optional, useLabels,renderAsStatic, errorMask, hintMask}
= case vizType of
	VHtmlDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue ov=:{content} omask) = case content of
				EmptyDocument = noDocument vst
				(DocumentContent info)
					# downLink = ATag [HrefAttr (buildLink info),TargetAttr "_blank",IdAttr (dp2id idPrefix currentPath),NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
					# prevLink = ATag [HrefAttr "#", IdAttr (dp2id idPrefix currentPath), NameAttr "x-form-document-preview-link"][ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
					= ([HtmlFragment [(Text (info.fileName+++" ("+++printByteSize info.size+++") ")),RawText "&nbsp;",downLink,prevLink]],
				  		3,
				  		{VSt | vst & currentPath = stepDataPath currentPath})
	VTextDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue {content} omask) = case content of
				EmptyDocument			= noDocument vst
				(DocumentContent info)	= ([TextFragment info.fileName],
				  							3,
			 	  							{VSt | vst & currentPath = stepDataPath currentPath})
	_ 
		# errMsg = getErrorMessage currentPath errorMask
		# hntMsg = getHintMessage currentPath hintMask
		= case new of 
			(VValue nval=:{content} nmask)			
				= ([TUIFragment (TUIDocumentControl {TUIDocumentControl | id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = toJSON nval, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})],
				  4,
				  {VSt | vst & currentPath = stepDataPath currentPath, valid = isValid nval currentPath errorMask optional valid})
			(VBlank)
				= case old of
					(VValue oval=:{content} omask)
						= ([TUIFragment (TUIDocumentControl {TUIDocumentControl |id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = toJSON oval, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})],
						4,
						{VSt | vst & currentPath = stepDataPath currentPath, valid = isValid oval currentPath errorMask optional valid})
					(VBlank)
						= ([TUIFragment (TUIDocumentControl {TUIDocumentControl |id = dp2id idPrefix currentPath, name = dp2s currentPath, docInfo = toJSON emptyDoc, fieldLabel = label2s optional label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = errMsg, hintMsg = hntMsg})],
						4,
						{VSt | vst & currentPath = stepDataPath currentPath, valid = isValid emptyDoc currentPath errorMask optional valid})	
where
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
	printByteSize size
	| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
	| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
	| otherwise 	  = toString size +++ " byte"
	
	noDocument vst = ([TextFragment "No Document."],2,vst)
	buildLink info
		# location = case info.dataLocation of
			LocalLocation taskId	= taskId
			SharedLocation sid _	= "shared_" +++ sid
		= "/document/download/link/" +++ location +++ "/" +++ toString info.DocumentInfo.index
	
	isValid :: Document DataPath ErrorMask Bool Bool -> Bool
	isValid doc cp em optional valid
	| optional || not (isEmptyDoc doc)	= valid
	| getErrorCount cp em > 0			= False
	| otherwise							= False

//Hidden type
gVisualize{|Hidden|} fx old new vst=:{VSt | currentPath}
	= ([],0,{VSt | vst & currentPath = stepDataPath currentPath})

gVisualize{|HtmlDisplay|} fx old new vst=:{VSt | origVizType, vizType, currentPath, renderAsStatic}
	= case origVizType of
		VHtmlDisplay
			# (def,rh,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
			= (def,rh,{VSt | vst & currentPath = stepDataPath currentPath})
		_
			# (def,rh,vst) = fx oldV newV {VSt | vst &  renderAsStatic = True, currentPath = shiftDataPath currentPath}
			= (def,rh,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
where 
	oldV = case old of (VValue (HtmlDisplay ov) om) = (VValue ov om); _ = VBlank
	newV = case new of (VValue (HtmlDisplay nv) nm) = (VValue nv nm); _ = VBlank

gVisualize{|Editable|} fx old new vst=:{VSt | vizType, currentPath, renderAsStatic}
	# (def,rh,vst) = fx oldV newV {VSt | vst & renderAsStatic = False, currentPath = shiftDataPath currentPath}
	= (def,rh,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
where 
	oldV = case old of (VValue (Editable ov) om) = (VValue ov om); _ = VBlank
	newV = case new of (VValue (Editable nv) nm) = (VValue nv nm); _ = VBlank

gVisualize{|VisualizationHint|} fx old new vst=:{VSt | idPrefix, vizType, origVizType, currentPath, renderAsStatic}
	= case origVizType of
		VHtmlDisplay
			= case old of
				(VValue (VHHidden _) _) 
					= ([],0,{VSt | vst & currentPath = stepDataPath currentPath})
				_
					# (viz,rh,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, vizType = VHtmlDisplay}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, vizType = vizType})
		VEditorUpdate
			= case (old,new) of		
				//_, hidden -> replace with hidden
				(_,(VValue (VHHidden _) _))
					# path = shiftDataPath currentPath
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s shiftPath, id = dp2id idPrefix shiftPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,0,{VSt | vst & currentPath = stepDataPath currentPath})
				//hidden, html -> replace with static
				((VValue (VHHidden _) _),(VValue (VHHtmlDisplay _) _))
					# (viz,rh,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//hidden, edit = replace with editable
				((VValue (VHHidden _) _),(VValue (VHEditable _) _))
					# (viz,rh,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//html, edit -> replace
				((VValue (VHHtmlDisplay _) _),(VValue (VHEditable _) _))
					# (viz,rh,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//edit, html -> replace
				((VValue (VHEditable _) _),(VValue (VHHtmlDisplay _) _))
					# (viz,rh,vst) = fx newV newV {vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//_ -> update	
				_	
					# (upd,rh,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
					= (upd,rh,{VSt | vst & currentPath = stepDataPath currentPath})	
		_
			= case old of
				(VValue (VHHidden _) _)
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s shiftPath, id = dp2id idPrefix shiftPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,0,{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue (VHHtmlDisplay _) _)
					# (viz,rh,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, renderAsStatic = True}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				(VValue (VHEditable _) _)
					# (viz,rh,vst) = fx oldV newV {vst & currentPath = shiftDataPath currentPath, renderAsStatic = False}
					= (viz,rh,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})			
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
	| getErrorCount dp em > 0	= False //If there is an error, the form is invalid. Regardless any optional fields.
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

consSelector :: GenericConsDescriptor String DataPath (VisualizationValue a) (Maybe String) Bool Bool -> [Visualization]
consSelector d idPrefix dp value label useLabels renderAsStatic
	# masked = case value of (VValue _ dm) = isMasked dp dm; _ = False
	//No choice needed with only one constructor
	| d.gcd_type_def.gtd_num_conses == 1 
		= []
	//Use radiogroup to choose a constructor
	//| d.gcd_type_def.gtd_num_conses <= MAX_CONS_RADIO 
	//	# items	= [TUIRadio {TUIRadio|name = name, id = (dp2id idPrefix rdp)+++"-radio", value = c.gcd_name, boxLabel = Just c.gcd_name, checked = (masked && c.gcd_index == index), fieldLabel = Nothing, hideLabel = True} 
	//			   \\ c <- d.gcd_type_def.gtd_conses & rdp <- radioDps (shiftDataPath dp)]
	//	= [TUIFragment (TUIRadioGroup {TUIRadioGroup|name = name, id = id, items = items, fieldLabel = label, columns = 4, hideLabel = not useLabels})]
	//Use combobox to choose a constructor
	| otherwise
		= [TUIFragment (TUIComboBox {TUIComboBox|name = name, id = id, value = (if masked d.gcd_name ""), fieldLabel = label, hideLabel = not useLabels, store = store, triggerAction = "all", editable = False})]
where
	store		= [("","Select...") : [(c.gcd_name,c.gcd_name) \\ c <- d.gcd_type_def.gtd_conses]]
	name		= dp2s dp
	id			= dp2id idPrefix dp
	index		= d.gcd_index
	radioDps dp	= [dp:radioDps (stepDataPath dp)]

determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = ([TUIUpdate (TUIRemove (fromJust (getId consid))) \\ consid <- (coerceToTUIDefs editor) | isJust (getId consid)])
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = reverse [TUIUpdate (TUIAdd consid def) \\ def <- coerceToTUIDefs editor]

determineChildAdditions :: String [Visualization] -> [Visualization]
determineChildAdditions consid editor = [TUIUpdate (TUIAddTo consid def) \\ def <- coerceToTUIDefs editor]

getHintUpdate :: TUIId DataPath HintMask -> Visualization
getHintUpdate id cp hm = TUIUpdate (TUISetHint id (getHintMessage cp hm))

getErrorUpdate :: TUIId DataPath ErrorMask -> Visualization
getErrorUpdate id cp em = TUIUpdate (TUISetError id (getErrorMessage cp em))

derive gPrint LabelOrNumber

determineIndexOfLabels :: !String !*VSt -> *VSt
determineIndexOfLabels label vst=:{VSt | errorMask,hintMask,currentPath}
	# curPath 	= [Label label:[Unlabeled i \\ i <- tl (getDP currentPath)]]
	# pos		= hd (getDP currentPath)
	# hntMask	= [(translateLDP curPath ldp pos, msg) \\ (ldp,msg) <- hintMask]
	# errMask	= [(translateLDP curPath ldp pos, msg) \\ (ldp,msg) <- errorMask]	
	= {VSt | vst & hintMask = hntMask, errorMask = errMask}
where 
	getDP (DataPath dp _ _) = dp

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
getId (TUIUsernameControl d)	= Just d.TUIBasicControl.id
getId (TUIPasswordControl d)	= Just d.TUIBasicControl.id
getId (TUIDocumentControl d)	= Just d.TUIDocumentControl.id
getId (TUIConstructorControl d)	= Just d.TUIConstructorControl.id
getId (TUITupleContainer d)		= Just d.TUITupleContainer.id
getId (TUIRecordContainer d)	= Just d.TUIRecordContainer.id

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
getId (TUIList d)				= Just d.TUIList.id
getId (TUIListItem d)			= Just d.TUIListItem.id
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
