implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum
import GenUpdate, GenEq
import Void, Either
import Text, Html, JSON, TUIDefinition

derive gEq Document

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| origVizType = VTextDisplay, vizType = VTextDisplay, idPrefix = "", currentPath = shiftDataPath initialDataPath, label = Nothing, 
		useLabels = False, selectedConsIndex = -1, optional = False, valid = True, renderAsStatic = False, updateMask = Untouched False [], verifyMask = VMValid Nothing Nothing [], updates = []}

import StdDebug

//Wrapper functions
visualizeAsEditor :: String (Maybe SubEditorIndex) UpdateMask VerifyMask a -> ([TUIDef],Bool) | gVisualize{|*|} a
visualizeAsEditor name mbSubIdx umask vmask x
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType  = VEditorDefinition, idPrefix = name, updateMask = umask, verifyMask = vmask}
	# vst = case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (defs,vst=:{VSt | valid}) = gVisualize{|*|} val val vst
	//= trace_n("==UpdateMask==\n"+++toString (toJSON umask) +++ "\n==VerifyMask==\n" +++ toString (toJSON vmask)+++"\n") (coerceToTUIDefs defs, valid)
	= (coerceToTUIDefs defs, valid)	
where
	val = VValue x
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))
where
	val = VValue x

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))
where
	val = VValue x

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst (gVisualize{|*|} val val {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
where
	val = VValue x
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} val val {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))
where
	val = VValue x

determineEditorUpdates	:: String (Maybe SubEditorIndex) [DataPath] UpdateMask VerifyMask a a -> ([TUIUpdate],Bool)	| gVisualize{|*|} a
determineEditorUpdates name mbSubIdx updatedPaths umask vmask old new
	# vst 	= {mkVSt & vizType = VEditorUpdate, idPrefix = name, updateMask = umask, verifyMask = vmask, updates = updatedPaths}
	# vst 	= case mbSubIdx of
		Nothing		= vst
		Just idx	= {VSt| vst & currentPath = dataPathSetSubEditorIdx vst.VSt.currentPath idx}
	# (updates,vst=:{VSt | valid}) = (gVisualize{|*|} (VValue old) (VValue new) vst)
	//= trace_n("==Update Mask==\n"+++toString (toJSON umask) +++ "\n==VerifyMask==\n" +++ toString (toJSON vmask)+++"\n") (coerceToTUIUpdates updates, valid)
	= (coerceToTUIUpdates updates, valid)

//Bimap for visualization values
derive bimap VisualizationValue

//Generic visualizer
generic gVisualize a :: (VisualizationValue a) (VisualizationValue a) *VSt -> ([Visualization], *VSt)

gVisualize{|UNIT|} _ _ vst
	= ([],vst)

gVisualize{|PAIR|} fx fy old new vst
	= case (old,new) of
		(VValue (PAIR ox oy), VValue (PAIR nx ny))
			# (vizx, vst) = fx (VValue ox) (VValue nx) vst
			# (vizy, vst) = fy (VValue oy) (VValue ny) vst
			= (vizx ++ vizy, vst)
		_
			# (vizx, vst) = fx VBlank VBlank vst
			# (vizy, vst) = fy VBlank VBlank vst
			= (vizx ++ vizy, vst)

gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,currentPath,valid,verifyMask,updateMask}
	= case (old,new) of
		//Same structure:
		(VValue (LEFT ox), VValue (LEFT nx))
			# oval = VValue ox
			# nval = VValue nx
			= case vizType of
				VEditorUpdate
					| isDirtyUM updateMask
						# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
						# (new,vst)			= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})			
					| otherwise
						# (upd,vst) = fx oval nval {vst & vizType = VEditorUpdate}
						= (upd,vst)
				_
					= fx oval nval vst
		(VValue (RIGHT oy), VValue (RIGHT ny))
			# oval = VValue oy
			# nval = VValue ny
			= case vizType of
				VEditorUpdate
					| isDirtyUM updateMask
						# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
						# (old,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
						# (new,vst)			= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
						= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
					| otherwise
						# (upd,vst) = fy oval nval {vst & vizType = VEditorUpdate}
						= (upd,vst)
				_
					= fy oval nval vst
		//Different structure:
		(VValue (LEFT ox), VValue (RIGHT ny))
			# oval = VValue ox
			# nval = VValue ny
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst)			= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid} //the mask is completely wrong, but that does not matter as we only need the id's of components for removal
					# (new,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy), VValue (LEFT nx))
			# oval = VValue oy
			# nval = VValue nx
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst)			= fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value
		
		//No value any more
		(VValue (LEFT ox), VBlank)
			# oval = VValue ox
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VValue (RIGHT oy), VBlank)
			# oval = VValue oy
			# nval = VBlank
			= case vizType of
				VEditorUpdate
					# (old,vst) = fy oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) = fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new, {vst & vizType = VEditorUpdate})
				_
					= fy oval oval vst //Default case: ignore the new value					
		
		//New value
		(VBlank, VValue (LEFT nx))
			# oval = VBlank
			# nval = VValue nx
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fx nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fx nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		(VBlank, VValue (RIGHT ny))
			# oval = VBlank
			# nval = VValue ny
			= case vizType of
				VEditorUpdate
					# (consSelUpd,vst)	= fy nval nval {vst & vizType = VConsSelectorUpdate}
					# (old,vst) 		= fx oval oval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid}
					# (new,vst) 		= fy nval nval {vst & vizType = VEditorDefinition, currentPath = currentPath, valid = valid, verifyMask = verifyMask, updateMask = updateMask}
					= (determineRemovals old ++ determineChildAdditions pathid new ++ consSelUpd, {vst & vizType = VEditorUpdate})
				_
					= fx oval oval vst //Default case: ignore the new value
		//Default case
		_
			= fx VBlank VBlank vst		
where
	//pathid					= dp2id idPrefix (dataPathSetConsFlag currentPath)
	pathid					= dp2id idPrefix currentPath

gVisualize{|CONS of d|} fx old new vst=:{vizType,idPrefix,currentPath,label,useLabels,optional,valid,verifyMask,updateMask}
	= case vizType of
		VEditorDefinition
			# (ox,nx) = case (old,new) of (VValue (CONS ox),VValue (CONS nx))	= (VValue ox, VValue nx); _ = (VBlank,VBlank)
			//records
			| not (isEmpty d.gcd_fields)
				# (valid,err,hnt) = case updateMask of
							(Untouched _ _) = case verifyMask of
								(VMInvalid IsBlankError _ _) = (False, "", "")
								(VMInvalid (ErrorMessage s) _ _) = (False, s, "")
								(VMValid mbHnt _ _) = (valid, "", mbHintToString mbHnt)
								(VMUntouched mbHnt _ _) = (False, "", mbHintToString mbHnt)
							_				= case verifyMask of
								(VMInvalid err _ _) = (False, toString err, "")
								(VMValid mbHnt _ _) = (valid, "", mbHintToString mbHnt)
								(VMUntouched mbHnt _ _) = (False, "", mbHintToString mbHnt)
				= case ox of 
					VBlank 
						= ([TUIFragment (TUIRecordContainer  {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = [], optional = optional, hasValue = False, errorMsg = err, hintMsg = hnt})]
						  , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
					_
						# (viz,vst) = fx ox nx {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False, valid = valid}
						= ([TUIFragment (TUIRecordContainer {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs", name = dp2s currentPath, title = label, items = coerceToTUIDefs viz, optional = optional, hasValue = True, errorMsg = err, hintMsg = hnt})]
						 , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional})
			//ADT's
			| otherwise
				# (viz,vst) = fx ox nx {VSt | vst & currentPath = shiftDataPath currentPath}
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional, selectedConsIndex= d.gcd_index})
		//Structure update
		VEditorUpdate
			// records
			| not (isEmpty d.gcd_fields)
				# (valid,msg) = verifyElementUpd valid id updateMask verifyMask
				= case (old,new) of 
					(VValue (CONS ox), VBlank)
						// remove components
						# (viz,vst) = fx (VValue ox) (VValue ox) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False, valid = valid}
						= (msg ++ determineRemovals viz
						  , {VSt | vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels})
					(VBlank, VValue (CONS nx))
						// add components
						# (viz,vst) = fx (VValue nx) (VValue nx) {vst & vizType = VEditorDefinition, label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False, valid = valid}
						= (msg ++ (determineChildAdditions ((dp2id idPrefix currentPath) +++ "-fs") viz)
						  , {VSt | vst & currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels});				
					(VValue (CONS ox), VValue (CONS nx)) 
						# (vizBody,vst) = fx (VValue ox) (VValue nx) {vst & label = Nothing, currentPath = shiftDataPath currentPath, useLabels = True, optional = False, valid = valid} 
						= (msg ++ vizBody
						  , {VSt|vst & vizType = vizType, currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels})
					_
						= ([],{VSt | vst & currentPath = stepDataPath currentPath})
			//ADT's
			| otherwise
				# (viz,vst) = fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath}
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath, optional = optional})
		//Cons selector	update	
		VConsSelectorUpdate = (consSelectorUpdate new updateMask, vst)
		//Html display vizualization
		VHtmlDisplay
			= case (old,new) of
				(VValue (CONS ox), VValue (CONS nx))
					# (vizBody,vst) = fx (VValue ox) (VValue nx) {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath}
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
	oldV 	= case old of (VValue (CONS ox)) = VValue ox; _ = VBlank
	newV 	= case new of (VValue (CONS nx)) = VValue nx; _ = VBlank  
	
	fsid = (dp2id idPrefix currentPath)+++"-fs"
	cId = (dp2id idPrefix currentPath)+++"c"
	id = (dp2id idPrefix currentPath)
	
	//Do not show constructors that start with an underscore (_Tuple2,_Cons etc.)
	vizCons = if (d.gcd_name.[0] == '_') [] [TextFragment d.gcd_name]

	//Only show a body when you have a value and it is masked
	//showBody dp VBlank			= False
	//showBody dp (VValue _ dm)	= isMasked dp dm
	
	consSelectorUpdate VBlank um = []
	consSelectorUpdate (VValue _) um
		= case um of
			(Touched _ _)
				| (isEmpty  d.gcd_fields && d.gcd_type_def.gtd_num_conses > 1)
					= [TUIUpdate (TUISetValue cId d.gcd_name)]	
				| otherwise
					= []
			_ = []

gVisualize{|OBJECT of d|} fx old new vst=:{vizType,idPrefix,label,currentPath,selectedConsIndex = oldSelectedConsIndex,useLabels,valid,optional,renderAsStatic,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	//ADT's with multiple constructors
	| d.gtd_num_conses > 1
		= case vizType of 
			VEditorDefinition
				# (valid,err,hnt) = verifyElementStr valid cmu cmv
				# (items,vst=:{selectedConsIndex}) 	= fx oldV newV {vst & useLabels = False, optional = False, valid = valid, updateMask = cmu, verifyMask = cmv}
				# consValues = [gdc.gcd_name \\ gdc <- d.gtd_conses]
				= ([TUIFragment (TUIConstructorControl {TUIConstructorControl
														| id = id
														, name = dp2s currentPath
														, fieldLabel = label
														, consSelIdx = case cmu of (Touched _ _) = selectedConsIndex; _ = -1
														, consValues = consValues
														, items = case cmu of (Touched _ _) = (coerceToTUIDefs items); _ = []
														, staticDisplay = renderAsStatic
														, errorMsg = err
														, hintMsg = hnt
														})]
				  ,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional, updateMask = um, verifyMask = vm})
			VEditorUpdate
				# (valid,msg) = verifyElementUpd valid id cmu cmv
				= case cmu of
					(Touched _ _)
						# (upd,vst=:{valid}) = fx oldV newV {vst & useLabels = False, optional = False, updateMask = cmu, verifyMask = cmv, valid = valid}
						= (msg ++ upd
							,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional, updateMask = um, verifyMask = vm})			
					_
						# (rem,vst=:{valid}) = fx oldV oldV {vst & vizType = VEditorDefinition, currentPath = currentPath, updateMask = cmu, verifyMask = cmv, valid = valid}
						= (msg ++ [TUIUpdate (TUISetValue cId ""):determineRemovals rem]
							,{vst & vizType = vizType, currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, valid = valid, updateMask = um, verifyMask = vm})						
			_
				# (viz,vst) = fx oldV newV vst
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath})
	//Everything else
	| otherwise
		= case (old,new) of
			(VValue (OBJECT ox), VValue (OBJECT nx))
				#(v,vst) = fx (VValue ox) (VValue nx) {VSt | vst & verifyMask = cmv, updateMask = cmu}
				= (v,{VSt | vst & verifyMask = vm, updateMask = um})
			(VValue (OBJECT ox), VBlank)
				#(v,vst) = fx(VValue ox) VBlank {VSt | vst & verifyMask = cmv, updateMask = cmu}
				= (v,{VSt | vst & verifyMask = vm, updateMask = um})
			(VBlank, VValue (OBJECT nx))
				#(v,vst) = fx VBlank (VValue nx) {VSt | vst & verifyMask = cmv, updateMask = cmu}
				= (v,{VSt | vst & verifyMask = vm, updateMask = um})
			_
				#(v,vst) = fx VBlank VBlank {VSt | vst & verifyMask = cmv, updateMask = cmu}
				= (v,{VSt | vst & verifyMask = vm, updateMask = um})
where
	id		= dp2id idPrefix currentPath
	cId 	= (dp2id idPrefix currentPath)+++"c"
	oldV 	= case old of (VValue (OBJECT ox)) = (VValue ox); _ = VBlank
	newV 	= case new of (VValue (OBJECT nx)) = (VValue nx); _ = VBlank
	
gVisualize{|FIELD of d|} fx old new vst=:{vizType,currentPath}
	//# vst = determineIndexOfLabels d.gfd_name vst	
	= case (old,new) of
		(VValue (FIELD ox), VValue (FIELD nx))
			= case vizType of
				VHtmlDisplay
					# (vizBody,vst) 	= fx (VValue ox) (VValue nx) {VSt |vst & label = Nothing}
					= ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],{VSt | vst & label = Nothing})
				VTextDisplay
					# (vizBody,vst) 	= fx (VValue ox) (VValue nx) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody]++[TextFragment " "], {VSt | vst & label = Nothing})
				_
					# (vizBody,vst)	= fx (VValue ox) (VValue nx) {VSt |vst & label = Just (formatLabel d.gfd_name)}
					= (vizBody, {VSt | vst & label = Nothing})
		_
			= fx VBlank VBlank {VSt |vst & label = Just (formatLabel d.gfd_name)}

//********************************************************************************************************************************************************
gVisualize{|Int|} old new vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIIntControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_	
			= ([TextFragment (toString old)], {VSt|vst & currentPath = stepDataPath currentPath})

		
gVisualize{|Real|} old new vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIRealControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_					
			= ([TextFragment (toString old)], {VSt|vst & currentPath = stepDataPath currentPath})
		
gVisualize{|Char|} old new vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUICharControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_		
			= ([TextFragment (toString old)], {VSt|vst & currentPath = stepDataPath currentPath})


gVisualize{|String|} old new vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl old vst
			= ([TUIFragment (TUIStringControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst
		_			
			=	([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Bool|} old new vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition
			# (ctl,vst) = visualizeBasicControl old vst	
			= ([TUIFragment (TUIBoolControl ctl)],vst)
		VEditorUpdate
			= updateBasicControl old new vst //!!: Could be wrong..
		_	
			= ([TextFragment (toString old)]
				, {VSt|vst & currentPath = stepDataPath currentPath})		
	
gVisualize{|Maybe|} fx old new vst=:{vizType,idPrefix,currentPath,optional,valid}
	= case vizType of
		VEditorDefinition
			= case (old,new) of
				(VValue (Just ox), _)
					# oval = VValue ox
					# (viz, vst) = fx oval oval {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		VEditorUpdate
			= case (old,new) of
				(VValue (Just ox), VValue (Just nx))
					# (viz, vst) = fx (VValue ox) (VValue nx) {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue (Just ox), VValue Nothing)
					# (viz, vst) = fx (VValue ox) VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				(VValue Nothing, VValue (Just nx))
					# (viz, vst) = fx VBlank (VValue nx) {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx VBlank VBlank {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		_			
			= case old of
				(VValue Nothing)	= ([TextFragment "-"],vst)
				(VValue (Just x))	= fx (VValue x) (VValue x) vst
				VBlank				= ([],vst)
where
	pathid = dp2id idPrefix currentPath		

gVisualize{|Dynamic|} old new vst
	= ([],vst)

gVisualize{|(,)|} f1 f2 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# (v1,v2) = case old of (VValue (o1,o2)) = (VValue o1, VValue o2) ; _ = (VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
			# (viz2,vst) = f2 v2 v2 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2]})]		 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = useLabels, updateMask = um, verifyMask = vm})		
		_
			= case (old,new) of
				(VValue (o1,o2), VValue(n1,n2))
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1) (VValue n1) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
					# (viz2,vst) = f2 (VValue o2) (VValue n2) vst
					= (viz1 ++ separator viz2 ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath}
					# (viz2,vst) = f2 VBlank VBlank vst
					= (viz1 ++ separator viz2 ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize{|(,,)|} f1 f2 f3 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3) = case old of (VValue (o1,o2,o3)) = (VValue o1, VValue o2, VValue o3) ; _ = (VBlank,VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
			# (viz2,vst) = f2 v2 v2 vst
			# (viz3,vst) = f3 v3 v3 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2,viz3]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels=oldLabels, updateMask = um, verifyMask = vm})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3), VValue(n1,n2,n3))
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1) (VValue n1) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
					# (viz2,vst) = f2 (VValue o2) (VValue n2) vst
					# (viz3,vst) = f3 (VValue o3) (VValue n3) vst
					= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath, updateMask = cmu, verifyMask = cmv}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize{|(,,,)|} f1 f2 f3 f4 old new vst=:{vizType,idPrefix,currentPath,useLabels, label,optional,updateMask,verifyMask}
	# oldLabel = useLabels
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (v1,v2,v3,v4) = case old of (VValue (o1,o2,o3,o4)) = (VValue o1, VValue o2, VValue o3,VValue o4) ; _ = (VBlank,VBlank,VBlank,VBlank)
			# (viz1,vst) = f1 v1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
			# (viz2,vst) = f2 v2 v2 vst
			# (viz3,vst) = f3 v3 v3 vst
			# (viz4,vst) = f4 v4 v4 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, items = map coerceToTUIDefs [viz1,viz2,viz3,viz4]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})		
		_
			= case (old,new) of
				(VValue (o1,o2,o3,o4), VValue(n1,n2,n3,n4))
					# oldLabels = useLabels
					# (viz1,vst) = f1 (VValue o1) (VValue n1) {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = cmu, verifyMask = cmv}
					# (viz2,vst) = f2 (VValue o2) (VValue n2) vst
					# (viz3,vst) = f3 (VValue o3) (VValue n3) vst
					# (viz4,vst) = f4 (VValue o4) (VValue n4) vst
					= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3 ++ separator viz4 ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})
				_
					# oldLabels = useLabels
					# (viz1,vst) = f1 VBlank VBlank {VSt| vst & currentPath = shiftDataPath currentPath, updateMask = cmu, verifyMask = cmv}
					# (viz2,vst) = f2 VBlank VBlank vst
					# (viz3,vst) = f3 VBlank VBlank vst
					# (viz4,vst) = f4 VBlank VBlank vst
					= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3 ++ separator viz4 ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize {|[]|} fx old new vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,valid,renderAsStatic, updateMask,verifyMask}
	# (cmu, um) = popMask updateMask
	# (cmv, vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# (valid,err,hnt) = verifyElementStr valid cmu cmv 
			# (items,vst) = TUIDef fx oldV 0 {VSt | vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, valid = valid, updateMask = cmu, verifyMask = cmv}
			= ([TUIFragment (TUIListContainer {TUIListContainer | items = items, optional = optional, name = name, id = id, fieldLabel = label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})],
			  {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm, useLabels = useLabels})	
		VEditorUpdate
			# (valid,msg) 					= verifyElementUpd valid id cmu cmv		
			# (upd,vst=:{valid=finalValid}) = TUIUpd fx oldV newV {VSt | vst & currentPath = shiftDataPath currentPath, updateMask = cmu, verifyMask = cmv, valid = valid}
			# (newDefs,vst)					= TUIDef fx newV 0 {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, valid = valid, updateMask = cmu, verifyMask = cmv}
			# (oldDefs,vst)					= TUIDef fx oldV 0 {VSt | vst & vizType = VEditorDefinition, currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, valid = valid, updateMask = cmu, verifyMask = cmv}		
			# (addrem)						= determineAddRem oldDefs newDefs 0
			= case cmu of
				(UMList [i:is] c)
					# (replacements)		= determineReplacements newDefs [i:is]	
					= (addrem++replacements++upd++msg,
			 		  {VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate, label = label, useLabels = useLabels, valid=finalValid, optional = optional, updateMask = um, verifyMask = vm})
				_ 
					= (addrem++upd++msg,
			 		  {VSt | vst & currentPath = stepDataPath currentPath, vizType=VEditorUpdate, label = label, useLabels = useLabels, valid=finalValid, optional = optional, updateMask = um, verifyMask = vm}) 
		VHtmlDisplay
			= case oldV of
				[] 
					= ([HtmlFragment [UlTag [] [LiTag [ClassAttr "list-item-light"] [(Text "Empty list")]]]],{VSt | vst & currentPath = stepDataPath currentPath})
				_		
					# (items,vst) = staticDef fx oldV {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment [UlTag [] [(LiTag [ClassAttr (itemCls i)] (flatten (coerceToHtml x))) \\ x <- items & i <- [0..]]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay
			= case oldV of
				[]
					= ([TextFragment "[]"],{VSt | vst & currentPath = stepDataPath currentPath})
				_
					# (items,vst) = staticDef fx oldV {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([TextFragment ("["+++join ", " (flatten  [(coerceToStrings x) \\ x <-items])+++"]")],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel
			= case oldV of
				[] = ([HtmlFragment [(Text "Empty list")]],{VSt | vst & currentPath = stepDataPath currentPath})
				_ 
					# (items,vst) = staticDef fx oldV {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment (htmlLabel items)],{VSt | vst & currentPath = stepDataPath currentPath})
		
		_
			= ([],
			  {VSt | vst & currentPath = stepDataPath currentPath})
		
where
	oldV = case old of (VValue ol) = ol; _ = []
	newV = case new of (VValue nl) = nl; _ = []

	id 			= dp2id idPrefix currentPath
	name 		= dp2s currentPath
	itemId idx 	= id+++"#"+++toString idx

	itemCls i
		| isEven i  = "list-item-light"
		| otherwise = "list-item-dark"

	TUIDef fx [] idx vst=:{VSt | optional}
		| renderAsStatic = ([],vst)
		| otherwise
			# (dx,vst)  = fx VBlank VBlank {VSt | vst & optional = True}
			= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}],{VSt | vst & optional = optional})
	TUIDef fx [x:xs] idx vst
		# (dx, vst) = fx (VValue x) (VValue x) {VSt | vst & optional = False}
		# (dxs,vst) = TUIDef fx xs (inc idx) {VSt | vst & optional = optional}
		= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}:dxs],vst)

	TUIUpd fx [o:os] [n:ns] vst
		# (u,  vst) 			= fx (VValue o) (VValue n) vst
		# (us, vst) 			= TUIUpd fx os ns vst
		= (u++us,vst)
	TUIUpd _  _      _      vst = ([],vst)
	
	staticDef fx []     vst = ([],vst)
	staticDef fx [o:os] vst
		# (hx, vst) = fx (VValue o) (VValue o) vst
		# (hxs,vst) = staticDef fx os vst
		= ([hx:hxs],vst);

	determineAddRem []     []     idx = []
	determineAddRem [o:os] []     idx = [TUIUpdate (TUIRemove (itemId idx)):determineAddRem os [] (idx+1)]
	determineAddRem []     [n:ns] idx = [if(idx > 0) (TUIUpdate (TUIAdd (itemId (idx-1)) n)) (TUIUpdate (TUIAddTo id n)):determineAddRem [] ns (idx+1)]
	determineAddRem [o:os] [n:ns] idx = determineAddRem os ns (idx+1)

	determineReplacements defs idx	 = [TUIUpdate (TUIReplace (itemId i) (defs!!i)) \\ i <-idx]
	
	htmlLabel [i] = (flatten (coerceToHtml i))
	htmlLabel [i:is] = (flatten (coerceToHtml i)) ++ [(Text ", ")] ++ htmlLabel is
	
//Document Type
gVisualize {|Document|} old new vst=:{vizType, label, idPrefix, currentPath, valid, optional, useLabels,renderAsStatic, updates, updateMask, verifyMask}
= case vizType of
	VEditorDefinition
		# (cmu,um) = popMask updateMask
		# (cmv,vm) = popMask verifyMask
		# (valid,err,hnt) = verifyElementStr valid cmu cmv
		= ([TUIFragment (TUIDocumentControl {TUIDocumentControl |id = id, name = dp2s currentPath, document = oval, fieldLabel = label, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})],
		  {VSt | vst & currentPath = stepDataPath currentPath, valid = valid, updateMask = um, verifyMask = vm})
	VEditorUpdate
		# (valid,msg,vst) = getMessageUpdates vst
		| oval =!= nval //use 'dirty field'
			= ([TUIUpdate (TUISetValue id (toString (toJSON nval))):msg],
			  {VSt | vst & currentPath = stepDataPath currentPath, valid = valid})
		| otherwise 
			= ((restoreField currentPath updates id (toString (toJSON oval)))++msg,
			  {VSt | vst & currentPath = stepDataPath currentPath})
	VHtmlDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue document)
				| document.Document.size == 0
					= noDocument vst
				| otherwise
					# downLink = ATag [HrefAttr (buildLink document)
									  ,TargetAttr "_blank",IdAttr id
									  ,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
					# prevLink = ATag [HrefAttr "#", IdAttr id
									  ,NameAttr "x-form-document-preview-link" ] [ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
					= ([HtmlFragment [(Text ( document.Document.name +++" ("+++printByteSize document.Document.size+++") ")),RawText "&nbsp;",downLink,prevLink]],
				  		{VSt | vst & currentPath = stepDataPath currentPath})
	VTextDisplay
		= case old of
			(VBlank) = noDocument vst
			(VValue document)
				| document.Document.size == 0 = noDocument vst
				| otherwise	= ([TextFragment document.Document.name],{VSt | vst & currentPath = stepDataPath currentPath})
where
	id = dp2id idPrefix currentPath
	
	oval = case old of (VValue o) = o; _ = {Document|documentId = "",name = "", mime = "", size = 0}
	nval = case new of (VValue n) = n; _ = {Document|documentId = "",name = "", mime = "", size = 0}
	
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
	printByteSize size
	| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
	| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
	| otherwise 	  = toString size +++ " byte"
	
	noDocument vst = ([TextFragment "No Document."],vst)
	buildLink document = "/services/json/documents/" +++ document.Document.documentId +++ "/download"

//Hidden type
gVisualize{|Hidden|} fx old new vst=:{VSt | currentPath}
	= ([],{VSt | vst & currentPath = stepDataPath currentPath})

gVisualize{|Display|} fx old new vst=:{VSt | origVizType, vizType, currentPath, renderAsStatic,valid}
	= case origVizType of
		VHtmlDisplay
			# (def,vst) = fx oldV newV vst
			= (def,{VSt | vst & currentPath = stepDataPath currentPath, valid = valid})
		_
			# (def,vst) = fx oldV newV {VSt | vst &  renderAsStatic = True}
			= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic, valid = valid})
where 
	oldV = case old of (VValue (Display ov)) = (VValue ov); _ = VBlank
	newV = case new of (VValue (Display nv)) = (VValue nv); _ = VBlank

gVisualize{|Editable|} fx old new vst=:{VSt | vizType, currentPath, renderAsStatic}
	# (def,vst) = fx oldV newV {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
where 
	oldV = case old of (VValue (Editable ov)) = (VValue ov); _ = VBlank
	newV = case new of (VValue (Editable nv)) = (VValue nv); _ = VBlank

gVisualize{|VisualizationHint|} fx old new vst=:{VSt | idPrefix, vizType, origVizType, currentPath, renderAsStatic,valid}
	= case origVizType of
		VHtmlDisplay
			= case old of
				(VValue (VHHidden _)) 
					= ([],{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue (VHDisplay _))
					# (viz,vst) = fx oldV newV {vst & vizType = VHtmlDisplay}
					= (viz,{vst & currentPath = stepDataPath currentPath, vizType = vizType, valid = valid})
				_
					# (viz,vst) = fx oldV newV {vst & vizType = VHtmlDisplay}
					= (viz,{vst & currentPath = stepDataPath currentPath, vizType = vizType})
		VEditorUpdate
			= case (old,new) of		
				//_, hidden -> replace with hidden
				(_,(VValue (VHHidden _)))
					# path = shiftDataPath currentPath
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s currentPath, id = dp2id idPrefix currentPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,{VSt | vst & currentPath = stepDataPath currentPath})
				//hidden, html -> replace with static
				((VValue (VHHidden _)),(VValue (VHDisplay _)))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic,valid=valid})
				//hidden, edit = replace with editable
				((VValue (VHHidden _)),(VValue (VHEditable _)))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//html, edit -> replace
				((VValue (VHDisplay _)),(VValue (VHEditable _)))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				//edit, html -> replace
				((VValue (VHEditable _)),(VValue (VHDisplay _)))
					# (viz,vst) = fx newV newV {vst & vizType = VEditorDefinition, renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic,valid=valid})
				//update VHDisplay, ignore validation
				((VValue (VHDisplay _)),(VValue (VHDisplay _)))
					# (upd,vst) = fx oldV newV vst
					= (upd,{VSt | vst & currentPath = stepDataPath currentPath, valid = valid})
				//_ -> update	
				_	
					# (upd,vst) = fx oldV newV vst
					= (upd,{VSt | vst & currentPath = stepDataPath currentPath})	
		_
			= case old of
				(VValue (VHHidden _))
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s currentPath, id = dp2id idPrefix currentPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,{VSt | vst & currentPath = stepDataPath currentPath})
				(VValue (VHDisplay _))
					# (viz,vst) = fx oldV newV {vst & renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				(VValue (VHEditable _))
					# (viz,vst) = fx oldV newV {vst & renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})			
where
	oldV = case old of (VValue (VHEditable ox)) = (VValue ox); (VValue (VHDisplay ox)) = (VValue ox); _ = VBlank
	newV = case new of (VValue (VHEditable nx)) = (VValue nx); (VValue (VHDisplay nx)) = (VValue nx); _ = VBlank
	
	id = dp2id idPrefix currentPath	
	
derive gVisualize Either, Void

instance toString (VisualizationValue a) | toString a
where
	toString VBlank		= ""
	toString (VValue x)	= toString x
	
value2s :: !UpdateMask !(VisualizationValue a) -> String | toString a
value2s (Touched _ _) (VValue a) = toString a
value2s _ _ = ""

labelAttr :: !Bool !(Maybe String) -> Maybe String
labelAttr False	_		= Nothing
labelAttr True	Nothing	= Just ""
labelAttr True	l		= l 

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

//***** NEW STUFF *************************************************************************************************
visualizeBasicControl :: !(VisualizationValue a) !*VSt -> (!TUIBasicControl, !*VSt) | toString a
visualizeBasicControl old vst=:{vizType,idPrefix,label,currentPath,updates,useLabels,optional,valid,renderAsStatic,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	# val = value2s cmu old
	# (valid,err,hnt) = verifyElementStr valid cmu cmv
	= ({TUIBasicControl | name = dp2s currentPath, id = dp2id idPrefix currentPath, value = val, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt}
	  ,{VSt | vst & verifyMask = vm, updateMask = um, currentPath = stepDataPath currentPath, valid = valid})

updateBasicControl :: !(VisualizationValue a) !(VisualizationValue a) !*VSt -> (![Visualization],!*VSt) | toString a
updateBasicControl old new vst=:{vizType,idPrefix,label,currentPath,updates,useLabels,optional,valid,renderAsStatic,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	# id = dp2id idPrefix currentPath
	# oldV = value2s cmu old
	# newV = value2s cmu new
	# (upd,vst) = updateVizValue oldV newV vst
	# (valid,msg) = verifyElementUpd valid id cmu cmv
	= (upd++msg, {VSt|vst & currentPath = stepDataPath currentPath, valid= valid, verifyMask = vm, updateMask = um})

mbHintToString :: (Maybe HintMessage) -> String
mbHintToString Nothing = ""
mbHintToString (Just h) = h

//TODO: incorporate 'dirty' from updatemask..	
updateVizValue:: !String !String !*VSt -> (![Visualization],!*VSt)
updateVizValue old new vst=:{currentPath, updates, idPrefix}
	| old == new = (restoreField currentPath updates (dp2id idPrefix currentPath) old,vst)
	= ([TUIUpdate (TUISetValue (dp2id idPrefix currentPath) new)],vst)

getMessageUpdates:: *VSt -> (!Bool,![Visualization], !*VSt)
getMessageUpdates vst=:{updateMask,verifyMask,valid,idPrefix,currentPath}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	# (valid,msg) = verifyElementUpd valid id cmu cmv
	= (valid,msg,{VSt | vst & updateMask = um, verifyMask = vm, valid = valid})
where	
	id = dp2id idPrefix currentPath

verifyElementStr :: !Bool !UpdateMask !VerifyMask -> (!Bool, !String, !String)
verifyElementStr valid cmu cmv = case cmu of
				(Untouched _ _) = case cmv of
					(VMValid mbHnt _ _) 				= (valid,"",mbHintToString mbHnt)
					(VMUntouched mbHnt _ _) 			= (False,"",mbHintToString mbHnt)
					(VMInvalid IsBlankError _ _)		= (False,"","")
					(VMInvalid (ErrorMessage s) _ _)	= (False,s,"")
				_ = case cmv of
					(VMValid mbHnt _ _)				= (valid,"",mbHintToString mbHnt)
					(VMUntouched mbHnt _ _) 		= (False,"",mbHintToString mbHnt)
					(VMInvalid err _ _)				= (False,toString err,"")

verifyElementUpd :: !Bool !String !UpdateMask !VerifyMask -> (!Bool, ![Visualization])
verifyElementUpd valid id cmu cmv = case cmu of
		(Untouched _ _) = case cmv of
			(VMInvalid IsBlankError _ _)   		= (False, []) //filter only isblankerrors or all errors?
			(VMInvalid (ErrorMessage s) _ _) 	= (False, [TUIUpdate (TUISetError id s)])
			(VMUntouched mbHnt _ _)				= (False, hntMsg mbHnt id)
			(VMValid mbHnt _ _) 				= (valid, hntMsg mbHnt id)
		_ = case cmv of
			(VMInvalid err _ _) 				= (False,[TUIUpdate (TUISetError id (toString err))])
			(VMUntouched mbHnt _ _)				= (False, hntMsg mbHnt id)
			(VMValid mbHnt _ _) 				= (valid, hntMsg mbHnt id)
where
	hntMsg h id = case mbHintToString h of "" = []; s = [TUIUpdate (TUISetHint id s)]
	
	
//*********************************************************************************************************************

//Sends the 'old' value if a field has received an update, but it should not be updated.
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