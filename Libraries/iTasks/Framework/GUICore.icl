implementation module GUICore

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap
import Void, Either
import Text, Html, ExtJS, JSON

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| vizType = VTextDisplay, idPrefix = "", dataPath = [0], label = Nothing, consBody = False, optional = False, blank = False}

//Wrapper functions
visualizeAsEditor :: String a -> [ExtJSDef] | gVisualize{|*|} a
visualizeAsEditor name x = coerceToExtJSDefs (fst (gVisualize{|*|} x x {mkVSt & vizType =VEditorDefinition, idPrefix = name}))
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst (gVisualize{|*|} x x {mkVSt & vizType = VHtmlDisplay})))

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} x x {mkVSt & vizType = VTextDisplay})))

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst (gVisualize{|*|} x x {mkVSt & vizType = VHtmlLabel})))

visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} x x {mkVSt & vizType = VTextLabel})))

determineEditorUpdates	:: String a a -> [ExtJSUpdate]	| gVisualize{|*|} a
determineEditorUpdates name old new = coerceToExtJSUpdates (fst (gVisualize{|*|} old new {mkVSt & vizType = VEditorUpdate, idPrefix = name}))

defaultValue :: a | gUpdate{|*|} a
defaultValue = fst (gUpdate{|*|} undef {USt|mode = UDCreate, searchPath = "", currentPath = [], consPath = [], update = ""})

updateValue :: String String a -> a	| gUpdate{|*|} a 
updateValue path update a
	//Only try to update when the 'path' string is a datapath formatted string
	| isdps path	= fst (gUpdate{|*|} a {USt| mode = UDSearch, searchPath = path, currentPath = [0], consPath = [], update = update})
	| otherwise		= a

//Generic visualizer
generic gVisualize a :: a a *VSt -> ([Visualization], *VSt)

gVisualize{|UNIT|} _ _ vst
	= ([],vst)

gVisualize{|PAIR|} fx fy old new vst=:{blank}
	| blank
		# (vizx, vst) = fx undef undef vst
		# (vizy, vst) = fy undef undef vst
		= (vizx ++ vizy, vst)
	| otherwise
		# (PAIR ox oy,PAIR nx ny) = (old,new)	
		# (vizx, vst) = fx ox nx vst
		# (vizy, vst) = fy oy ny vst
		= (vizx ++ vizy, vst)


gVisualize{|EITHER|} fx fy old new vst=:{vizType,idPrefix,dataPath,consBody,blank}
	| blank
		= fx undef undef vst
	| otherwise
		= case (old,new) of
			//Same structure:
			(LEFT ox, LEFT nx)		= fx ox nx vst
			(RIGHT oy, RIGHT ny)	= fy oy ny vst
			//Different structure:
			(LEFT ox, RIGHT ny)
				= case vizType of
					VEditorUpdate
						# (old,vst) = fx ox ox {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
						# (new,vst) = fy ny ny {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = vizType, idPrefix = idPrefix, dataPath = dataPath, consBody = consBody})
					_
						= fx ox ox vst //Default case: ignore the new value
			(RIGHT oy, LEFT nx)
				= case vizType of
					VEditorUpdate
						# (old,vst) = fy oy oy {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
						# (new,vst) = fx nx nx {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
						= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = vizType, idPrefix = idPrefix, dataPath = dataPath, consBody = consBody})
					_
						= fy oy oy vst //Default case: ignore the new value			
where
	pathid = dp2id idPrefix dataPath		
	
gVisualize{|CONS of d|} fx old new vst=:{vizType,idPrefix,dataPath,label,consBody,optional,blank}
	# (ox,nx) = if blank (undef,undef) (case (old,new) of (CONS ox,CONS nx) = (ox,nx))
	= case vizType of
		//Editor definition
		VEditorDefinition
			# (vizBody,vst)	= fx ox nx {vst & label = Nothing, dataPath = shiftDataPath dataPath, consBody = False}
			| not (isEmpty d.gcd_fields) //Records	
				| dataPathLevel dataPath == 1	//Don't nest on the first level
					= (vizBody, {VSt|vst & dataPath = stepDataPath dataPath})
				| otherwise						//Add a fieldset on nested records
					= ([ExtJSFragment (ExtJSFieldSet {ExtJSFieldSet | id = (dp2id idPrefix dataPath) +++ "-fs"
																	, title = title label
																	, items = coerceToExtJSDefs vizBody
																	, autoHeight = True, border = True})]
																	, {VSt|vst & dataPath = stepDataPath dataPath})	
			| consBody //Normal ADT's without constructor selector
				= (vizBody, {VSt|vst & dataPath = stepDataPath dataPath})
			| otherwise	//Normal ADT's with constructor selector
				= ((consSelector d idPrefix dataPath (label2s optional label)) ++ vizBody, {VSt|vst & dataPath = stepDataPath dataPath})	
		//Html display vizualization
		VHtmlDisplay
			# (vizBody, vst) = fx ox nx {vst & label = Nothing, dataPath = shiftDataPath dataPath}
			| not (isEmpty d.gcd_fields) //Records
				= ([HtmlFragment [TableTag [] (flatten (coerceToHtml vizBody))]],{vst & dataPath = stepDataPath dataPath})
			| otherwise
				= (vizCons ++ vizBody, {vst & dataPath = stepDataPath dataPath})
		_ 	//Other visualizations
			# (vizBody, vst) = fx ox nx {vst & label = Nothing, dataPath = shiftDataPath dataPath}
			= (vizCons ++ vizBody, {vst & dataPath = stepDataPath dataPath})
			
where
	title (Just t)	= t
	title Nothing	= ""
	
	//Do not show constructors that start with an underscore (_Tuple2,_Cons etc.)
	vizCons = if (d.gcd_name.[0] == '_') [] [TextFragment d.gcd_name]

gVisualize{|OBJECT of d|} fx old new vst=:{blank}
	| blank
		= fx undef undef vst
	| otherwise
		= let (OBJECT ox,OBJECT nx) = (old,new) in fx ox nx vst
	
gVisualize{|FIELD of d|} fx (FIELD ox) (FIELD nx) vst=:{vizType}
	= case vizType of
		VHtmlDisplay
			# (vizBody,vst) 	= fx ox nx {VSt |vst & label = Nothing}
			= ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],vst)
		VTextDisplay
			# (vizBody,vst) 	= fx ox nx {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody], vst)
		_
			= fx ox nx {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = value2s blank old, fieldLabel = label2s optional label, allowDecimals = False, numDecimals = 0})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})
		
gVisualize{|Real|} old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = value2s blank old, fieldLabel = label2s optional label, allowDecimals = True, numDecimals = 1000})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})
		
gVisualize{|Char|} old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = value2s blank old, fieldLabel = label2s optional label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})

gVisualize{|Bool|} old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSCheckBox {ExtJSCheckBox|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = if blank "false" (if old "true" "false"), boxLabel = Nothing, fieldLabel = label2s optional label, checked = (if blank False old)})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})		

gVisualize{|String|} old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = value2s blank old, fieldLabel = label2s optional label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment old],{vst & dataPath = stepDataPath dataPath})

gVisualize{|Maybe|} fx old new vst=:{vizType,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition
			# (cblank, cval) = childval blank old 
			# (viz,vst) = fx cval cval {vst & optional = True, blank = cblank}
			= (viz,{vst & optional = optional, blank = blank, dataPath = stepDataPath dataPath})
		VEditorUpdate
			= ([],{vst & dataPath = stepDataPath dataPath})
		_	
			= case old of
				Nothing		= ([TextFragment "-"],vst)
				Just x		= fx x x vst 
where
	childval True	_		= (True, undef)
	childval False	Nothing	= (True, undef)
	childval False (Just x)	= (False, x)


gVisualize{|Dynamic|} old new vst
	= ([],vst)
/*
gVisualize{|[]|} fx old new vst=:{vizType,label,idPrefix,dataPath,optional,blank}
	= case vizType of
		VEditorDefinition
			= (newEntry,{vst & dataPath = stepDataPath dataPath})
		_
			= ([],vst)
where
	newEntry = [ ExtJSFragment (ExtJSCustom (JSON "{ xtype : \"displayfield\", value : \"<a href=\\\"#\\\">Add more</a>\"}"))
			   , ExtJSFragment (ExtJSHtmlPanel {ExtJSHtmlPanel|html = "TEST",border = False, bodyCssClass = ""})
			   ]
	
//Calc body visualizations
//Add list modifier controls
//Add new entry control
//Wrap in panel
*/
derive gVisualize []

derive gVisualize Either, (,), (,,), (,,,), Void

//Generic updater
generic gUpdate a :: a *USt ->  (a, *USt)

gUpdate{|UNIT|} _ ust=:{mode=UDCreate} = (UNIT, ust)
gUpdate{|UNIT|} u ust = (u, ust)

gUpdate{|PAIR|} fx fy p ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	# (ny,ust) = fy y ust
	= (PAIR nx ny, ust)
where
	(PAIR x y) = p
	
gUpdate{|PAIR|} fx fy _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	# (ny,ust) = fy undef ust
	= (PAIR nx ny, ust)
	
gUpdate{|PAIR|} fx fy p ust = (p, ust)

gUpdate{|EITHER|} fx fy e ust=:{mode=UDSearch}
	= case e of
		(LEFT x)	
			# (nx,ust) = fx x ust
			= (LEFT nx, ust)
		(RIGHT y)
			# (ny,ust) = fy y ust
			= (RIGHT ny,ust)
			
gUpdate{|EITHER|} fx fy _ ust=:{mode=UDCreate,consPath}
	= case consPath of
		[ConsLeft:cl]
			# (nx,ust) = fx undef {ust & consPath = cl}
			= (LEFT nx, ust)
		[ConsRight:cl]
			# (ny,ust) = fy undef {ust & consPath = cl}
			= (RIGHT ny, ust)
		[]
			# (nx,ust) = fx undef ust
			= (LEFT nx, ust)

gUpdate{|EITHER|} fx fy e ust = (e, ust)
	
gUpdate{|CONS|} fx c ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (CONS nx, ust)
where
	(CONS x) = c
	
gUpdate{|CONS|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (CONS nx, ust)

gUpdate{|CONS|} fx c ust = (c, ust)

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		# (nx,ust) = fx undef {USt|ust & mode = UDCreate, consPath = path}
		= (OBJECT nx, {USt|ust & mode = UDDone})			 
	| otherwise
		# (nx,ust) = fx x {USt|ust & currentPath = shiftDataPath currentPath}
		= (OBJECT nx, {USt|ust & currentPath = stepDataPath currentPath})
where
	(OBJECT x) = o

	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []
	
gUpdate{|OBJECT|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (OBJECT nx, ust)

gUpdate{|OBJECT|} fx o ust = (o, ust)

gUpdate{|FIELD|} fx f ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (FIELD nx, ust)
where
	(FIELD x) = f
		
gUpdate{|FIELD|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (FIELD nx, ust)

gUpdate{|FIELD|} fx f ust = (f, ust)

gUpdate{|Int|} _ ust=:{USt|mode=UDCreate} = (0,ust)
gUpdate{|Int|} i ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (toInt update, {USt|ust & mode = UDDone})
	| otherwise
		= (i, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Int|} i ust = (i,ust)

gUpdate{|Real|} _ ust=:{USt|mode=UDCreate} = (0.0, ust)
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (toReal update, {USt|ust & mode = UDDone})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Real|} r ust = (r, ust)

gUpdate{|Char|} _ ust=:{USt|mode=UDCreate} = (' ', ust)
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (if (size update > 0) update.[0] c, {USt|ust & mode = UDDone})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Char|} c ust = (c, ust)

gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate} = (False, ust)
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (update == "True", {USt|ust & mode = UDDone})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Bool|} b ust = (b, ust)

gUpdate{|String|} _ ust=:{USt|mode=UDCreate} = ("", ust)
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (update, {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|String|} s ust = (s, ust)


//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

//Specialized instances for [] and Maybe that choose the non-recursive constructor 

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate} = ([], ust)
gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,currentPath}
	# (l,ust) = gUpdateList fx l ust
	= (l,{ust & currentPath = stepDataPath currentPath})
where
	gUpdateList fx [] ust=:{USt|currentPath,searchPath,update}
		| dp2s currentPath == searchPath && update == "_Cons"
			# (a,ust) = fx undef {ust& mode = UDCreate}
			= ([a],{USt|ust & mode = UDDone})	
		| otherwise	
			= ([],ust)
	gUpdateList fx [x:xs] ust=:{USt|currentPath,searchPath,update}
		|dp2s currentPath == searchPath
			| update == "_Nil"
				= ([],{USt|ust & mode = UDDone})
			| otherwise
				= ([x:xs],{USt|ust & mode = UDDone})
		| otherwise
			# (x,ust)	= fx x {ust & currentPath = shiftDataPath currentPath}
			# (xs,ust)	= gUpdateList fx xs {ust & currentPath = stepDataPath (shiftDataPath currentPath)}
			= ([x:xs],ust)
gUpdate{|[]|} fx l ust = (l,ust)

gUpdate{|Maybe|} fx _ ust=:{USt|mode=UDCreate} = (Nothing,ust)
gUpdate{|Maybe|} fx m ust=:{USt|currentPath,searchPath,update}
	| dp2s currentPath == searchPath && update == ""	
		= (Nothing, {USt|ust & mode = UDDone}) //Reset
	| otherwise
		= case m of
			Nothing
				# (x,ust) = fx undef {ust & mode = UDCreate} //Create an empty value to update
				# (x,ust=:{mode,currentPath}) = fx x {ust & mode = UDSearch,currentPath = currentPath, searchPath = searchPath,update = update}
				= case mode of
					UDDone	= (Just x,ust) //Only switch keep newly created value if a field was updated
					_		= (Nothing, ust)
			Just x
				# (x,ust) = fx x ust
				= (Just x,ust)

derive gUpdate Either, (,), (,,), (,,,), Void

//Utility functions
dp2s :: DataPath -> String
dp2s path = join "-" (map toString (reverse path))

dp2id :: String DataPath -> String
dp2id prefix path = prefix +++ "-" +++ dp2s path 

isdps :: String -> Bool
isdps path = and [c == '-' || isDigit c \\ c <-: path]

stepDataPath :: DataPath -> DataPath
stepDataPath []		= []
stepDataPath [x:xs]	= [inc x:xs]

shiftDataPath :: DataPath -> DataPath
shiftDataPath path	= [0:path]

dataPathLevel :: DataPath -> Int
dataPathLevel l	= length l

value2s :: Bool a -> String | toString a
value2s True _	= ""
value2s False a	= toString a

label2s :: Bool (Maybe String) -> Maybe String
label2s _		Nothing		= Nothing
label2s True	(Just l)	= Just (l +++ " (optional)")
label2s False	(Just l)	= Just l

formatLabel :: String -> String
formatLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= [c \\ c <-: label]
	addspace []			= []
	addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

consSelector :: GenericConsDescriptor String DataPath (Maybe String) -> [Visualization]
consSelector d idPrefix dataPath label
	//No choice needed with only one constructor
	| d.gcd_type_def.gtd_num_conses == 1 
		= []
	//Use radiogroup to choose a constructor
	| d.gcd_type_def.gtd_num_conses <= MAX_CONS_RADIO 
		= [ExtJSFragment (ExtJSRadioGroup {ExtJSRadioGroup|name = name, id = id, items = items, fieldLabel = label})]
	//Use combobox to choose a constructor
	| otherwise
		= [ExtJSFragment (ExtJSComboBox {ExtJSComboBox|name = name, id = id, value = d.gcd_name, fieldLabel = label, store = store, triggerAction = "all", editable = False})]
where
	items	= [ExtJSRadio {ExtJSRadio|name = name, value = c.gcd_name, boxLabel = Just c.gcd_name, checked = c.gcd_index == index, fieldLabel = Nothing} 
			  \\ c <- d.gcd_type_def.gtd_conses]
	store	= [(c.gcd_name,c.gcd_name) \\ c <- d.gcd_type_def.gtd_conses]
	name	= dp2s dataPath
	id		= dp2id idPrefix dataPath
	index	= d.gcd_index
	
	
determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = [ExtJSUpdate (ExtJSRemove consid) \\ consid <- collectIds (coerceToExtJSDefs editor)]
where
	collectIds [] = []
	collectIds [ExtJSNumberField {ExtJSNumberField|id}:is] = [id:collectIds is]
	collectIds [ExtJSTextField {ExtJSTextField|id}:is] = [id:collectIds is]
	collectIds [ExtJSCheckBox {ExtJSCheckBox|id}:is] = [id:collectIds is]
	collectIds [ExtJSRadioGroup {ExtJSRadioGroup|id}:is] = [id:collectIds is]
	collectIds [ExtJSFieldSet {ExtJSFieldSet|id}:is] = [id:collectIds is]
	collectIds [_:is] = collectIds is
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = [ExtJSUpdate (ExtJSAdd consid def) \\ def <- coerceToExtJSDefs editor]

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
