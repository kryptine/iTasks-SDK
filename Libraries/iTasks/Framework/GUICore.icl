implementation module GUICore

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap
import Void, Either
import Text, Html, ExtJS, JSON

MAX_CONS_RADIO :== 3	//When the number of constructors is upto this number, the choice is made
						//with radio buttons. When it exceeds this, a combobox is used.
NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization


mkVSt :: *VSt
mkVSt = {VSt| vizType = VTextDisplay, idPrefix = "", dataPath = [0], label = Nothing, consBody = False}

//Wrapper functions
visualizeAsEditor :: String a -> [ExtJSDef] | gVisualize{|*|} a
visualizeAsEditor name x = coerceToExtJSDefs (fst (gVisualize{|*|} x x {mkVSt & vizType =VEditorDefinition, idPrefix = name}))
	
visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = []

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join "" (coerceToStrings (fst (gVisualize{|*|} x x {mkVSt & vizType = VTextDisplay})))

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x = []

visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join "" (coerceToStrings (fst (gVisualize{|*|} x x {mkVSt & vizType = VTextLabel})))


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

gVisualize{|PAIR|} fx fy (PAIR ox oy) (PAIR nx ny) vst
	# (vizx, vst) = fx ox nx vst
	# (vizy, vst) = fy oy ny vst
	= (vizx ++ vizy, vst)

//Same structure:
gVisualize{|EITHER|} fx fy (LEFT ox) (LEFT nx) vst			= fx ox nx vst
gVisualize{|EITHER|} fx fy (RIGHT oy) (RIGHT ny) vst		= fy oy ny vst
//Different structure:
gVisualize{|EITHER|} fx fy (LEFT ox) (RIGHT ny) vst=:{vizType,idPrefix,dataPath,consBody}
	= case vizType of
		VEditorUpdate
			# (old,vst) = fx ox ox {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
			# (new,vst) = fy ny ny {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
			= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = vizType, idPrefix = idPrefix, dataPath = dataPath, consBody = consBody})
		_
			= fx ox ox vst //Default case: ignore the new value
where
	pathid = dp2id idPrefix dataPath
gVisualize{|EITHER|} fx fy (RIGHT oy) (LEFT nx) vst=:{vizType,idPrefix,dataPath,consBody}
	= case vizType of
		VEditorUpdate
			# (old,vst) = fy oy oy {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
			# (new,vst) = fx nx nx {vst & vizType = VEditorDefinition, dataPath = dataPath, consBody = True}
			= (determineRemovals old ++ determineAdditions pathid new, {vst & vizType = vizType, idPrefix = idPrefix, dataPath = dataPath, consBody = consBody})
		_
			= fy oy oy vst //Default case: ignore the new value
where
	pathid = dp2id idPrefix dataPath
	
gVisualize{|CONS of d|} fx (CONS ox) (CONS nx) vst=:{vizType,idPrefix,dataPath,label,consBody}
= case vizType of
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
			= ((consSelector d idPrefix dataPath label) ++ vizBody, {VSt|vst & dataPath = stepDataPath dataPath})	
	_ 	//Text and html visualization
		# (vizBody, vst) = fx ox nx {vst & label = Nothing, dataPath = shiftDataPath dataPath}
		= (vizBody, {vst & dataPath = stepDataPath dataPath})
		
where
	title (Just t)	= t
	title Nothing	= ""

gVisualize{|OBJECT of d|} fx (OBJECT ox) (OBJECT nx) vst
	= fx ox nx vst

gVisualize{|FIELD of d|} fx (FIELD ox) (FIELD nx) vst=:{vizType}
	= case vizType of
		VTextDisplay
			# (vizBody,vst) 	= fx ox nx {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= ([TextFragment d.gfd_name,TextFragment ": " : vizBody], vst)
		_
			= fx ox nx {VSt |vst & label = Just (formatLabel d.gfd_name)}

gVisualize{|Int|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = toString old, fieldLabel = label, allowDecimals = False})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})
		
gVisualize{|Real|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSNumberField {ExtJSNumberField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = toString old, fieldLabel = label, allowDecimals = True})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})
		
gVisualize{|Char|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = toString old, fieldLabel = label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})

gVisualize{|Bool|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSCheckBox {ExtJSCheckBox|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = old, fieldLabel = label, checked = old})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment (toString old)],{vst & dataPath = stepDataPath dataPath})		

gVisualize{|String|} old new vst=:{vizType,label,idPrefix,dataPath}
	= case vizType of
		VEditorDefinition	= ([ExtJSFragment (ExtJSTextField {ExtJSTextField|name = dp2s dataPath, id = dp2id idPrefix dataPath, value = old, fieldLabel = label})], {VSt|vst & dataPath = stepDataPath dataPath})
		_					= ([TextFragment old],{vst & dataPath = stepDataPath dataPath})


gVisualize{|Dynamic|} old new vst
	= ([],vst)

derive gVisualize [], Maybe, Either, (,), (,,), (,,,), Void

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
gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch}
	= gUpdateList fx l ust
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
	| dp2s currentPath == searchPath
		| update == "Nothing"
			= (Nothing, {USt|ust & mode = UDDone})
		| update == "Just"
			# (a,ust) = fx undef {ust & mode = UDCreate}
			= (Just a, {USt|ust & mode = UDDone})
		| otherwise
			= (m, {USt|ust & mode = UDDone})
	| otherwise
		= case m of
			Nothing
				= (Nothing,{ust & currentPath = stepDataPath currentPath})
			(Just x)
				# (x,ust) = fx x {ust & currentPath = shiftDataPath currentPath}
				= (Just x, {ust & currentPath = stepDataPath currentPath})


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
		= [] //TODO
where
	items	= [ExtJSRadio {ExtJSRadio|name = name, value = c.gcd_name, boxLabel = Just c.gcd_name, checked = c.gcd_index == index, fieldLabel = Nothing} 
			  \\ c <- d.gcd_type_def.gtd_conses]
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