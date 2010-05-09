implementation module CommonDomain

import iTasks
import StdOverloaded, StdClass, StdInt, StdMisc, StdArray
import GenPrint, GenParse, GenVisualize, GenUpdate, GenLexOrd
import Text, Time

derive gPrint			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormattedTextControls, FormButton, ButtonState, Color
derive gParse			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormattedTextControls, FormButton, ButtonState, Color
derive gVisualize		EmailAddress, DateTime
derive gUpdate			EmailAddress, Note, DateTime, FormattedText, FormattedTextControls, Color
derive gMerge			EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormattedTextControls, FormButton, ButtonState, Color
derive gMakeSharedCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormattedTextControls, FormButton, ButtonState, Color
derive gMakeLocalCopy	EmailAddress, Password, Note, Date, Time, DateTime, Currency, FormattedText, FormattedTextControls, FormButton, ButtonState, Color
derive gLexOrd			Currency

derive bimap	Maybe, (,)

//VValue a DataMask
gVisualize{|FormButton|} old new vst=:{vizType,label=fLabel,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition
			= ([TUIFragment (TUIFormButtonControl {TUIButtonControl | label = label old, iconCls = icon old, name = dp2s currentPath, id = id, value = toString pressedOld, fieldLabel = labelAttr useLabels fLabel, optional = optional, staticDisplay = renderAsStatic})]
				, 1
				, {VSt | vst & currentPath = stepDataPath currentPath})
		VEditorUpdate
			| pressedOld <> pressedNew	= ([TUIUpdate (TUISetValue id (toString pressedNew))]
											, 1
											, {VSt | vst & currentPath = stepDataPath currentPath})
										= ([],1,{VSt | vst & currentPath = stepDataPath currentPath})
		_
										= ([TextFragment (label old)]
											, 1
											, {VSt | vst & currentPath = stepDataPath currentPath})
where
	id			= dp2id idPrefix currentPath
	
	label b		= case b of (VValue b _) = b.FormButton.label; _ = ""
	icon b		= case b of (VValue b _) = b.icon; _ = ""	
	
	pressedOld	= case old of (VValue ob _) = pressed ob; _ = False
	pressedNew	= case new of (VValue nb _) = pressed nb; _ = True
	pressed b	= case b.FormButton.state of
		Pressed		= True
		NotPressed	= False	

gVisualize{|Password|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUIPasswordControl {TUIBasicControl | name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic})]
								, 1
								, {VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
		VEditorUpdate
			| oldV <> newV	= ([TUIUpdate (TUISetValue id newV)]
								, 1
								, {VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})
		_					= ([TextFragment (foldr (+++) "" (repeatn (size oldV) "*"))],1,{VSt | vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath old optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
		
gVisualize{|Date|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUIDateControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic})]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		VEditorUpdate
			| oldV <> newV 	= ([TUIUpdate (TUISetValue id newV)]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})
		_					= ([TextFragment (toString old)],1,{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
	
gVisualize{|Time|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUITimeControl {TUIBasicControl|name = dp2s currentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic})]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		VEditorUpdate
			| oldV <> newV 	= ([TUIUpdate (TUISetValue id newV)]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})
		_					= ([TextFragment (toString old)],1,{VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath new optional valid})
where
	id		= dp2id idPrefix currentPath
	oldV	= value2s currentPath old
	newV	= value2s currentPath new
	
gVisualize{|Note|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUINoteControl {TUIBasicControl|name = dp2s contentPath, id = id, value = oldV, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic})]
								, 3
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath old optional valid})
		VEditorUpdate
			| oldV <> newV 	= ([TUIUpdate (TUISetValue id newV)]
								, 3
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
		_					= ([HtmlFragment (flatten [[Text line,BrTag []] \\ line <- split "\n" (toString old)])]
								, 3
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath	= shiftDataPath currentPath
	id			= dp2id idPrefix contentPath
	oldV		= value2s contentPath old
	newV		= value2s contentPath new
	
gVisualize{|FormattedText|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUIFormattedTextControl	{ name				= dp2s contentPath
																		, id				= id
																		, value				= replaceMarkers oldV
																		, fieldLabel		= labelAttr useLabels label
																		, optional			= optional
																		, enableAlignments	= controls.alignmentControls
																		, enableColors		= controls.colorControls
																		, enableFont		= controls.fontControl
																		, enableFontSize	= controls.fontSizeControls
																		, enableFormat		= controls.formatControls
																		, enableLinks		= controls.linkControl
																		, enableLists		= controls.listControls
																		, enableSourceEdit	= controls.sourceEditControl
																		}
								)]
								, 0
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath old optional valid})
		VEditorUpdate
			| oldV <> newV	= ([TUIUpdate (TUISetValue id (replaceMarkers newV))]
								, 0
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
		_					# htmlFrag = case old of
								VBlank		= [Text ""]
								VValue v _	= html v
							= ([HtmlFragment htmlFrag]
								, 0
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath	= shiftDataPath currentPath
	id			= dp2id idPrefix contentPath
	oldV		= value2s contentPath old
	newV		= value2s contentPath new
	controls = case old of
		VBlank								= allControls
		VValue (FormattedText _ controls) _	= controls
		
	replaceMarkers v
		# v = replaceSubString SelectionStartMarker ("<span class='marker-start' id='" +++ id +++ "_marker-start'></span>") v
		# v = replaceSubString SelectionEndMarker ("<span class='marker-end' id='" +++ id +++ "_marker-end'></span>") v
		= v

gVisualize{|Color|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid, renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUIColorChooser	{ TUIBasicControl
																| name			= dp2s contentPath
																, id			= id
																, value			= oldV
																, fieldLabel	= labelAttr useLabels label
																, optional		= optional
																, staticDisplay = renderAsStatic
																}
								)]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath old optional valid})
		VEditorUpdate
			| oldV <> newV	= ([TUIUpdate (TUISetValue id newV)]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
		_					# htmlFrag = case old of
								VBlank		= [Text ""]
								VValue v _	= html v
							= ([HtmlFragment htmlFrag]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid contentPath new optional valid})
where
	// Use the path to the inner constructor instead of the current path.
	// This way the generic gUpdate will work for this type
	contentPath	= shiftDataPath currentPath
	id			= dp2id idPrefix contentPath
	oldV		= value2s contentPath old
	newV		= value2s contentPath new

gVisualize{|Currency|} old new vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,valid,renderAsStatic}
	= case vizType of
		VEditorDefinition	= ([TUIFragment (TUICurrencyControl {TUICurrencyControl|id = id, name = dp2s currentPath
												, value = oldV, fieldLabel = labelAttr useLabels label
												, currencyLabel = curLabel old, optional = optional
												, staticDisplay = renderAsStatic})]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid= stillValid currentPath old optional valid})
		VEditorUpdate
			| oldV <> newV 	= ([TUIUpdate (TUISetValue id newV)]
								, 1
								, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})
		_					= ([TextFragment (toString old)], 1, {VSt|vst & currentPath = stepDataPath currentPath, valid = stillValid currentPath new optional valid})
where
	curLabel (VValue (EUR _) _)	= "&euro;"
	curLabel (VValue (GBP _) _)	= "&pound;"
	curLabel (VValue (USD _) _)	= "$"
	curLabel (VValue (JPY _) _) = "&yen;"
	curLabel _					= "&euro;" //Use the default currency
	
	oldV	= value currentPath old
	newV	= value currentPath new
	value dp VBlank			= ""
	value dp (VValue v dm)	= if (isMasked dp dm) (decFormat (toInt v)) ""
	
	id = dp2id idPrefix currentPath

		
gUpdate{|FormButton|} _ ust=:{USt|mode=UDCreate}
	= ({FormButton | label = "Form Button", icon="", state = NotPressed}, ust)
gUpdate{|FormButton|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= ({s & state = if(update == "true") Pressed NotPressed}, toggleMask {USt|ust & mode = UDDone}) 
	| otherwise
		= (s, {USt | ust & currentPath = stepDataPath currentPath})
gUpdate{|FormButton|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
		
gUpdate{|Password|} _ ust=:{USt|mode=UDCreate} 
	= (Password "", ust)
gUpdate{|Password|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (Password update, toggleMask {USt | ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Password|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
	
gUpdate{|Date|} _ ust=:{USt|mode=UDCreate,world}
	# (date,world) = currentDate world
	= (date, {USt|ust & world = world})
gUpdate{|Date|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (fromString update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Date|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})

gUpdate{|Date|} s ust = (s, ust)

gUpdate{|Time|} _ ust=:{USt|mode=UDCreate,world}
	# (time,world) = currentTime world
	= (time, {USt|ust & world = world})
gUpdate{|Time|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (fromString update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Time|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})
gUpdate{|Time|} s ust = (s, ust)

gUpdate{|Currency|} _ ust=:{USt|mode=UDCreate} = (EUR 0, ust)
gUpdate{|Currency|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| currentPath == searchPath
		= (parseUpdate s update, toggleMask {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt| ust & currentPath = stepDataPath currentPath})
where
	parseUpdate orig update =
		 case split "." update of
			[whole]		= replaceVal orig (100 * toInt whole)
			[whole,dec] = replaceVal orig (100 * toInt whole + (if (size dec == 1) (10 * toInt dec) (toInt (dec % (0,1)))))
			_			= orig
	
	replaceVal (EUR _) x = (EUR x)
	replaceVal (GBP _) x = (GBP x)
	replaceVal (USD _) x = (USD x)
	replaceVal (JPY _) x = (JPY x)

gUpdate{|Currency|} s ust=:{USt|mode=UDMask,currentPath,mask}
	= (s, {USt|ust & currentPath = stepDataPath currentPath, mask = appendToMask currentPath mask})	
gUpdate{|Currency|} s ust = (s,ust)

currentTime :: !*World -> (!Time,!*World)
currentTime world
	# (tm,world) = localTime world
	= ({Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec},world)

currentDate :: !*World -> (!Date,!*World)
currentDate world
	# (tm,world) = localTime world
	= ({Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year},world)

currentDateTime :: !*World -> (!DateTime,!*World)
currentDateTime world
	# (tm,world)	= localTime world
	# date			= {Date| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year}
	# time			= {Time|hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}
	= (DateTime date time,world)

instance html Note
where
	html (Note msg) = [Text msg]

instance html FormattedText
where
	html (FormattedText src _) = [RawText (removeMarkers src)]
	
instance html Color
where
	html (Color c) = [DivTag [StyleAttr ("background-color: #" +++ c +++ "; border: 1px solid; border-color: #ACA899; height: 10px; width: 10px;")] []]

instance toString Time
where
	toString {Time|hour,min,sec}	= (pad 2 hour) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)

instance fromString Time
where
	fromString s					= {Time|hour = toInt (s %(0,1)), min = toInt (s %(3,4)), sec = toInt (s %(6,7)) }

instance toString Date
where
	toString {Date|year,mon,day}	= (pad 2 day) +++ "-" +++ (pad 2 mon) +++ "-" +++ (pad 4 year)

instance fromString Date
where
	fromString s					= {Date|day = toInt (s %(0,1)), mon = toInt (s %(3,4)), year = toInt (s %(6,9))}

instance toString DateTime
where
	toString (DateTime d t) = toString d +++ " " +++ toString t

instance toString Note
where
	toString (Note s)				= s
	
instance toString FormattedText
where
	toString (FormattedText s _) = s
	
instance toString Color
where
	toString (Color c) = "#" +++ c

toUnformattedString :: !FormattedText !Bool -> String
toUnformattedString (FormattedText s _) includeCursorMarkers
	# s = insertMarkers s
	# s	= replaceSubString "<br>" "\n" s
	# s	= replaceSubString "<BR>" "\n" s
	# s	= replaceSubString "<br/>" "\n" s
	# s	= replaceSubString "<BR/>" "\n" s
	# s	= replaceSubString "</li>" "\n" s
	# s	= stripHtmlTags s
	# s = replaceSubString "&nbsp;" " " s
	# s = replaceSubString "&lt;" "<" s
	# s = replaceSubString "&gt;" ">" s
	# s = replaceSubString "&amp;" "&" s
	= s
where
	stripHtmlTags s
		# fstOpen	= indexOf "<" s
		# fstClose	= indexOf ">" s
		| fstOpen <> -1 && fstClose <> -1 && fstOpen < fstClose
			= stripHtmlTags (subString 0 fstOpen s +++ subString (fstClose + 1) (textSize s - fstClose) s)
		| otherwise
			= s
			
	insertMarkers s
		# s = case indexOf "<span class=\"marker-start\"" s of
			-1	= s
			n	= subString 0 n s +++ SelectionStartMarker +++ subString (indexOfAfter n "</span>" s + 7) (textSize s) s
		# s = case indexOf "<span class=\"marker-end\"" s of
			-1	= s
			n	= subString 0 n s +++ SelectionEndMarker +++ subString (indexOfAfter n "</span>" s + 7) (textSize s) s
		= s
			
removeMarkers :: !String -> String
removeMarkers s
	# s = replaceSubString SelectionStartMarker "" s
	# s = replaceSubString SelectionEndMarker "" s
	= s
				
instance toString Currency
where
	toString (EUR x) = "EUR " +++ decFormat x
	toString (GBP x) = "GBP " +++ decFormat x
	toString (USD x) = "USD " +++ decFormat x
	toString (JPY x) = "JPY " +++ decFormat x

instance toInt Currency
where
	toInt (EUR val) = val
	toInt (GBP val) = val
	toInt (USD val) = val
	toInt (JPY val) = val
		
instance < Currency
where
	(<) x y = case x =?= y of
		LT	= True
		_	= False

instance < Time
where
	(<) x y
		| x.Time.hour < y.Time.hour															= True
		| x.Time.hour == y.Time.hour && x.Time.min < y.Time.min								= True
		| x.Time.hour == y.Time.hour && x.Time.min == y.Time.min && x.Time.sec < y.Time.sec	= True
		| otherwise																			= False
		
instance < Date
where
	(<) x y 
		| x.Date.year < y.Date.year															= True
		| x.Date.year == y.Date.year && x.Date.mon < y.Date.mon								= True
		| x.Date.year == y.Date.year && x.Date.mon == y.Date.mon && x.Date.day < y.Date.day	= True
		| otherwise																			= False

instance zero Currency
where
	zero = EUR 0

instance + Currency
where
	(+) (EUR x) (EUR y) = EUR (x + y)
	(+) (GBP x) (GBP y) = GBP (x + y)
	(+) (USD x) (USD y) = USD (x + y)
	(+) (JPY x) (JPY y) = JPY (x + y)
	(+) _ _ = abort "Trying to add money of different currencies!"

instance + Time
where
	(+) x y = {Time|hour = x.Time.hour + y.Time.hour, min = x.Time.min + y.Time.min, sec = x.Time.sec + y.Time.sec}

instance + Date
where
	(+) x y = {Date|year = x.Date.year + y.Date.year, mon = x.Date.mon + y.Date.mon, day = x.Date.day + y.Date.day}

instance - Time
where
	(-) x y = {Time|hour = x.Time.hour - y.Time.hour, min = x.Time.min - y.Time.min, sec = x.Time.sec - y.Time.sec}

instance - Date
where
	(-) x y = {Date|year = x.Date.year - y.Date.year, mon = x.Date.mon - y.Date.mon, day = x.Date.day - y.Date.day}

instance - Currency
where
	(-) (EUR x) (EUR y) = EUR (x - y)
	(-) (GBP x) (GBP y) = GBP (x - y)
	(-) (USD x) (USD y) = USD (x - y)
	(-) (JPY x) (JPY y) = JPY (x - y)
	(-) _ _ = abort "Trying to subtract money of different currencies!"
	
//Utility functions
pad :: Int Int -> String
pad len num = (createArray (max 0 (len - size nums)) '0' ) +++ nums
where 
	nums = toString num

decFormat :: Int -> String
decFormat x = toString (x / 100) +++ "." +++ pad 2 (x rem 100)

mkEmptyFormattedText :: !FormattedTextControls -> FormattedText
mkEmptyFormattedText controls = mkFormattedText "" controls

mkFormattedText :: !String !FormattedTextControls -> FormattedText
mkFormattedText src controls = FormattedText src controls

setFormattedTextSrc :: !String !FormattedText -> FormattedText
setFormattedTextSrc cont (FormattedText _ controls) = FormattedText cont controls

getFormattedTextSrc :: !FormattedText -> String
getFormattedTextSrc (FormattedText src _) = src

allControls	:: FormattedTextControls
allControls =	{ alignmentControls	= True
				, colorControls		= True
				, fontControl		= True
				, fontSizeControls	= True
				, formatControls	= True
				, linkControl		= True
				, listControls		= True
				, sourceEditControl	= True
				}
noControls	:: FormattedTextControls
noControls =	{ alignmentControls	= False
				, colorControls		= False
				, fontControl		= False
				, fontSizeControls	= False
				, formatControls	= False
				, linkControl		= False
				, listControls		= False
				, sourceEditControl	= False
				}