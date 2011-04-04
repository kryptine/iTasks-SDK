implementation module ExperimentalDomain

import iTasks, Text, Functor

derive gUpdate			FormattedText, FormattedTextControls, SourceCode, SourceCodeLanguage, Color
derive gDefaultMask		FormattedText, FormattedTextControls, SourceCode, SourceCodeLanguage, Color
derive gVerify			FormattedText, FormattedTextControls, SourceCode, SourceCodeLanguage, Color
derive JSONEncode		FormattedText, FormattedTextControls, SourceCode, SourceCodeLanguage, Color
derive JSONDecode		FormattedText, FormattedTextControls, SourceCode, SourceCodeLanguage, Color
derive JSONEncode		TUIColorChooser, TUISourceCode
derive gEq				FormattedText, SourceCode, Color, FormattedTextControls, SourceCodeLanguage
derive bimap			Maybe, (,)

mkEmptyFormattedText :: !FormattedTextControls -> FormattedText
mkEmptyFormattedText controls = mkFormattedText "" controls

mkFormattedText :: !String !FormattedTextControls -> FormattedText
mkFormattedText src controls = FormattedText src controls

setFormattedTextSrc :: !String !FormattedText -> FormattedText
setFormattedTextSrc cont (FormattedText _ controls) = FormattedText cont controls

getFormattedTextSrc :: !FormattedText -> String
getFormattedTextSrc (FormattedText src _) = removeMarkerTags src
where
	removeMarkerTags s
		# s = case indexOf "<markerstart" s of
			-1	= s
			n	= subString 0 n s +++ subString (indexOfAfter n "</markerstart>" s + 14) (textSize s) s
		# s = case indexOf "<markerend" s of
			-1	= s
			n	= subString 0 n s +++ subString (indexOfAfter n "</markerend>" s + 12) (textSize s) s
		= s

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

gVisualize{|FormattedText|} val vst = visualizeControl (TUICustomControl "itasks.tui.FormattedText" (map (appSnd toJSON) info)) (toString,html) val vst
where
	info =	[ ("enableAlignments",	controls.alignmentControls)
			, ("enableColors",		controls.colorControls)
			, ("enableFont",		controls.fontControl)
			, ("enableFontSize",	controls.fontSizeControls)
			, ("enableFormat",		controls.formatControls)
			, ("enableLinks",		controls.linkControl)
			, ("enableLists",		controls.listControls)
			, ("enableSourceEdit",	controls.sourceEditControl)
			]
	controls = case val of
		Nothing							= allControls
		Just (FormattedText _ controls) = controls
		
	replaceMarkers v
		//# v = replaceSubString SelectionStartMarker ("<markerstart id='" +++ id +++ "_marker-start'></markerstart>") v
		//# v = replaceSubString SelectionEndMarker ("<markerend id='" +++ id +++ "_marker-end'></markerend>") v
		= v
		
toUnformattedString :: !FormattedText !Bool -> String
toUnformattedString (FormattedText s _) includeCursorMarkers
	# s = if includeCursorMarkers (insertMarkers s) s
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
		# s = case indexOf "<markerstart" s of
			-1	= s
			n	= subString 0 n s +++ SelectionStartMarker +++ subString (indexOfAfter n "</markerstart>" s + 14) (textSize s) s
		# s = case indexOf "<markerend" s of
			-1	= s
			n	= subString 0 n s +++ SelectionEndMarker +++ subString (indexOfAfter n "</markerend>" s + 12) (textSize s) s
		= s
			
removeMarkers :: !String -> String
removeMarkers s
	# s = replaceSubString SelectionStartMarker "" s
	# s = replaceSubString SelectionEndMarker "" s
	= s

instance html FormattedText
where
	html (FormattedText src _) = RawText (removeMarkers src)
	
instance toString FormattedText
where
	toString (FormattedText s _) = s
	
mkSourceCode :: !String !SourceCodeLanguage -> SourceCode
mkSourceCode src lang = SourceCode src lang

setSource :: !String !SourceCode -> SourceCode
setSource src (SourceCode _ lang) = SourceCode src lang

getSource :: !SourceCode -> String
getSource (SourceCode src _) = src

:: TUISourceCode =
	{ xtype			:: !String
	, name			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	, language		:: !String //'js', 'css', 'php', 'htm', 'html', 'xml'
	}

:: TUIColorChooser =
	{ xtype			:: !String
	, name			:: !String
	, value			:: !String
	, fieldLabel	:: !Maybe String
	, optional		:: !Bool
	}
	
gVisualize{|SourceCode|} val vst = visualizeControl (TUICustomControl "itasks.tui.SourceCode" [("language",JSONString language)]) (textOnly toString) val vst
where
	language = case val of
		Nothing							= ""
		Just (SourceCode _ lang) 		= case lang of
			JS		= "js"
			CSS		= "css"
			PHP		= "php"
			HTML	= "html"
			XML		= "xml"
			Clean	= "clean"

instance html SourceCode
where
	html (SourceCode src _) = Text src
	
instance toString SourceCode
where
	toString (SourceCode src _) = src

gVisualize{|Color|} val vst = visualizeControl (TUICustomControl "itasks.tui.ColorChooser" []) (toString,html) val vst

instance html Color
where
	html (Color c) = DivTag [StyleAttr ("background-color: #" +++ c +++ "; border: 1px solid; border-color: #ACA899; height: 10px; width: 10px;")] []

instance toString Color
where
	toString (Color c) = "#" +++ c
