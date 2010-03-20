implementation module DocumentDomain

from Types import :: Document {..}, :: Task

import GenVisualize, GenUpdate, GenParse, GenPrint
import StdList, StdFile, StdArray, Text
import StdMisc
import Directory, ExtToMime
import TSt, DocumentDB

derive gUpdate PDFDocument, ImageDocument, TextDocument
derive gParse  PDFDocument, ImageDocument, TextDocument
derive gPrint  PDFDocument, ImageDocument, TextDocument

derive bimap (,), Maybe

gVisualize{|PDFDocument|} old new vst=:{vizType, currentPath, valid}
= case VEditorUpdate of
	VEditorUpdate 
		= case new of
		VBlank							= vizDocument VBlank 			VBlank 			  "(pdf)" vst
		(VValue (PDFDocument nv) nmask) = vizDocument (VValue nv nmask) (VValue nv nmask) "(pdf)" {VSt | vst & valid = isValid nv.content ["pdf"] valid}
	_	
		= case old of
		VBlank							= vizDocument VBlank 			VBlank 			  "(pdf)" vst
		(VValue (PDFDocument ov) omask) = vizDocument (VValue ov omask) (VValue ov omask) "(pdf)" {VSt | vst & valid = isValid ov.content ["pdf"] valid}

gVisualize{|ImageDocument|} old new vst=:{vizType, currentPath, valid}
= case VEditorUpdate of
	VEditorUpdate 
		= case new of
		VBlank							  = vizDocument VBlank 			  VBlank 		 	"(image)" vst
		(VValue (ImageDocument nv) nmask) = vizDocument (VValue nv nmask) (VValue nv nmask) "(image)" {VSt | vst & valid = isValid nv.content ["jpg","jpeg","gif","png","bmp"] valid}
	_	
		= case old of
		VBlank							  = vizDocument VBlank 			  VBlank 		    "(image)" vst
		(VValue (ImageDocument ov) omask) = vizDocument (VValue ov omask) (VValue ov omask) "(image)" {VSt | vst & valid = isValid ov.content ["jpg","jpeg","gif","png","bmp"] valid}

gVisualize{|TextDocument|} old new vst=:{vizType, currentPath, valid}
= case VEditorUpdate of
	VEditorUpdate 
		= case new of
		VBlank							  = vizDocument VBlank 			  VBlank 		 	"(plain text)" vst
		(VValue (TextDocument nv) nmask) = vizDocument (VValue nv nmask) (VValue nv nmask) "(plain text)" {VSt | vst & valid = isValid nv.content ["txt"] valid}
	_	
		= case old of
		VBlank							  = vizDocument VBlank 			  VBlank 		    "(plain text)" vst
		(VValue (TextDocument ov) omask) = vizDocument (VValue ov omask) (VValue ov omask) "(plain text)" {VSt | vst & valid = isValid ov.content ["txt"] valid}

// --- Utility functions for visualize
vizDocument :: !(VisualizationValue Document) !(VisualizationValue Document) String !*VSt -> ([Visualization],RenderingHint,*VSt)
vizDocument old new desc vst
	# cp = vst.VSt.currentPath
	#(viz,rh,vst=:{VSt|label, optional}) = gVisualize{|*|} old new {VSt | vst & currentPath = shiftDataPath cp}
	= case viz of
		([(TUIFragment tuidef):vs])
			= ([TUIFragment (TUIPanel {TUIPanel | layout = "", items = [tuidef,(TUIBox {TUIBox|html="<p style=\"margin-left: 3px\">"+++desc})], buttons=Nothing, autoHeight = True, autoWidth = True, border=False, bodyCssClass="", fieldLabel = label2s optional label, renderingHint = rh, unstyled=True})]++vs,
			  rh,
			  {VSt | vst & currentPath = stepDataPath cp})
		_	
			= (viz,0,{VSt | vst & currentPath = stepDataPath cp})

isValid :: !DocumentContent [String] Bool -> Bool
isValid content extList val
	= case content of
		EmptyDocument = val
		DocumentContent info
			# ext = last(split "." info.DocumentInfo.fileName)
			| isMember (toLowerCase ext) (extList) = val
			| otherwise			   				   = False
// ---

// --- Store and retrieve documents from file system (server side)
storeDocumentToFile :: Document String -> Task Bool
storeDocumentToFile doc=:{content} path = mkInstantTask "Store Document to FS" storeDoc
where
	storeDoc tst
		= case content of
			EmptyDocument = (TaskFinished False, tst)
			DocumentContent info
				# (mbData,tst) = retrieveDocument doc tst
				| isJust mbData
					# world	= tst.TSt.world
					// check if the location exists and creat it otherwise
					# ((ok,dir),world) 	= pd_StringToPath path world
					| not ok		   	= (TaskFinished False,{TSt | tst & world = world})
					# (err,world)		= case getFileInfo dir world of
											((DoesntExist,fileinfo),world) = createDirectory dir world
											(_,world)					   = (NoDirError,world)
					# ok				= case err of NoDirError = True; _ = False
					| not ok			= (TaskFinished False,{TSt | tst & world = world})
					# (ok,file,world) 	= fopen (path+++"/"+++info.DocumentInfo.fileName) FWriteData world
					| not ok 			= (TaskFinished False,{TSt | tst & world = world})
					# file 				= fwrites (fromJust mbData) file
					# (ok,world) 		= fclose file world
					| not ok 			= (TaskFinished False,{TSt | tst & world = world})
					= (TaskFinished ok,{TSt | tst & world = world})
				| otherwise = (TaskFinished False,tst)		
	
loadDocumentFromFile :: String String -> Task (Maybe Document)
loadDocumentFromFile fname path = mkInstantTask "Load Document from FS" loadDoc 
where
	loadDoc tst
		# world 			= tst.TSt.world
		# (ok,file,world) 	= fopen (path+++"/"+++fname) FReadData world		
		| not ok 			= (TaskFinished Nothing,{TSt | tst & world = world})
		# (data,file,size) 	= readFile file "" 0
		# (ok,world) 		= fclose file world
		| not ok 			= (TaskFinished Nothing,{TSt | tst & world = world})
		//Find way to derive mime/type
		# mime				= extToMimeType ("."+++last(split "." fname))
		# (doc,tst) 		= createDocument fname mime Local (taskNrToString tst.taskNr) data {TSt | tst & world = world}
		= (TaskFinished (Just doc),tst)

	readFile :: !*File !String !Int -> (!String,!*File,!Int)
	readFile f a s
		# (d,f) = freads f 102400
		| (size d) < 102400 = (a+++d,f,(size d)+s)
		| otherwise = readFile f (a+++d) ((size d)+s)