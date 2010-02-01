implementation module DocumentDomain

from Types import :: Document {..}
import GenVisualize, GenUpdate, GenParse, GenPrint
import StdList, Text

derive gUpdate PDFDocument, ImageDocument
derive gParse  PDFDocument, ImageDocument
derive gPrint  PDFDocument, ImageDocument

gVisualize{|PDFDocument|} old new vst=:{vizType, currentPath, valid}
= case VEditorUpdate of
	VEditorUpdate 
		= case new of
		VBlank							= vizDocument VBlank 			VBlank 			  "(pdf)" vst
		(VValue (PDFDocument nv) nmask) = vizDocument (VValue nv nmask) (VValue nv nmask) "(pdf)" {VSt | vst & valid = isValid nv.Document.fileName ["pdf"] valid}
	_	
		= case old of
		VBlank							= vizDocument VBlank 			VBlank 			  "(pdf)" vst
		(VValue (PDFDocument ov) omask) = vizDocument (VValue ov omask) (VValue ov omask) "(pdf)" {VSt | vst & valid = isValid ov.Document.fileName ["pdf"] valid}

gVisualize{|ImageDocument|} old new vst=:{vizType, currentPath, valid}
= case VEditorUpdate of
	VEditorUpdate 
		= case new of
		VBlank							  = vizDocument VBlank 			  VBlank 		 	"(image)" vst
		(VValue (ImageDocument nv) nmask) = vizDocument (VValue nv nmask) (VValue nv nmask) "(image)" {VSt | vst & valid = isValid nv.Document.fileName ["jpg","jpeg","gif","png","bmp"] valid}
	_	
		= case old of
		VBlank							  = vizDocument VBlank 			  VBlank 		    "(image)" vst
		(VValue (ImageDocument ov) omask) = vizDocument (VValue ov omask) (VValue ov omask) "(image)" {VSt | vst & valid = isValid ov.Document.fileName ["jpg","jpeg","gif","png","bmp"] valid}

// ---
vizDocument :: !(VisualizationValue Document) !(VisualizationValue Document) String !*VSt -> ([Visualization],RenderingHint,*VSt)
vizDocument old new desc vst
	# cp = vst.VSt.currentPath
	#(viz,rh,vst=:{label, optional}) = gVisualize{|*|} old new {VSt | vst & currentPath = shiftDataPath cp}
	= case viz of
		([(TUIFragment tuidef):vs])
			= ([TUIFragment (TUIPanel {TUIPanel | layout = "", items = [tuidef,(TUIBox {TUIBox|html="<p style=\"margin-left: 3px\">"+++desc})], buttons=[], autoHeight = True, autoWidth = True, border=False, bodyCssClass="", fieldLabel = label2s optional label, renderingHint = rh, unstyled=True})]++vs,
			  rh,
			  {VSt | vst & currentPath = stepDataPath cp})
		_	
			= (viz,0,{VSt | vst & currentPath = stepDataPath cp})

isValid :: String [String] Bool -> Bool
isValid fn extList val
	# ext = last (split "." fn)
	| isMember (toLowerCase ext) (extList) = val
	| otherwise			   				   = False