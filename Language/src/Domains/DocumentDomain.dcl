definition module DocumentDomain

import GenVisualize, GenUpdate, GenParse, GenPrint
from Types import :: Document {..}, :: Task

//derive gUpdate PDFDocument, ImageDocument, TextDocument
//derive gVisualize PDFDocument, ImageDocument, TextDocument
//derive gParse PDFDocument, ImageDocument, TextDocument
//derive gPrint PDFDocument, ImageDocument, TextDocument

:: PDFDocument = PDFDocument Document
:: ImageDocument = ImageDocument Document
:: TextDocument = TextDocument Document

// Preview type
//:: Preview = Preview Document

// Converter
//class PreviewConverter a
//where
//	convertToPreview :: a -> Preview
		
//instance PreviewConverter PDFDocument
//instance PreviewConverter ImageDocument
//instance PreviewConverter TextDocument
//instance PreviewConverter Document

// Combinators for handling server-side files
//storeDocumentToFile  :: Document String -> Task Bool
