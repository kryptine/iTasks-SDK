definition module DocumentDomain

import GenVisualize, GenUpdate, GenParse, GenPrint
from Types import :: Document {..}

derive gUpdate PDFDocument, ImageDocument
derive gVisualize PDFDocument, ImageDocument
derive gParse PDFDocument, ImageDocument
derive gPrint PDFDocument, ImageDocument

:: PDFDocument = PDFDocument Document
:: ImageDocument = ImageDocument Document