definition  module FormData
 
import 	iTasks
				
derive gPrint 		Form, FormShape
derive gParse 		Form, FormShape
derive gUpdate 		Form, FormShape
derive gVisualize 	Form, FormShape
derive gError		Form, FormShape
derive gHint		Form, FormShape


:: Form			= 	{ formShape :: ![FormShape]
			  		, formDyn	:: !Dynamic
			  		}
:: FormShape 	= 	Integer 	
				| 	Real 		
				| 	String 	
				| 	Bool 		
				| 	Tuple  	!(!FormShape, !FormShape)
				| 	List 	!FormShape
				|	Hide	!FormShape
				| 	Option 	!FormShape
				| 	Labeled !(!String, !FormShape)
				| 	Notes 	
				| 	Date 		
				| 	Time 		
				| 	Document 	
				| 	GoogleMap 

emptyForm 			:: Form
formShapeToFormDyn	:: [FormShape] -> Task Dynamic
