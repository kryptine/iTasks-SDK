definition module iDataExceptions

import iDataForms

//	Exception handling and storage.

:: Judgement		:==	Maybe (String,String)	// id + message					
Ok 					:: Judgement
noException			:: !Judgement -> Bool
yesException		:: !Judgement -> Bool

instance			+ Judgement

ExceptionStore		:: !(Judgement -> Judgement) !*HSt -> (Judgement,!*HSt)
