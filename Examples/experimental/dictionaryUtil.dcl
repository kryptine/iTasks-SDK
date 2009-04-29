definition module dictionaryUtil

import iTasks

:: Dictionary_iData a = Dictionary_iData a

class BoxediData a | iData a


get_iDataDictionaries 	:: a -> (!Dictionary_iData a,a) | BoxediData a

iDataVal2Dynamic 		::  a -> Dynamic | BoxediData a
//iDataFun2Dynamic 		:: (A.a: (Dictionary_iData a) -> b) -> Dynamic | TC b



iTaskEditor 			:: (a -> Task a) | BoxediData a
d_iTaskEditor 			:: !(Dictionary_iData a) -> (a -> Task a)


iTaskDelegate 			:: ((a -> Task b) a -> Task b) | BoxediData b
d_iTaskDelegate 		:: !(Dictionary_iData b) -> ((a -> Task b) a -> Task b)

iTaskDelegateEditor 	:: (a -> Task a) | BoxediData a
d_iTaskDelegateEditor 	:: !(Dictionary_iData a) -> (a -> Task a)
