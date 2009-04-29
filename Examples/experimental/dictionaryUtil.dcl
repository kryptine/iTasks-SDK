definition module dictionaryUtil

import iTasks

:: Dictionary_iData a = Dictionary_iData a

class BoxediData a | iData a

iDataVal2Dynamic 		::  a -> Dynamic | BoxediData a
//iDataFun2Dynamic 		:: (A.a: (Dictionary_iData a) -> b) -> Dynamic | TC b



iTaskEditor 			:: (a -> Task a) | BoxediData a
d_iTaskEditor 			:: !(Dictionary_iData a) -> (a -> Task a)
