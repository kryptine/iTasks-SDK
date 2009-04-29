definition module dictionaryUtil

import iTasks

:: Dictionary_iData a = Dictionary_iData a

class BoxediData a | iData a

get_iDataDictionaries 	:: a -> (!Dictionary_iData a,a) | BoxediData a

iDataVal2Dynamic 		::  a -> Dynamic | BoxediData a
//iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> (b -> Task a)) -> Dynamic | TC b

(OO) infixr 9			:: (dict a -> b) (dict -> a) -> (dict -> b) 
//applyDynamicTask		:: Dynamic (Dictionary_iData b,a) -> Task b | iData b & TC a
applyDynamicTask2 		:: Dynamic a -> Task a | BoxediData a


iTaskEditor 			:: (a -> Task a) | BoxediData a
d_iTaskEditor 			:: !(Dictionary_iData a) -> (a -> Task a)


iTaskDelegate 			:: ((a -> Task b) a -> Task b) | BoxediData b
d_iTaskDelegate 		:: !(Dictionary_iData b) -> ((a -> Task b) a -> Task b)

