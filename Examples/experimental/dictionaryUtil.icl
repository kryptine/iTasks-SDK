implementation module dictionaryUtil

import iTasks

:: Dictionary_iData a = Dictionary_iData a

class BoxediData a | iData a

get_iDataDictionaries :: a -> (!Dictionary_iData a,a) | BoxediData a
get_iDataDictionaries a = code {
pop_a 0
}

iDataVal2Dynamic ::  a -> Dynamic | BoxediData a
iDataVal2Dynamic val = dynamic (get_iDataDictionaries val)

iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> b) -> Dynamic | TC b
iDataFun2Dynamic f = dynamic f :: (A.a: (Dictionary_iData a) -> b^)

iTaskEditor :: (a -> Task a) | BoxediData a
iTaskEditor = \a -> editTask "Editor" a

d_iTaskEditor :: !(Dictionary_iData a) -> (a -> Task a)
d_iTaskEditor fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskEditor
    .o 1 0
} 

