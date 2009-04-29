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

// ***************************************

iTaskEditor :: (a -> Task a) | BoxediData a
iTaskEditor = \a -> editTask "Editor" a

d_iTaskEditor :: !(Dictionary_iData a) -> (a -> Task a)
d_iTaskEditor fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskEditor
    .o 1 0
} 

// ***************************************

iTaskDelegate :: ((a -> Task b) a -> Task b) | BoxediData b
iTaskDelegate = \ataskb vala -> delegateTask ataskb vala
where
	delegateTask ataskb vala
	=							[Text "Choose persons you want to delegate work to:",BrTag [],BrTag []] 
								?>>	chooseUser
		>>= \(wid,worker) -> 	getCurrentUser
		>>= \(_,me) ->			wid @: ("Task for " +++ me, ataskb vala)
		>>= \result -> 			[Text ("Result from " +++ worker), toHtml result] 
								?>> editTask "OK" Void 
		>>= \_ ->				return result


d_iTaskDelegate :: !(Dictionary_iData b) -> ((a -> Task b) a -> Task b)
d_iTaskDelegate fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskDelegate
    .o 1 0
} 

// ***************************************

iTaskDelegateEditor :: (a -> Task a) | BoxediData a
iTaskDelegateEditor = iTaskDelegate iTaskEditor

d_iTaskDelegateEditor :: !(Dictionary_iData a) -> (a -> Task a)
d_iTaskDelegateEditor fun = code {
    .d 2 0
          jmp e_dictionaryUtil_siTaskDelegateEditor
    .o 1 0
} 