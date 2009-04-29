module tryout

import iTasks
import dictionaryUtil
import iDataTrivial

Start world = startEngine tryout world

tryout :: [Workflow]
tryout
=	[{ name		= "tryout"
	 , label	= "tryout"
	 , roles	= []
	 , mainTask	= myChangedWorkflow >>| return Void
	 }]


myfun :: Task [Int]
myfun = apply (iDataFun2Dynamic d_iTaskEditor) (iDataVal2Dynamic [0])

myfun2 :: Task [Int]
myfun2 = apply (iDataFun2Dynamic d_iTaskDelegateEditor) (iDataVal2Dynamic [0])



apply :: Dynamic Dynamic -> Task c | iData c
apply 
	(edit::A.b: (Dictionary_iData b) -> (a -> Task b)) ((dict,val)::(!Dictionary_iData c^,a)) = edit dict val 
apply _ _ = return createDefault

iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> (b -> Task a)) -> Dynamic | TC b
iDataFun2Dynamic f = dynamic f :: (A.a: (Dictionary_iData a) -> (b^ -> Task a))



// ********************************



myChangedWorkflow = pushChangeRequest whenToApply whatToApply myWorkflow


myWorkflow 
	= 	parallel "andTasks"  (\_ -> False) id  [(toString i, myTask i) \\ i <- [0..5]] >>|
		parallel "andTasks"  (\_ -> False) id  [(toString i, myTask i) \\ i <- [0..5]]
where
	myTask val = normalTask val <\/> alternativeTask val

	normalTask val = editTask ("Normal OK" <+++ val) val 

//	alternativeTask :: a Dynamic -> Task a
	alternativeTask val dyn = apply dyn (iDataVal2Dynamic val)


whatToApply = iDataFun2Dynamic d_iTaskDelegateEditor

whenToApply = CC (pred 30)
where
	pred 0 tst =	({newCondition = Nothing, 				 isApplicable = False, applyChange = False},tst)
	pred n tst =	({newCondition = Just (CC (pred (n-1))), isApplicable = True,  applyChange = isEven n},tst)

	



