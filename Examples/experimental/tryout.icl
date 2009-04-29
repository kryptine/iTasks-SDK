module tryout

import iTasks
import dictionaryUtil


Start world = startEngine tryout world

tryout :: [Workflow]
tryout
=	[{ name		= "tryout"
	 , label	= "tryout"
	 , roles	= []
	 , mainTask	= myfun
	 }]


myfun :: Task Void
myfun = apply (iDataFun2Dynamic d_iTaskEditor) (iDataVal2Dynamic 23)
//myfun = apply (dynamic d_iTaskEditor) (iDataVal2Dynamic 23)

apply :: Dynamic Dynamic -> Task Void
apply 
	(edit::A.b: (Dictionary_iData b) -> (b -> Task b)) 
	((dict,val)::(!Dictionary_iData a,a)) 
		= edit dict val >>| return Void
dyn_iDataApply _ _ = return Void

/* JOHN : de volgende functie wil ik graag in een module stoppen, maar ik krijg hem niet geexporteerd
*/

iDataFun2Dynamic :: (A.a: (Dictionary_iData a) -> b) -> Dynamic | TC b
iDataFun2Dynamic f = dynamic f :: (A.a: (Dictionary_iData a) -> b^)

