implementation module GenVerify

import StdMaybe, StdGeneric, StdBool, StdInt, Text, StdList
import GenUpdate, StdMisc

derive bimap (,), Maybe

derive gError Void, Either
derive gHint  Void, Either

generic gError a :: a *ESt -> *ESt
generic gHint  a :: (Maybe a) *HSt -> *HSt

instance == LabelOrNumber
where
	(==) (Unlabeled a) (Unlabeled b) = a == b
	(==) (Label a) (Label b) = a == b
	(==) _ _ = False

mkHSt :: *HSt
mkHSt = {HSt | dataMask = [], hintMask = [], currentPath = shiftLabeledDataPath []}

mkESt :: *ESt
mkESt = {ESt | dataMask = [], errorMask = [], currentPath = shiftLabeledDataPath []}

determineErrors :: a DataMask -> ErrorMask | gError{|*|} a
determineErrors a mask
	# est = gError{|*|} a {ESt | mkESt & dataMask = mask}
	= est.ESt.errorMask

determineHints :: a DataMask -> HintMask | gHint{|*|} a
determineHints a mask
	# hst = gHint{|*|} (Just a) {HSt | mkHSt & dataMask = mask}
	= hst.HSt.hintMask

// generic error
gError{|UNIT|} 	 	 	      _          est = est
gError{|PAIR|} 			fx fy (PAIR x y) est = fy y (fx x est)
gError{|EITHER|}		fx fy (LEFT x) 	 est = fx x est
gError{|EITHER|}		fx fy (RIGHT y)  est = fy y est
gError{|OBJECT of d|}	fx    (OBJECT x) est=:{ESt | currentPath}
	# est = fx x est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|CONS of d|}		fx    (CONS x)   est=:{ESt | currentPath}
	# est = fx x {ESt | est & currentPath = shiftLabeledDataPath currentPath}
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|FIELD of d|}	fx	  (FIELD x)	 est = fx x est

gError{|Int|} 		_ est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|Real|}		_ est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|Char|}		_ est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|Bool|}		_ est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|String|}	_ est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}

gError{|(,)|}  f1 f2	 	(x1,x2)	   	   est=:{ESt | currentPath}
	# est = f1 x1 {ESt | est & currentPath = shiftLabeledDataPath currentPath}
	# est = f2 x2 est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|(,,)|}  f1 f2 f3	 (x1,x2,x3)	   est=:{ESt | currentPath}
	# est = f1 x1 {ESt | est & currentPath = shiftLabeledDataPath currentPath}
	# est = f2 x2 est
	# est = f3 x3 est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}	
gError{|(,,,)|} f1 f2 f3 f4 (x1,x2,x3,x4)  est=:{ESt | currentPath}
	# est = f1 x1 {ESt | est & currentPath = shiftLabeledDataPath currentPath}
	# est = f2 x2 est
	# est = f3 x3 est
	# est = f4 x4 est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gError{|[]|}		fx	  l		 	 est=:{ESt | currentPath}
	# est = checkItems fx l {ESt | est & currentPath = shiftLabeledDataPath currentPath}
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
where
	checkItems fx []	 est = est
	checkItems fx [i:ix] est = checkItems fx ix (fx i est)

gError{|Maybe|} fx	Nothing est=:{ESt | currentPath} 
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}
gError{|Maybe|} fx (Just x) est=:{ESt | currentPath}
	# est = fx x est
	= {ESt | est & currentPath = stepLabeledDataPath currentPath}

gError{|Dynamic|}	x est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}

//generic hint
gHint{|UNIT|} 	 	      _          		hst = hst
gHint{|PAIR|}		_  _  Nothing	 		hst = hst
gHint{|PAIR|} 		fx fy (Just (PAIR x y)) hst = fy (Just y) (fx (Just x) hst)
gHint{|EITHER|}		_  _  Nothing			hst = hst
gHint{|EITHER|}		fx fy (Just (LEFT x)) 	hst = fx (Just x) hst
gHint{|EITHER|}		fx fy (Just (RIGHT y))	hst = fy (Just y) hst
gHint{|OBJECT|}		_	  Nothing			hst = hst
gHint{|OBJECT|}		fx    (Just (OBJECT x))	hst=:{HSt | currentPath}
	# hst = fx (Just x) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|CONS|}		_	  Nothing	 		hst = hst	
gHint{|CONS|}		fx    (Just (CONS x))	hst=:{HSt | currentPath}
	# hst = fx (Just x) {HSt | hst & currentPath = shiftLabeledDataPath currentPath}
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|FIELD|}		_	  Nothing			hst = hst
gHint{|FIELD|}		fx	  (Just (FIELD x))	hst = fx (Just x) hst

gHint{|Int|} 		_ hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|Real|}		_ hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|Char|}		_ hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|Bool|}		_ hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|String|}		_ hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|(,)|}		_  _   Nothing			hst = hst
gHint{|(,)|}  		f1 f2  (Just (x1,x2))	hst=:{HSt | currentPath}
	# hst = f1 (Just x1) {HSt | hst & currentPath = shiftLabeledDataPath currentPath}
	# hst = f2 (Just x2) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|(,,)|}  	_  _  _  Nothing	   		hst = hst
gHint{|(,,)|}  	f1 f2 f3 (Just (x1,x2,x3))	hst=:{HSt | currentPath}
	# hst = f1 (Just x1) {HSt | hst & currentPath = shiftLabeledDataPath currentPath}
	# hst = f2 (Just x2) hst
	# hst = f3 (Just x3) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}	
gHint{|(,,,)|} _  _  _  _  Nothing				hst = hst
gHint{|(,,,)|} f1 f2 f3 f4 (Just(x1,x2,x3,x4))  hst=:{HSt | currentPath}
	# hst = f1 (Just x1) {HSt | hst & currentPath = shiftLabeledDataPath currentPath}
	# hst = f2 (Just x2) hst
	# hst = f3 (Just x3) hst
	# hst = f4 (Just x4) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|[]|}		_	 Nothing	 hst = hst
gHint{|[]|}		fx	 (Just l) 	 hst=:{HSt | currentPath}
	# hst = checkItems fx l {HSt | hst & currentPath = shiftLabeledDataPath currentPath}
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
where
	checkItems fx []	 hst = hst
	checkItems fx [i:ix] hst = checkItems fx ix (fx (Just i) hst)

gHint{|Maybe|} _  Nothing		  hst = hst
gHint{|Maybe|} fx (Just (Just x)) hst=:{HSt | currentPath} 
	# hst = fx (Just x) hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}
gHint{|Maybe|} fx (Just Nothing) hst=:{HSt | currentPath}
	# hst = fx Nothing hst
	= {HSt | hst & currentPath = stepLabeledDataPath currentPath}

gHint{|Dynamic|}	x hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}

//Utility functions
appendError :: !String !MessagePredicate *ESt -> *ESt
appendError err pred est=:{ESt | currentPath,errorMask} 
	= {ESt | est & errorMask = [(currentPath,pred,err):errorMask]}

appendHint :: !String !MessagePredicate *HSt -> *HSt
appendHint hint pred hst=:{HSt | currentPath,hintMask}
	= {HSt | hst & hintMask = [(currentPath,pred,hint):hintMask]}

instance VerifyState ESt
where
	firstChild :: (*ESt -> *ESt) *ESt -> *ESt
	firstChild f est=:{ESt | currentPath}
		# est = f {ESt | est & currentPath = [Unlabeled 0:currentPath]}
		= {ESt | est & currentPath = currentPath}
	
	nthChild :: !Int !(*ESt -> *ESt) *ESt -> *ESt
	nthChild n f est=:{ESt | currentPath}
		# est = f {ESt | est & currentPath = [Unlabeled n:currentPath]}
		= {ESt | est & currentPath = currentPath}
		
	labeledChild :: !String (*ESt -> *ESt) *ESt -> *ESt
	labeledChild l f est=:{ESt | currentPath}
		# est = f {ESt | est & currentPath = [Label l:currentPath]}
		= {ESt | est & currentPath = currentPath}

	continue :: !*ESt -> *ESt
	continue est=:{ESt | currentPath} = {ESt | est & currentPath = stepLabeledDataPath currentPath}

instance VerifyState HSt
where
	firstChild :: (*HSt -> *HSt) *HSt -> *HSt
	firstChild f hst=:{HSt | currentPath}
		# hst = f {HSt | hst & currentPath = [Unlabeled 0:currentPath]}
		= {HSt | hst & currentPath = currentPath}
	
	nthChild :: !Int (*HSt -> *HSt) *HSt -> *HSt
	nthChild n f hst=:{HSt | currentPath}
		# hst = f {HSt | hst & currentPath = [Unlabeled n:currentPath]}
		= {HSt | hst & currentPath = currentPath}
		
	labeledChild :: !String (*HSt -> *HSt) *HSt -> *HSt
	labeledChild l f hst=:{HSt | currentPath}
		# hst = f {HSt | hst & currentPath = [Label l:currentPath]}
		= {HSt | hst & currentPath = currentPath}
		
	continue :: !*HSt -> *HSt
	continue hst=:{HSt | currentPath} = {HSt | hst & currentPath = stepLabeledDataPath currentPath}

shiftLabeledDataPath :: LabeledDataPath -> LabeledDataPath
shiftLabeledDataPath ldp = [Unlabeled 0:ldp]

stepLabeledDataPath :: LabeledDataPath -> LabeledDataPath
stepLabeledDataPath [Unlabeled x:xs] 	= [Unlabeled (inc x):xs]
stepLabeledDataPath [Label s:xs]		= [Label s:xs]
stepLabeledDataPath [] 					= []

dp2ldp :: DataPath -> LabeledDataPath
dp2ldp dp = [Unlabeled i \\ i <- dataPathList dp]

getErrorMessage :: DataPath DataMask ErrorMask -> String
getErrorMessage dp dmask mmask = join ", " [s \\ (ldp,p,s) <- mmask | eqPath (dataPathList dp) ldp && predValid ldp p dmask]

getHintMessage :: DataPath DataMask HintMask -> String
getHintMessage dp dmask mmask = join ", " [s \\ (ldp,p,s) <- mmask | eqPath (dataPathList dp) ldp && predValid ldp p dmask]

getErrorCount :: DataPath DataMask ErrorMask -> Int
getErrorCount dp dmask mmask = length [s \\ (ldp,p,s) <- mmask | eqPath (dataPathList dp) ldp && predValid ldp p dmask]

predValid :: LabeledDataPath MessagePredicate DataMask -> Bool
predValid ldp MPAlways dm = True
predValid ldp MPIfMasked dm = isMember ldp [[Unlabeled i \\ i<-dp] \\ dp <- dm]

eqPath :: [Int] [LabelOrNumber] -> Bool
eqPath [] 		[] 					= True
eqPath [i:ix]	[(Unlabeled l):lx] 	= i == l && eqPath ix lx
eqPath _		_					= False