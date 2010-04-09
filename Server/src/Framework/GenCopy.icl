implementation module GenCopy

import TSt, StdFunc, StdTuple, DocumentDB, StdMisc
derive bimap (,)

generic gMakeSharedCopy a :: !a !String -> a
gMakeSharedCopy{|Int|}		x _ = x
gMakeSharedCopy{|Real|}		x _ = x
gMakeSharedCopy{|Char|}		x _ = x
gMakeSharedCopy{|Bool|}		x _ = x
gMakeSharedCopy{|String|}	x _ = x
gMakeSharedCopy{|OBJECT|}	f (OBJECT x) sid		= OBJECT (f x sid)
gMakeSharedCopy{|CONS|}		f (CONS x) sid			= CONS (f x sid)
gMakeSharedCopy{|FIELD|}	f (FIELD x) sid			= FIELD (f x sid)
gMakeSharedCopy{|PAIR|}		fx fy (PAIR x y) sid	= PAIR (fx x sid) (fy y sid)
gMakeSharedCopy{|EITHER|}	fx fy (LEFT x) sid		= LEFT (fx x sid)
gMakeSharedCopy{|EITHER|}	fx fy (RIGHT y) sid		= RIGHT (fy y sid)
gMakeSharedCopy{|UNIT|}		UNIT _					= UNIT
gMakeSharedCopy{|Dynamic|}	dyn _	= dyn
gMakeSharedCopy{|Document|}	doc sid	= {Document|doc & type = Shared sid}

derive gMakeSharedCopy [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden

generic gMakeLocalCopy a :: !a !*TSt -> (a,!*TSt)
gMakeLocalCopy{|Int|}		x tst = (x,tst)
gMakeLocalCopy{|Real|}		x tst = (x,tst)
gMakeLocalCopy{|Char|}		x tst = (x,tst)
gMakeLocalCopy{|Bool|}		x tst = (x,tst)
gMakeLocalCopy{|String|}	x tst = (x,tst)
gMakeLocalCopy{|OBJECT|}	f (OBJECT x) tst		= app2 (OBJECT,id) (f x tst)
gMakeLocalCopy{|CONS|}		f (CONS x) tst			= app2 (CONS,id) (f x tst)
gMakeLocalCopy{|FIELD|}		f (FIELD x) tst			= app2 (FIELD,id) (f x tst)
gMakeLocalCopy{|PAIR|}		fx fy (PAIR x y) tst	# (rx,tst) = fx x tst
													= app2 ((PAIR rx),id) (fy y tst)
gMakeLocalCopy{|EITHER|}	fx fy (LEFT x) tst		= app2 (LEFT,id) (fx x tst)
gMakeLocalCopy{|EITHER|}	fx fy (RIGHT y) tst		= app2 (RIGHT,id) (fy y tst)
gMakeLocalCopy{|UNIT|}		UNIT tst				= (UNIT,tst)
gMakeLocalCopy{|Dynamic|}	dyn tst = (dyn,tst)
gMakeLocalCopy{|Document|}	doc=:{content} tst=:{taskNr}
	= case content of
		DocumentContent info = case info.dataLocation of
			SharedLocation _ _
				# (mbDoc,tst) = retrieveDocument info.dataLocation info.DocumentInfo.index tst
				= case mbDoc of
					Just (doc,docdata) | not (isEmptyDoc doc)	= createDocument info.fileName info.mimeType Local (taskNrToString taskNr) docdata tst
					_											= abort "non-empty doc without data"
			LocalLocation _	= ({Document|doc & type = Local},tst)
		EmptyDocument		= ({Document|doc & type = Local},tst)

derive gMakeLocalCopy [], Maybe, Either, (,), (,,), (,,,), Void, Static, Hidden