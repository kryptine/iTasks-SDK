implementation module iTasks.Gin.Printer

import StdList
import StdString
import StdOverloaded, StdInt, StdMisc

import Data.Map

from Text.PPrint import ::Doc, ::SimpleDoc, display, renderPretty
from Text.PPrint import qualified <$>, <+>, <->, </>, align, empty, space, comma, line, char, text, string
from Text.PPrint import qualified parens, braces, brackets, dquotes, tupled
from Text.PPrint import qualified punctuate, hsep, vsep, vcat, fillSep, hang, indent, renderPretty, display

from iTasks.Gin.Parser import ::GPath(..), :: GPathNode(..), ::GResourceId(..)

prettyPrint :: Doc -> String
prettyPrint doc = 'Text.PPrint'.display ('Text.PPrint'.renderPretty 0.9 120 doc)

instance Printer Doc
where
	def a = 'Text.PPrint'.hang 4 a
	scope docs = 'Text.PPrint'.vsep docs
	newscope docs = 'Text.PPrint'.indent 4 (scope docs)
	align a = 'Text.PPrint'.align a
	(<->) a b = a 'Text.PPrint'. <-> b
	(<+>) a b = a 'Text.PPrint'. <+> b
	(<-/>) a b = a 'Text.PPrint'. </> b
	(<-$>) a b = a 'Text.PPrint'. <$> b
	(<$?>) a b = a 'Text.PPrint'. <$> b
	empty = 'Text.PPrint'.empty
	space = 'Text.PPrint'.space
	comma = 'Text.PPrint'.comma
	char c = 'Text.PPrint'.char c
	text s = 'Text.PPrint'.text s
	string s = 'Text.PPrint'.string s
	parens doc = 'Text.PPrint'.parens doc
	brackets doc = 'Text.PPrint'.brackets doc
	braces doc = 'Text.PPrint'.braces doc
	dquotes doc = 'Text.PPrint'.dquotes doc
	tupled docs = 'Text.PPrint'.tupled docs
	punctuate doc docs = 'Text.PPrint'.punctuate doc docs
	hsep docs = 'Text.PPrint'.hsep docs
	fillSep docs = 'Text.PPrint'.fillSep docs
	position _ = 'Text.PPrint'.empty

::  PDoc = Empty
		| Text String //must not contain newlines
		| Line PDoc
		| Cat PDoc PDoc
		| Position GPath

positionPrint :: PDoc -> (String, LineMap)
positionPrint doc 
	# (s,_,paths) = pp doc 1
	= (s,fromList paths)
where
	pp :: PDoc Int -> (String, Int, [(Int, GPath)])
	pp Empty      nr		= ("", nr, [])
	pp (Text s)   nr		= (s , nr, [])
	pp (Line a)   nr		# (s, nr`, paths) = pp a (nr + 1)
							= ("\n" +++ s, nr`, paths)
	pp (Cat a b)  nr		# (s1, nr, paths1) = pp a nr
							# (s2, nr, paths2) = pp b nr
							= (s1 +++ s2, nr, paths1 ++ paths2)
	pp (Position path) nr	= ("", nr, [(nr,path)])

instance Printer PDoc
where
	def a = Cat a (char ';')
	scope docs = fold (<-$>) docs
	newscope docs = braces (scope docs)
	align a = a
	(<->) a b = Cat a b
	(<+>) a b = Cat a (Cat space b)
	(<-/>) a b = a <+> b
	(<-$>) a b = Cat a (Line b)
	(<$?>) a b = a <+> b
	empty = Empty
	space = Text " " 
	comma = Text ","
	char c = Text (toString c)
	text s = Text s
	string s = Text s
	parens doc = enclose "(" doc ")"
	brackets doc = enclose "[" doc "]"
	braces doc = enclose "{" doc "}"
	dquotes doc = enclose "\"" doc "\""
	tupled docs = parens (hsep (punctuate comma docs))
	punctuate p []     = []
	punctuate p [d]    = [d]
	punctuate p [d:ds] = [(d <-> p) : punctuate p ds]
	hsep docs = fold (<->) docs
	fillSep docs = fold (<-/>) docs
	position p = Position p

enclose :: !String !PDoc !String -> PDoc
enclose l doc r  = Cat (Text l) (Cat doc (Text r))

fold :: (a a -> a ) [a] -> a | Printer a
fold f []       = empty
fold f ds       = foldr1 f ds

foldr1 :: (a a -> a) [a] -> a | Printer a
foldr1 f [x]      = x
foldr1 f [x:xs]   = f x (foldr1 f xs)
