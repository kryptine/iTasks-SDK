definition module PrintUtil

// a collection of print routines to write html to Std Output
// (c) MJP 2005

import StdGeneric

:: *HtmlStream	:== [# String !]

:: FoF			:== (*HtmlStream -> *HtmlStream)

// generic function for printing tags
// Constructors are converted to html tag strings
// prefix Name_ of Name_Attrname is removed, and Name is converted to lowercase string

generic gHpr a	:: !*HtmlStream !a -> *HtmlStream		

derive gHpr UNIT, PAIR, EITHER, CONS, OBJECT
derive gHpr Int, Real, Bool, String, Char, []

// the main print routine

print_to_stdout :: !a !*HtmlStream			-> *HtmlStream | gHpr{|*|} a

// instance of toString for an html stream.
instance toString [# String !]

// handy utility print routines	

print 			:: !String 				-> FoF
(<+)  infixl 	:: !*HtmlStream !a 		-> *HtmlStream	| gHpr{|*|} a
(<+>) infixl 	:: !*HtmlStream !FoF 	-> *HtmlStream

htmlAttrCmnd 			:: !hdr !tag !body  	-> FoF | gHpr{|*|} hdr & gHpr{|*|} tag & gHpr{|*|} body
htmlBodylessAttrCmnd	:: !hdr !attr 			-> FoF | gHpr{|*|} hdr & gHpr{|*|} attr
openCmnd 				:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b
styleCmnd 				:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b
styleAttrCmnd 			:: !a !b 				-> FoF | gHpr{|*|} a & gHpr{|*|} b
