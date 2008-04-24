implementation module PrintUtil

import StdArray, StdFile, StdList, StdString, ArgEnv
import StdGeneric
import StdStrictLists
import iDataSettings

generic gHpr a :: !*HtmlStream !a -> *HtmlStream

gHpr{|String|}              file s              = [|s:file]			// the only entry that actually prints something
																	// all others eventually come here converted to string

gHpr{|Int|}                 file i              = [|toString i:file]
gHpr{|Real|}                file r              = [|toString r:file] 
gHpr{|Bool|}                file b              = [|toString b:file]
gHpr{|Char|}                file c              = [|toString c:file]
gHpr{|UNIT|}                file _ 				= file
gHpr{|PAIR|}   gHpra gHprb  file (PAIR a b) 	= gHprb (gHpra file a) b
gHpr{|EITHER|} gHprl gHprr  file (LEFT left) 	= gHprl file left
gHpr{|EITHER|} gHprl gHprr  file (RIGHT right) 	= gHprr file right
gHpr{|OBJECT|} gHpro        file (OBJECT object)= gHpro file object 

gHpr{|CONS of t|} gPrHtmlc prev (CONS c)		// constructor names are printed, prefix Foo_ is stripped
= case t.gcd_name.[0] of
	'`' 	= 	gPrHtmlc prev c					// just skip this constructor name
	else	=	case t.gcd_arity of
					0 = prev <+ myprint t.gcd_name	 
					1 = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name <+ "=\"") c	<+ "\"" 
					n = gPrHtmlc (prev <+ " " <+ myprint t.gcd_name         ) c
where
	myprint :: String -> String
	myprint string = {toLower` char \\ char <-: stripprefix string }
	
	toLower` '_' = '-'
	toLower` c = toLower c 

	stripprefix string 
	# list = fromString string
	| isMember '_' list = toString (tl (dropWhile ((<>) '_') list))
	| otherwise 		= string  

gHpr{|[]|} gHlist file list = myfold file list 
where
	myfold file [x:xs] = myfold (gHlist file x) xs
	myfold file [] = file


// instance of toString for an html stream.
instance toString [# String !]
where
	toString stream
		# n_chars						= count_chars stream 0
		= copy_strings stream n_chars (createArray n_chars '\0')
	where
		count_chars [|]    n = n
		count_chars [|s:l] n = count_chars l (n+size s)
		
		copy_strings [|e:l] i s
			# size_e	= size e
			# i			= i-size_e
			= copy_strings l i (copy_chars e 0 i size_e s)
		copy_strings [|] 0 s
			= s
			
		copy_chars :: !{#Char} !Int !Int !Int !*{#Char} -> *{#Char}
		copy_chars s_s s_i d_i n d_s
			| s_i<n
				# d_s	= {d_s & [d_i]=s_s.[s_i]}
				= copy_chars s_s (s_i+1) (d_i+1) n d_s
				= d_s
		
// utility print functions based on gHpr

print			:: !String -> FoF
print a			= \f -> [|a:f]

(<+)  infixl	:: !*HtmlStream !a -> *HtmlStream | gHpr{|*|} a
(<+)  file new	= gHpr{|*|} file new

(<+>) infixl	:: !*HtmlStream !FoF -> *HtmlStream
(<+>) file new	= new file

print_to_stdout :: !a !*HtmlStream			-> *HtmlStream | gHpr{|*|} a
print_to_stdout value inout = inout <+ value

htmlCmnd		:: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
htmlCmnd		hdr txt			= \file -> closeCmnd hdr (openCmnd hdr "" file <+ txt)

openCmnd		:: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
openCmnd		hdr attr		= \file -> [|"<":file]  <+ hdr <+ attr <+ ">"

closeCmnd		:: !a -> FoF | gHpr{|*|} a
closeCmnd		hdr				= \file -> print "</" file <+ hdr <+ ">"

htmlAttrCmnd	:: !hdr !attr !body -> FoF | gHpr{|*|} hdr & gHpr{|*|} attr & gHpr{|*|} body
htmlAttrCmnd	hdr attr txt	= \file -> closeCmnd hdr (openCmnd hdr attr file <+ txt)

htmlBodylessAttrCmnd	:: !hdr !attr -> FoF | gHpr{|*|} hdr & gHpr{|*|} attr
htmlBodylessAttrCmnd	hdr attr = \file -> [|"<":file]  <+ hdr <+ attr <+ " />"

styleCmnd		:: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
styleCmnd		stylename attr	= \file -> print "." file <+ stylename <+ "{" <+ attr <+ "}"

styleAttrCmnd	:: !a !b -> FoF | gHpr{|*|} a & gHpr{|*|} b
styleAttrCmnd	name value		= \file -> print "" file <+ name <+ ": " <+ value <+ ";"

instance FileSystem NWorld where
	fopen string int nworld=:{worldC}
		# (bool,file,worldC) = fopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

	fclose file nworld=:{worldC}
		# (bool,worldC) = fclose file worldC
		= (bool,{nworld & worldC = worldC})

	stdio nworld=:{worldC}
		# (file,worldC) = stdio worldC
		= (file,{nworld & worldC = worldC})

	sfopen string int nworld=:{worldC}
		# (bool,file,worldC) = sfopen string int worldC
		= (bool,file,{nworld & worldC = worldC})

appWorldNWorld :: !.(*World -> *World) !*NWorld -> *NWorld
appWorldNWorld f nw=:{worldC}
	= {nw & worldC=f worldC}

accWorldNWorld :: !.(*World -> *(.a,*World)) !*NWorld -> (.a,!*NWorld)
accWorldNWorld f nw=:{worldC}
	# (a,worldC)	= f worldC
	= (a,{nw & worldC=worldC})
