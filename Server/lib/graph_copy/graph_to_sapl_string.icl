implementation module graph_to_sapl_string

import StdEnv
import StdStrictLists
import graph_to_string_with_descriptors


import StdDebug
//import sapldebug
// Conversion of dynamic graph string to sapl code
// JMJ 2007

// Simplified sapl representation
:: DynamicSapl = IntS Int | BoolS Bool | CharS Char | RealS Real | StringS String | CstrS String String Int [DynamicSapl] | 
                 FunctionS String String Int [DynamicSapl] | ArrayS Int [DynamicSapl] | ListS [DynamicSapl] |
                 TupleS Int [DynamicSapl] | RecS String String Int [DynamicSapl]

makeSaplExpression :: !DynamicSapl -> String
makeSaplExpression e = mkse e
where
 mkse (IntS i)              = toString i
 mkse (BoolS b)             = toString b
 mkse (CharS c)         	= toString c
 mkse (RealS r)         	= toString r
 mkse (StringS s)       	= "\"" +++  s  +++ "\""
 mkse (CstrS mod name _ []) = makePrintableName (mod +++ "." +++ makeSaplName name)
 mkse (CstrS mod name _ as) = "(" +++ makePrintableName (mod +++ "."  +++ makeSaplName name) +++ args as +++ ")"
 mkse (FunctionS mod name _ []) = makePrintableName (mod +++ "."  +++ makeSaplName name)
 mkse (FunctionS mod name _ as) = "(" +++ makePrintableName (mod +++ "."  +++ makeSaplName name) +++ args as +++ ")"
 mkse (RecS mod name _ []) 	= makePrintableName (makeRecName mod name)
 mkse (RecS mod name _ as) 	= "(" +++ makePrintableName (makeRecName mod name) +++ args as +++ ")"
 mkse (ArrayS  _ as)        = mkl as
 mkse (ListS    as)         = mkl as
 mkse (TupleS n as)         = "(_predefined._Tuple" +++ toString n +++ args as +++ ")"


 args []                = ""
 args [a:as]      		= " " +++ mkse a +++ args as
 mkl  []                = "_predefined._Nil"
 mkl  [a:as]            = "(_predefined._Cons " +++ mkse a +++ " " +++ mkl as +++ ")"
 
instance toString DynamicSapl
where toString (IntS i)   	= "IntS "  +++ toString i
      toString (BoolS i) 	= "BoolS "  +++ toString i
      toString (CharS i) 	= "CharS "  +++ toString i
      toString (RealS i) 	= "RealS "  +++ toString i      
      toString (StringS i) 	= "StringS "  +++ toString i

sapl_from_string 	:: !*{#Char} -> (.a,!Int)
sapl_from_string  str = (undef,3)

graph_to_sapl_string :: !a -> String
graph_to_sapl_string a = makeSaplExpression (graph_to_sapl_dynamic a)

string_to_graph :: !String -> .a
string_to_graph thread = abort "Cannot create Sapl graph while you are in Clean.\n"

graph_to_sapl_dynamic :: !a -> DynamicSapl
graph_to_sapl_dynamic graph
      # (g,d,m) = graph_to_string_with_descriptor_and_module_table graph
      # (v,_) = convertfromdyn g d m
      = v

// Testing function, also gives decoding
graph_to_sapl_dynamic_test :: !a -> (({#Int},Char,{#String},Char,{#String}),Char,DynamicSapl,Char,String)
graph_to_sapl_dynamic_test graph  
      # (g,d,m) = graph_to_string_with_descriptor_and_module_table graph
      # (v,_) = convertfromdyn g  d m
      = (dyndesc graph,'\n',v,'\n',graph_to_sapl_string graph)

dyndesc gg # (g,d,m) = graph_to_string_with_descriptor_and_module_table gg
	       = (string_to_int_array g,'\n',d,'\n',m);

// Decoding functions
string_to_int_array :: !{#Char} -> {#Int}
string_to_int_array s
	= {select_int_from_string i s\\i<-[0,IF_INT_64_OR_32 8 4..size s-IF_INT_64_OR_32 7 3]}

select_int_from_string i s
	= IF_INT_64_OR_32
		(toInt s.[i]+(toInt s.[i+1]<<8)+(toInt s.[i+2]<<16)+(toInt s.[i+3]<<24)
		+(toInt s.[i+4]<<32)+(toInt s.[i+5]<<40)+(toInt s.[i+6]<<48)+(toInt s.[i+7]<<56))
		(toInt s.[i]+(toInt s.[i+1]<<8)+(toInt s.[i+2]<<16)+(toInt s.[i+3]<<24));

sifs = select_int_from_string
sbfs i s = int2bool (sifs i s)
scfs i s = toChar(sifs i s)

srfs i s
	= IF_INT_64_OR_32
		(make_real_from_int (select_int_from_string i s))
		(make_real_from_2_ints (select_int_from_string i s) (select_int_from_string (i+4) s))
where
	make_real_from_int :: !Int -> Real
	make_real_from_int r = code {
		pop_b 0
	}

	make_real_from_2_ints :: !Int !Int -> Real
	make_real_from_2_ints r0 r1 = code {
		pop_b 0
	}

int2bool 0 = False
int2bool 1 = True

arity c | '0' <= c && c <= '9' = toInt c - toInt '0'
                               = toInt c - toInt 'A' + 10
                               
selectmodnr i s = toInt s.[i]+(toInt s.[i+1]<<8)

getName :: Int String -> String
getName i s = s % (i,poszero-1)
where poszero = hd [n\\ n <- [i+1..(size s)]| s.[n] == '\0']
    
convertfromdyn str ds md = decodeDyn 0
where                               
	decodeDyn pos
	# dnr               = sifs pos str
	| dnr < 0           = getEarlierElem (pos + dnr - 1) (pos+IF_INT_64_OR_32 8 4) // shared node
	# desc_type         = ds.[dnr-1].[0]
	# next_pos = pos+IF_INT_64_OR_32 8 4
	| desc_type == 'i'  = (IntS (sifs next_pos str),pos+IF_INT_64_OR_32 16 8) // Int
	| desc_type == 'c'  = (CharS (scfs next_pos str),pos+IF_INT_64_OR_32 16 8) // Char
	| desc_type == 'b'  = (BoolS (sbfs next_pos str),pos+IF_INT_64_OR_32 16 8) // Bool
	| desc_type == 's'  = readString pos // String in array
	| desc_type == 'r'  = (RealS (srfs next_pos str),pos+IF_INT_64_OR_32 16 12) // Real	
	//| desc_type == 'C' && size str > pos + 4 && sifs (pos+4) str < 0         // shared node in array
	                    //= getEarlierElem (pos + 4 + sifs (pos+4) str + 3) (pos+8)       
	| desc_type == 'C'
		| size str > next_pos && sifs next_pos str < 0 // shared node in constructor
	                    = makeBoxedConstr  pos
		| size str > next_pos && ds.[sifs next_pos str - 1].[0] == 's' // String
	                    = readString next_pos
		| size str > next_pos && ds.[sifs next_pos str - 1].[0] == 'a'
			| sifs (pos+IF_INT_64_OR_32 24 12) str <> 0 // unboxed array 
	                    # typedes = ds.[sifs (pos+IF_INT_64_OR_32 24 12) str-1]
	                    # ssize = sifs (pos+IF_INT_64_OR_32 16 8) str
	                    = makeUnboxedArray  typedes ssize (pos+IF_INT_64_OR_32 32 16)
    		//sifs (pos+IF_INT_64_OR_32 24 12) str == 0 // boxed array
	                    # ssize = sifs (pos+IF_INT_64_OR_32 16 8) str
	                    = makeBoxedArray ssize (pos+IF_INT_64_OR_32 32 16)
		// boxed constructor or partial application
		= makeBoxedConstr pos
	| desc_type == ':' // boxed list
	                    = makeBoxedList pos 
	| desc_type == 'R'
		| ds.[dnr-1].[5] == 'l' && ds.[dnr-1].[6] == 'R'// unboxed list of records
	                    = makeUnBoxedListOfRecords pos
		| ds.[dnr-1].[5] == 'l'// unboxed list 
	                    = makeUnBoxedList ds.[dnr-1].[6] pos
		// records constructor & unboxed constructors
	    = makeRecord pos
	| desc_type == 'n' = (ListS [],next_pos)// empty list 
	| desc_type == 't' // tuple
	                    = makeTuple (arity ds.[dnr-1].[1]) next_pos

	getEarlierElem pos newpos    // backward ref
	# dnr               = sifs (pos) str   // descriptor is in word before
	# desc_type         = ds.[dnr-1].[0]
	| desc_type == 's'  = (fst (decodeDyn pos),newpos)  // string case
	                    = (fst (decodeDyn pos),newpos)

	readString pos
		# strsize = sifs (pos+IF_INT_64_OR_32 8 4) str
		  first_char_pos = pos + IF_INT_64_OR_32 16 8
		  newpos = first_char_pos + ((strsize + IF_INT_64_OR_32 7 3) bitand (IF_INT_64_OR_32 (-8) (-4)))
		= (StringS (str % (first_char_pos,first_char_pos + strsize - 1)), newpos)

	makeUnboxedArray typedes size pos  
	    | typedes%(0,0) == "i" || typedes%(0,0) == "b" || typedes%(0,0) == "c" 
	     # (elems,rest) = readUMany typedes.[0] size pos  []
	     = (ArrayS size elems,rest)
	    | typedes%(0,0) == "R"  
	    = makeUnBoxedArrayOfRecords size (pos-IF_INT_64_OR_32 8 4)
	     
	
	readUDMany types 0 pos res        = (res,pos)
	readUDMany ['r':types] n pos res = readUDMany types (n-IF_INT_64_OR_32 1 2) (pos+8) (res ++ [makeRealType pos])
	readUDMany [type:types] n pos res = readUDMany types (n-1) (pos+IF_INT_64_OR_32 8 4) (res ++ [makeType type pos])
	
	readUMany type 0 pos res = (res,pos)
	readUMany type=:'r' n pos res = readUMany type (n-2) (pos+IF_INT_64_OR_32 16 8) (res ++ [makeRealType pos])
	readUMany type n pos res = readUMany type (n-1) (pos+IF_INT_64_OR_32 8 4) (res ++ [makeType type pos])
	
	makeBoxedArray size pos 
	    # (elems,pos) = (readMany size pos [])
	    = (ArrayS size elems,pos)
	
	makeTuple size pos 
	    # (elems,pos) = (readMany size pos [])
	    = (TupleS size elems,pos)
	
	makeRecord pos 
	    # dnr         = sifs pos str 
	    # desc        = ds.[dnr-1]
	    #(name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc) 
	                  = makeRecordTypeDesc desc
	    # (ubels,pos) = readUDMany ubtypes nrub (pos+IF_INT_64_OR_32 8 4) []
	    # (bels,pos)  = readMany nrpointer pos []
	    # mergedelems = merge_elems alltypes ubels bels
	    # typedelems  = setTypes (makeRecordType typedesc) mergedelems
	    = if (desc.[5]== 'd') (CstrS modname name tsize typedelems,pos)(RecS modname name tsize typedelems,pos)

    makeRecordTypeDesc desc    
 	    # tsize       = arity desc.[1]
	    # nrpointer   = arity desc.[2]
	    # nrub        = tsize - nrpointer
	    # modnr       = selectmodnr 3 desc
	    # start_types = if (desc.[5] == 'd') 6 (if (desc.[5] == 'l' && desc.[6] == 'R') 7 5)
	    # modname     = md.[modnr-1]
	    # typedesc    = takeWhile (\a -> a <> '\0') [c\\ c <-: desc%(start_types,size str-1)]
	    # alltypes    = [t\\ t <- typedesc| (t <> '(') && (t <> ')') && (t <> ',')]
	    # ubtypes     = [c\\ c <- alltypes| c <> 'a']
	    # name        = getName (start_types+length typedesc+1) desc 
        | start_types <> 7 = (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc)   // normal record
                           = (name,modname,tsize-1,nrpointer-1,nrub,droplast alltypes,ubtypes,droplast typedesc)   // list: drop last of pointer part (= pointer to tail)
    

	merge_elems [] _ _                  = [] 
	merge_elems ['a':types]  ubels bels = [hd bels  : merge_elems types ubels (tl bels)]
	merge_elems [_:types]    ubels bels = [hd ubels : merge_elems types (tl ubels) bels]
	
	readMany  0 pos res = (res,pos)
	readMany  n pos res 
	                 # (elem,newpos) = decodeDyn pos
	                 = readMany (n-1) newpos (res ++ [elem])

	makeBoxedConstr pos 
	# dnr               = sifs pos str 
	# desc              = ds.[dnr-1]
	# desc_type         = desc.[0]
	# nrargs            = arity (ds.[dnr-1].[1])
	# modnr             = selectmodnr 3 desc
	# name              = getName 5 desc
	# modname           = md.[modnr-1]
	# (elems,newpos)    = readMany nrargs (pos+IF_INT_64_OR_32 8 4) []
	| name == "ARRAY" = (hd elems,newpos)  // array or string
	                  = (CstrS modname name nrargs elems,newpos)
	    
	makeBoxedList pos 
	    # (elems,newpos) = readListElems pos [] 
	    = (ListS  elems,newpos)

	makeUnBoxedList type pos 
	    # (elems,newpos) = readUBListElems type pos [] 
	    = (ListS  elems,newpos)

	readListElems pos  elems 
	# dnr               = sifs pos str
	| dnr < 0           = (elems,pos+IF_INT_64_OR_32 8 4) // always nil
	# desc_type         = ds.[dnr-1].[0]
	|  desc_type == ':' 
	   # (elem,newpos) = decodeDyn (pos+IF_INT_64_OR_32 8 4) 
	   = readListElems newpos (elems++[elem]) 
	   = (elems,pos+IF_INT_64_OR_32 8 4)
	
	readUBListElems type pos elems   
	# dnr               = sifs pos str 
	| dnr < 0           = (elems,pos+IF_INT_64_OR_32 8 4) // always nil
	# desc_type         = ds.[dnr-1].[0]
	| desc_type == 'R'
	   | type=='r'
		   # elem = makeRealType (pos+IF_INT_64_OR_32 8 4) 
		   = readUBListElems type (pos+IF_INT_64_OR_32 16 12) (elems++[elem]) 
		   # elem = makeType type (pos+IF_INT_64_OR_32 8 4) 
		   = readUBListElems type (pos+IF_INT_64_OR_32 16 8) (elems++[elem]) 
	   = (elems,pos+IF_INT_64_OR_32 8 4)

	makeUnBoxedArrayOfRecords size pos 
	    # dnr         = sifs pos str 
	    # desc        = ds.[dnr-1]
	    # typedes     = makeRecordTypeDesc desc
	    # (elems,pos) = readUBArrayRecordElems size (pos+IF_INT_64_OR_32 8 4) typedes []
	    = (ArrayS size elems,pos)

	makeUnBoxedListOfRecords pos 
	    # dnr         = sifs pos str 
	    # desc        = ds.[dnr-1]
	    # typedes     = makeRecordTypeDesc desc
	    # (elems,pos) = readUBListRecordElems pos typedes []
	    = (ListS elems,pos)

	readUBListRecordElems pos (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc)  elems
	# dnr               = sifs pos str 
	| dnr < 0           = (elems,pos+IF_INT_64_OR_32 8 4) // always nil
	# desc_type         = ds.[dnr-1].[0]
	|  desc_type == 'R' 
	# (ubels,pos) = readUDMany ubtypes nrub (pos+IF_INT_64_OR_32 8 4) []
	# (bels,pos)  = readMany nrpointer pos []
	# mergedelems = merge_elems alltypes ubels bels
	# typedelems  = setTypes (makeRecordType typedesc) mergedelems
	# elem = RecS modname  name tsize typedelems
	= readUBListRecordElems pos (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc) (elems++[elem]) 
	= (elems,pos+IF_INT_64_OR_32 8 4)
	
	readUBArrayRecordElems 0 pos (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc)  elems 
	= (elems,pos)
	readUBArrayRecordElems size pos (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc)  elems
	# (ubels,pos) = readUDMany ubtypes nrub pos []
	# (bels,pos)  = readMany nrpointer pos []
	# mergedelems = merge_elems alltypes ubels bels
	# typedelems  = setTypes (makeRecordType typedesc) mergedelems
	# elem = RecS modname  name tsize typedelems
	= readUBArrayRecordElems (size-1) pos (name,modname,tsize,nrpointer,nrub,alltypes,ubtypes,typedesc) (elems++[elem]) 
	

	makeType 'i' pos  = IntS (sifs pos str)
	makeType 'c' pos  = CharS (scfs pos str)
	makeType 'b' pos  = BoolS (sbfs pos str)

	makeRealType pos  = RealS (srfs pos str)


//Start = testmrt "(1(2))34"
//testmrt str 
//# rt    = [toString c\\ c <-: str]
//# elems = [S c\\ c <- rt| c <> "(" && c <> ")" && c <> ","]
//= setTypes rt elems

// Setting the tuples and records correct in a record is quit compilcated
// Records can have type descriptors of the form a(a,a)a
// makeRecordType transforms this into: [[0],[2],[0],[0]]
// The 2 at the second position indicated that a tuple of size 2 starts at this position
// ((a),a)a is transformed in: [[2,1],[0],[0]] (nested tuple or records)
// setTypes applies this to the sequential list of elements
setTypes rtypes elems = mt rtypes elems
where
 mt [[ ]:ts] [elem:elems] = [elem : mt ts elems]
 mt [[0]:ts] [elem:elems] = [elem : mt ts elems]
 mt [[1]:ts] [elem:elems] = [TupleS 1 [elem] : mt ts elems]
 mt [[a:as]:ts] elems     = [TupleS (length rs) rs:mt (drop (a-1) ts) (drop a elems)] 
 where rs = mt [as:take (a-1) ts] (take a elems)
 mt [] [] = []
       

makeRecordType ltypes  = mrt [ltype\\ ltype <- ltypes| ltype <> ','] 
where
 mrt ['(':ltypes] = [first : mrt (tl rs)]
 where (first,rs) = dostartpars ['(':ltypes]
 mrt [')':ltypes] = mrt ltypes
 mrt [_  :ltypes] = [[0] : mrt ltypes]
 mrt []           = []
 dostartpars ['(':ltypes]  
 # f = gettuplength 1 0 ltypes
 # (fs,rs) = dostartpars ltypes
 = ([f:fs],rs)
 dostartpars rs = ([],rs)
 
gettuplength 1 length [')':rs] = length
gettuplength n length [')':rs] = gettuplength (n-1) length rs
gettuplength n length ['(':rs] = gettuplength (n+1) length rs
gettuplength n length [r:rs]   = gettuplength n  (length+1) rs

droplast [x] = []
droplast [x:xs] = [x:droplast xs]

makeSaplName :: String -> String
makeSaplName str 
# lstr           = [c\\ c <-: str]
# revl           = reverse lstr
# (dgs,revrest)  = span isDigit revl
# initname       = reverse (remsc revrest)
// FIXME: heuristic for this case: _f703;703;703 -> _f703_703
# initname       = if (dgs<>[]) (takeWhile (\r -> not (r == ';')) initname) initname
# initstr        = {c\\ c <- initname}
# fname          = makeName initstr
| dgs <> [] && hd revrest == ';'     
=  (fname +++ "_" +++ toString (reverse dgs))
| dgs <> []     
=  (fname +++ toString (reverse dgs))
=  fname

remsc [';':rs] = rs
remsc rs       = rs

makePrintableName f      | ss f                              = "<{" +++ f +++ "}>"
                                                             = f
where ss f = or [is_ss c\\ c <-: f]
      is_ss c = not (isAlphanum c || c == '_' || c == '.')              

makeRecName :: String String -> String
makeRecName mod name 
| last [c\\ c <-: ("" +++ name)] == ';' = mod +++ "." +++ name
                                        = mod +++ "._" +++ makeSaplName {a\\ a<-: name| a <> '[' && a <> ']' && a <> '#'}
    
makeName name | name.[0] == '\\' = "anon" 
              | startsWith "<lambda" name = "anon"
              | startsWith "c;" name = "_lc"
              | startsWith "g_" name = "_lc"
                                 = name 

startsWith :: String String -> Bool
startsWith s1 s2 = s1 == s2%(0,size s1-1)

print_graph :: !a -> String
print_graph g = des2string (string_to_int_array a,"[" +++ printlist [b\\ b <-:bs],"[" +++ printlist [c\\ c <-:cs])
where (a,bs,cs) = (graph_to_string_with_descriptor_and_module_table g)
des2string (ia,des,mods) = printintarray ia +++ "\n" +++ des +++ "\n" +++ mods +++ "\n" 

printintarray ia = "[" +++ printlist [toString a\\ a <-: ia]
printlist [] = "]"
printlist [a] =  a +++ "]"
printlist [a:as] =  a +++ ", " +++ printlist as
