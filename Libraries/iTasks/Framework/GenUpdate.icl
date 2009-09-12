implementation module GenUpdate

import StdString, StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, GenBimap
import Void, Either
import Text

defaultValue :: !*World -> (!a,!*World) | gUpdate{|*|} a
defaultValue world  
	# (a,ust=:{world}) = gUpdate{|*|} undef {USt|mode = UDCreate, searchPath = "", currentPath = [], consPath = [], update = "", mask = [], world = world}
	= (a,world)

updateValue	:: String String a !*World -> (a,!*World)	| gUpdate{|*|} a  
updateValue path update a world
	//Only try to update when the 'path' string is a datapath formatted string
	| isdps path	
		# (a,ust=:{world}) = gUpdate{|*|} a {USt| mode = UDSearch, searchPath = path, currentPath = [0], consPath = [], update = update, mask = [], world = world}
		= (a,world)
	| otherwise	
		= (a,world)


//Generic updater
generic gUpdate a :: a *USt ->  (a, *USt)

gUpdate{|UNIT|} _ ust=:{mode=UDCreate} = (UNIT, ust)
gUpdate{|UNIT|} u ust = (u, ust)

gUpdate{|PAIR|} fx fy p ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	# (ny,ust) = fy y ust
	= (PAIR nx ny, ust)
where
	(PAIR x y) = p
	
gUpdate{|PAIR|} fx fy _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	# (ny,ust) = fy undef ust
	= (PAIR nx ny, ust)
	
gUpdate{|PAIR|} fx fy p ust = (p, ust)

gUpdate{|EITHER|} fx fy e ust=:{mode=UDSearch}
	= case e of
		(LEFT x)	
			# (nx,ust) = fx x ust
			= (LEFT nx, ust)
		(RIGHT y)
			# (ny,ust) = fy y ust
			= (RIGHT ny,ust)
			
gUpdate{|EITHER|} fx fy _ ust=:{mode=UDCreate,consPath}
	= case consPath of
		[ConsLeft:cl]
			# (nx,ust) = fx undef {ust & consPath = cl}
			= (LEFT nx, ust)
		[ConsRight:cl]
			# (ny,ust) = fy undef {ust & consPath = cl}
			= (RIGHT ny, ust)
		[]
			# (nx,ust) = fx undef ust
			= (LEFT nx, ust)

gUpdate{|EITHER|} fx fy e ust = (e, ust)
	
gUpdate{|CONS|} fx c ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (CONS nx, ust)
where
	(CONS x) = c
	
gUpdate{|CONS|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (CONS nx, ust)

gUpdate{|CONS|} fx c ust = (c, ust)

gUpdate{|OBJECT of d|} fx o ust=:{mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		# (nx,ust) = fx undef {USt|ust & mode = UDCreate, consPath = path}
		= (OBJECT nx, {USt|ust & mode = UDDone})			 
	| otherwise
		# (nx,ust) = fx x {USt|ust & currentPath = shiftDataPath currentPath}
		= (OBJECT nx, {USt|ust & currentPath = stepDataPath currentPath})
where
	(OBJECT x) = o

	path = case [cons \\ cons <- d.gtd_conses | cons.gcd_name == update] of
		[cons]	= getConsPath cons
		_		= []
	
gUpdate{|OBJECT|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (OBJECT nx, ust)

gUpdate{|OBJECT|} fx o ust = (o, ust)

gUpdate{|FIELD|} fx f ust=:{mode=UDSearch}
	# (nx,ust) = fx x ust
	= (FIELD nx, ust)
where
	(FIELD x) = f
		
gUpdate{|FIELD|} fx _ ust=:{mode=UDCreate}
	# (nx,ust) = fx undef ust
	= (FIELD nx, ust)

gUpdate{|FIELD|} fx f ust = (f, ust)

gUpdate{|Int|} _ ust=:{USt|mode=UDCreate} = (0,ust)
gUpdate{|Int|} i ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (toInt update, {USt|ust & mode = UDDone})
	| otherwise
		= (i, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Int|} i ust = (i,ust)

gUpdate{|Real|} _ ust=:{USt|mode=UDCreate} = (0.0, ust)
gUpdate{|Real|} r ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (toReal update, {USt|ust & mode = UDDone})
	| otherwise
		= (r, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Real|} r ust = (r, ust)

gUpdate{|Char|} _ ust=:{USt|mode=UDCreate} = (' ', ust)
gUpdate{|Char|} c ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (if (size update > 0) update.[0] c, {USt|ust & mode = UDDone})
	| otherwise
		= (c, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Char|} c ust = (c, ust)

gUpdate{|Bool|} _ ust=:{USt|mode=UDCreate} = (False, ust)
gUpdate{|Bool|} b ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (update == "True", {USt|ust & mode = UDDone})
	| otherwise
		= (b, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|Bool|} b ust = (b, ust)

gUpdate{|String|} _ ust=:{USt|mode=UDCreate} = ("", ust)
gUpdate{|String|} s ust=:{USt|mode=UDSearch,searchPath,currentPath,update}
	| dp2s currentPath == searchPath
		= (update, {USt|ust & mode = UDDone})
	| otherwise
		= (s, {USt|ust & currentPath = stepDataPath currentPath})
gUpdate{|String|} s ust = (s, ust)


//Specialize instance for Dynamic
gUpdate{|Dynamic|} _ ust=:{USt|mode=UDCreate}	= (dynamic 42, ust)
gUpdate{|Dynamic|} d ust						= (d, ust)

//Specialized instances for [] and Maybe that choose the non-recursive constructor 

gUpdate{|[]|} fx _ ust=:{USt|mode=UDCreate} = ([], ust)
gUpdate{|[]|} fx l ust=:{USt|mode=UDSearch,currentPath}
	# (l,ust) = gUpdateList fx l ust
	= (l,{ust & currentPath = stepDataPath currentPath})
where
	gUpdateList fx [] ust=:{USt|currentPath,searchPath,update}
		| dp2s currentPath == searchPath && update == "_Cons"
			# (a,ust) = fx undef {ust& mode = UDCreate}
			= ([a],{USt|ust & mode = UDDone})	
		| otherwise	
			= ([],ust)
	gUpdateList fx [x:xs] ust=:{USt|currentPath,searchPath,update}
		|dp2s currentPath == searchPath
			| update == "_Nil"
				= ([],{USt|ust & mode = UDDone})
			| otherwise
				= ([x:xs],{USt|ust & mode = UDDone})
		| otherwise
			# (x,ust)	= fx x {ust & currentPath = shiftDataPath currentPath}
			# (xs,ust)	= gUpdateList fx xs {ust & currentPath = stepDataPath (shiftDataPath currentPath)}
			= ([x:xs],ust)
gUpdate{|[]|} fx l ust = (l,ust)

gUpdate{|Maybe|} fx _ ust=:{USt|mode=UDCreate} = (Nothing,ust)
gUpdate{|Maybe|} fx m ust=:{USt|currentPath,searchPath,update}
	| dp2s currentPath == searchPath && update == ""	
		= (Nothing, {USt|ust & mode = UDDone}) //Reset
	| otherwise
		= case m of
			Nothing
				# (x,ust) = fx undef {ust & mode = UDCreate} //Create an empty value to update
				# (x,ust=:{mode,currentPath}) = fx x {ust & mode = UDSearch,currentPath = currentPath, searchPath = searchPath,update = update}
				= case mode of
					UDDone	= (Just x,ust) //Only switch keep newly created value if a field was updated
					_		= (Nothing, ust)
			Just x
				# (x,ust) = fx x ust
				= (Just x,ust)

derive gUpdate Either, (,), (,,), (,,,), Void

//Utility functions
dp2s :: DataPath -> String
dp2s path = join "-" (map toString (reverse path))

dp2id :: String DataPath -> String
dp2id prefix path = prefix +++ "-" +++ dp2s path 

isdps :: String -> Bool
isdps path = and [c == '-' || isDigit c \\ c <-: path]

stepDataPath :: DataPath -> DataPath
stepDataPath []		= []
stepDataPath [x:xs]	= [inc x:xs]

shiftDataPath :: DataPath -> DataPath
shiftDataPath path	= [0:path]

dataPathLevel :: DataPath -> Int
dataPathLevel l	= length l
