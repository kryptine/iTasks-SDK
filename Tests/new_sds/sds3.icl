module sds3

import StdEnv
import Data.Void, Data.Tuple, Data.Error, Data.Func, Data.Either, Text.JSON, Data.List
import System.File, System.Time 

from Data.Map import :: Map
import qualified Data.Map as Map

println :: !a !*MyWorld -> *MyWorld | toString a
println msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites (toString msg) console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

getDescriptor :: !a -> VIEWID
getDescriptor _
	= code {
		pushD_a	0
		pop_a	1
	}

genID world=:{nextid}
	= (nextid, {world & nextid = nextid + 1})
	
// -----------------------------------------------------------------------

:: VIEWID :== Int

:: NRequest = // Notificication request
		{ target  :: VIEWID
		, origin  :: VIEWID
		, param   :: Dynamic /* p */
		, handler :: *MyWorld -> *MyWorld
		}

:: *MyWorld = 
		{ sdsmem		:: Map Int JSONNode
        , sdsstore      :: Map String JSONNode
		, notification	:: [NRequest]
		, nextid		:: Int
		, world			:: *World
		}

createMyWorld :: *World -> *MyWorld
createMyWorld world = {MyWorld | sdsmem = 'Map'.newMap, sdsstore = 'Map'.newMap, notification = [], nextid = 1, world = world}

// -----------------------------------------------------------------------

:: IFun p :== p -> Bool

:: Source p r w *env =
	{ get	:: p   env -> *(MaybeErrorString r, env)
	, put	:: p w env -> *(MaybeErrorString (IFun p), env)
	}

:: PView p` r` w` *env
	= 		 	Source 		(Source p` r` w` env)
	| E.r w:	Projection 	(PView  p` r  w  env) (Lens r w r` w`)
	| E.p:		Translation (PView  p  r` w` env) (p` -> p) 							& TC p
	| E.p1 p2:	Split 		(PView  p1 r` w` env) (Split p2 r` w`) (p` -> (p1,p2)) 		& TC p1 & TC p2
	| E.p1 r1 w1 p2 r2 w2:
				Join		(PView  p1 r1 w1 env) (PView p2 r2 w2 env) (p` -> (p1,p2)) (w` -> (w1,w2)) (r1 r2 -> r`) & TC p1 & TC p2
	| E.p1 p2:
				Union		(PView  p1 r` w` env) (PView p2 r` w` env) (p` -> Either p1 p2) (p1 r` w` -> IFun p2) (p2 r` w` -> IFun p1) & TC p1 & TC p2
	
:: View r w *env :== PView Void r w env

:: Lens r w r` w` =
	{ lget	:: r -> r`
	, lput	:: r w` -> w
	}

:: Split p r w = 
	{ sget	:: p r -> r
	, sput	:: p r w -> (w, IFun p)
	}

applyLens :: (PView p a a *env) (Lens a a r` w`) -> PView p r` w` *env
applyLens pview lens = Projection pview lens

fixP :: (PView p r w *env) p -> View r w *env | TC p
fixP pview p = Translation pview (const p)

applySplit :: (PView p1 r w *env) (Split p2 r w) (p->(p1,p2)) -> (PView p r w *env) | TC p1 & TC p2
applySplit pview split tr = Split pview split tr

applyTransformation :: (PView p1 r w *env) (p2 -> p1) -> (PView p2 r w *env) | TC p1 & TC p2
applyTransformation pview tr = Translation pview tr

join :: (PView p1 r1 w1 *env) (PView p2 r2 w2 *env) -> (PView (p1,p2) (r1,r2) (w1,w2) *env) | TC p1 & TC p2
join pview1 pview2 = Join pview1 pview2 id id tuple

union :: (PView p1 r w *env) (PView p2 r w *env) (p1 r w -> IFun p2) (p2 r w -> IFun p1) -> (PView (Either p1 p2) r w *env) | TC p1 & TC p2
union pview1 pview2 ifun1 ifun2 = Union pview1 pview2 id ifun1 ifun2

// These can be encoded as a type class if we have functional dependencies...
tr1 a = (Void, a)
tr2 (a,b) = (a,b)
tr3 (a,b,c) = ((a,b), c)
tr4 (a,b,c,d) = (((a,b), c),d)

// -----------------------------------------------------------------------

get :: (View r w *env) *env -> *(MaybeErrorString r, *env)
get pview env = get` pview Void env

// TODO: return "read cache"
get` :: (PView p r w *env) p *env -> *(MaybeErrorString r, *env)
get` (Source {get}) p env 
	= get p env
	
get` (Projection pview` {lget}) p env
	# (ret, env) = get` pview` p env
	= (fmap lget ret, env)

get` (Translation pview` tr) p env
	= get` pview` (tr p) env

get` (Split pview` {sget} tr) p env
	# (p1, p2) = tr p
	# (ret, env) = get` pview` p1 env
	= (fmap (sget p2) ret, env)

get` (Join pview1 pview2 trp trw trr) p env
	# (p1, p2) = trp p
	# (ret1, env) = get` pview1 p1 env
	| isError ret1
		= (Error (fromError ret1), env)
	# (ret2, env) = get` pview2 p2 env
	| isError ret2
		= (Error (fromError ret2), env)
		= (Ok (trr (fromOk ret1) (fromOk ret2)), env)

get` (Union pview1 pview2 trp _ _) p env
	= case trp p of
		Left p1  = get` pview1 p1 env
		Right p2 = get` pview2 p2 env

put :: (View r w *MyWorld) w *MyWorld -> *(MaybeErrorString Void, *MyWorld)
put pview w env 
	= case put` pview Void w env of
		(Error msg, _, myworld) = (Error msg, myworld)
		(Ok _, ns, myworld)
			# myworld = notificateAll (getDescriptor pview) ns myworld
			= (Ok Void, myworld)
			
:: NEvent = E.p: NEvent VIEWID (p -> Bool) & TC p // Notification event

put` :: (PView p r w *env) p w *env -> (MaybeErrorString (IFun p), [NEvent], *env) | TC p
put` d=:(Source pview) p wval myworld
	= case pview.put p wval myworld of
		(Error msg, myworld) = (Error msg, [], myworld)
		(Ok ifun, myworld) 	 = (Ok ifun, [NEvent (getDescriptor d) ifun], myworld)

put` d=:(Projection pview {lput}) p wval env
	= case get` pview p env of 
		(Error msg, env) = (Error msg, [], env)
		(Ok rval, env)   = case put` pview p (lput rval wval) env of	
									(Error msg, _, env)  = (Error msg, [], env)
									(Ok ifun, ns, env) 	 = (Ok ifun, [NEvent (getDescriptor d) ifun:ns], env)

put` d=:(Translation pview tr) p wval env
	= case put` pview (tr p) wval env of
		(Error msg, _, env) = (Error msg, [], env)
		(Ok ifun, ns, env) 	# ifun` = \p -> ifun (tr p)
							= (Ok ifun`, [NEvent (getDescriptor d) ifun`:ns], env)

put` d=:(Split pview {sput} trp) p wval env 
	= case get` pview p1 env of 
		(Error msg, env) = (Error msg, [], env)
		(Ok rval, env)   # (wval, ifun2) = sput p2 rval wval
						 = case put` pview p1 wval env of	
								(Error msg, _, env) = (Error msg, [], env)
								(Ok ifun1, ns, env) # ifun` = genifun ifun1 ifun2
													= (Ok ifun`, [NEvent (getDescriptor d) ifun`], env)
where
	genifun _ ifun2 p = let (p1,p2) = trp p in ifun2 p2 // TODO: notification when actually???
	(p1, p2) = trp p

put` d=:(Join pview1 pview2 trp trw _) p wval env 
	= case put` pview1 p1 w1 env of
		(Error msg, _, env)  = (Error msg, [], env)
		(Ok ifun1, ns1, env) = case put` pview2 p2 w2 env of
									(Error msg, _, env)  = (Error msg, [], env)
									(Ok ifun2, ns2, env) # ifun` = genifun ifun1 ifun2
														 = (Ok ifun`, [NEvent (getDescriptor d) ifun`:ns1++ns2], env)
where
	genifun ifun1 ifun2 p = let (p1,p2) = trp p in ifun1 p1 || ifun2 p2
	(p1, p2) = trp p
	(w1, w2) = trw wval

put` d=:(Union pview1 pview2 trp ifunl ifunr) p wval env
	= case trp p of
	
		Left p1 = case get` pview1 p1 env of
					(Error msg, env) = (Error msg, [], env)
					(Ok rval, env)   = case put` pview1 p1 wval env of
											(Error msg, _, env) = (Error msg, [], env)
											(Ok ifun, ns, env)  # ifun` = genifun (ifunl p1 rval wval) ifun 
 														   		= (Ok ifun`, [NEvent (getDescriptor d) ifun`], env)

		Right p2 = case get` pview2 p2 env of
					(Error msg, env) = (Error msg, [], env)
					(Ok rval, env)   = case put` pview2 p2 wval env of
											(Error msg, _, env) = (Error msg, [], env)
											(Ok ifun, ns, env) # ifun` = genifun ifun (ifunr p2 rval wval)
 															   = (Ok ifun`, [NEvent (getDescriptor d) ifun`], env) 															   
where
	genifun ifun1 ifun2 p = case trp p of
								(Left p1)  = ifun2 p1
								(Right p2) = ifun1 p2

notificateAll :: VIEWID [NEvent] *MyWorld -> *MyWorld
notificateAll eventsource ns myworld = foldl notificate myworld ns
where
	notificate myworld=:{MyWorld|notification} (NEvent sdsid invalidator)  
			= foldl geninv myworld notification
	where
		// Skip if ...
		geninv myworld {target, origin} | target <> sdsid || (target <> origin && origin == eventsource)
			= myworld
	
		geninv myworld {target, handler, param} | target == sdsid
			= case param of
				(qr :: qr^) = case invalidator qr of
										False = myworld
										True  = handler myworld
							= myworld	
			
		geninv myworld _ = myworld

class registerForNotification env :: (PView p r w *env) p String *env -> *env | TC p

instance registerForNotification MyWorld
where
	registerForNotification pview p msg myworld=:{MyWorld|notification} 
		= {MyWorld | myworld & notification = [createRequest origin (dynamic p):lowerLayers++notification]}
	where
		origin = getDescriptor pview
		createRequest target param = 	{ target = target 
										, origin = origin 
										, param = param
										, handler = println ("Notification: "+++msg)
										} 
	
		lowerLayers = [createRequest viewid dynparam \\ (viewid, dynparam) <- collectIds pview p]
		
		collectIds :: (PView p r w *MyWorld) p -> [(VIEWID, Dynamic)] | TC p
		collectIds d=:(Source _) p = []//[(getDescriptor d, dynamic p)]
		collectIds d=:(Projection pview _) p = collectIds pview p
		collectIds d=:(Translation pview tr) p = collectIds pview (tr p)
		collectIds d=:(Split pview split trp) p = [(getDescriptor d, dynamic p)]//:collectIds pview (fst (trp p))]
		collectIds d=:(Join pview1 pview2 trp _ _) p 
			= let (p1, p2) = trp p in [(getDescriptor d, dynamic p):collectIds pview1 p1 ++ collectIds pview2 p2]
		collectIds d=:(Union pview1 pview2 trp _ _) p 
			= case trp p of
				Left p1 = [(getDescriptor d, dynamic p)]//:collectIds pview1 p1]
				Right p2 = [(getDescriptor d, dynamic p)]//:collectIds pview2 p2]

// -----------------------------------------------------------------------

createMemoryView :: a *MyWorld -> *(PView Void a a MyWorld, *MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a
createMemoryView val myworld=:{sdsmem}
	# (memid, myworld) = genID myworld
	# sds = Source {Source | get  = get` memid, put = put` memid}
	= (sds, {myworld & sdsmem = 'Map'.put memid (toJSON val) sdsmem})
where
	get` memid Void myworld=:{sdsmem}
		= case 'Map'.get memid sdsmem of
			(Just json) = case fromJSON json of
								(Just val) = (Ok val, myworld)
								Nothing	   = (Error "E2", myworld)
			Nothing 	= (Error "E1", myworld)

	put` memid Void val myworld=:{sdsmem}
		= (Ok (const True), {myworld & sdsmem = 'Map'.put memid (toJSON val) sdsmem})


createStoreView :: String a -> (PView Void a a MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a
createStoreView name def = Source {Source | get = get` name def, put = put` name}
where
    get` name def Void myworld=:{sdsstore}
        = case 'Map'.get name sdsstore of
			(Just json) = case fromJSON json of
								(Just val) = (Ok val, myworld)
								Nothing	   = (Error "E2", myworld)
			Nothing 	= (Ok def, myworld)

    put` name Void val myworld=:{sdsstore}
        = (Ok (const True), {myworld & sdsstore = 'Map'.put name (toJSON val) sdsstore})

/*
// -----------------------------------------------------------------------

instance fromString Int
where
	fromString s = toInt s

stringListLens :: Lens [a] [a] [String] [String] | toString a & fromString a
stringListLens =
	{ lget = map toString
	, lput = const (map fromString)
	}

:: ListSplit = Head | Tail 

listSplit :: Split ListSplit [a] [a]
listSplit = { sget = sget`, sput = sput``}
where
	sget` Head [a:as] = [a]
	sget` Tail [a:as] = as
	sget` _    []     = []

	sput`` a b c = (sput` a b c, const True)

	sput` Head [a:as] [b] = [b:as]
	sput` Tail [a:as] bs  = [a:bs]
	sput` _    as     _   = as

:: LT = TailTail

// -----------------------------------------------------------------------

Start world 
	# myworld = createMyWorld world
	
	# (listview, myworld) = createMemoryView [1,2,3] myworld
	# listview1 = applyLens listview stringListLens
	# (val, myworld) = get listview1 myworld
	# (_, myworld) = put listview1 ["4","5","6"] myworld	
	
	# listview2 = applySplit listview1 listSplit tr1
	# (val, myworld) = get (fixP listview2 Head) myworld
	# listview3 = applySplit listview2 listSplit tr2
	# (val, myworld) = get (fixP listview3 (Tail, Tail)) myworld

	# listview4 = applyTransformation listview3 (const (Tail,Tail))
	# (val, myworld) = get (fixP listview4 TailTail) myworld
	
	# listview5 = join listview1 listview2
	# (val, myworld) = get (fixP listview5 (Void,Head)) myworld			
	
	= (val, myworld.world)	
*/	

:: TaskInstance =
    { instanceId    :: Int
    , instanceType  :: TaskInstanceType
    , instanceTags  :: [String]          //For selecting stuff
    , instanceState :: String            //Normally this is what you update when you process task events
    }

:: TaskInstanceType = SessionTask | PersistentTask

:: TaskInstanceFilter =
    { filterById    :: Maybe Int
    , filterByType  :: Maybe TaskInstanceType
    , filterByTag   :: Maybe String
    }

emptyFilter :: TaskInstanceFilter
emptyFilter = {filterById = Nothing,filterByType=Nothing,filterByTag=Nothing}

instance == TaskInstanceType
where
	(==) SessionTask SessionTask = True
	(==) PersistentTask PersistentTask = True
	(==) _ _ = False

derive JSONEncode TaskInstance, TaskInstanceType
derive JSONDecode TaskInstance, TaskInstanceType

instanceTableData :: [TaskInstance]
instanceTableData =
			[{instanceId=1,instanceType=SessionTask,instanceTags = ["old"],instanceState = "Hansje"}
            ,{instanceId=2,instanceType=SessionTask,instanceTags = [],instanceState = "Pansje"}
            ,{instanceId=3,instanceType=SessionTask,instanceTags = [],instanceState = "Kevertje"}
            ,{instanceId=4,instanceType=PersistentTask,instanceTags = ["work","personal"],instanceState = "Die"}
            ,{instanceId=5,instanceType=SessionTask,instanceTags = [],instanceState = "Zat"}
            ,{instanceId=6,instanceType=PersistentTask,instanceTags = ["important","work"],instanceState = "Eens"}
            ,{instanceId=7,instanceType=PersistentTask,instanceTags = ["work"],instanceState = "Op"}
            ,{instanceId=8,instanceType=SessionTask,instanceTags = [],instanceState = "Een"}
            ,{instanceId=9,instanceType=SessionTask,instanceTags = [],instanceState = "Hek"}
            ,{instanceId=10,instanceType=PersistentTask,instanceTags = [],instanceState = "Toen"}
            ,{instanceId=11,instanceType=PersistentTask,instanceTags = ["important"],instanceState = "Kwam"}
            ,{instanceId=12,instanceType=SessionTask,instanceTags = [],instanceState = "De"}
            ,{instanceId=13,instanceType=SessionTask,instanceTags = [],instanceState = "Regen"}
            ]

instanceTable :: PView Void [TaskInstance] [TaskInstance] MyWorld
instanceTable = createStoreView "instanceTable" instanceTableData

filteredInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
filteredInstances = applySplit instanceTable {sget = sget`, sput = sput`} tr1
where
    sget` tfilter is = filter (filterFun tfilter) is
    sput` tfilter is ws
        = let (ds,us) = splitWith (filterFun tfilter) is
          in (us ++ ws, notifyFun (ds ++ ws))

    filterFun {filterById,filterByType,filterByTag}
        = \i -> (maybe True (\m -> i.instanceId == m) filterById)
            &&  (maybe True (\m -> i.instanceType == m) filterByType)
            &&  (maybe True (\m -> isMember m i.instanceTags) filterByTag)

    notifyFun ws {filterById,filterByType,filterByTag}
        =   (maybe True (\m -> isMember m (writeIds ws)) filterById)
        ||  (maybe True (\m -> isMember m (writeTypes ws)) filterByType)
        ||  (maybe True (\m -> isMember m (writeTags ws)) filterByTag)

    writeIds ws   = removeDup (map (\i. i.instanceId) ws)
    writeTypes ws = removeDup (map (\i. i.instanceType) ws)
    writeTags ws  = removeDup (flatten (map (\i. i.instanceTags) ws))

instancesOfType :: PView (TaskInstanceType,TaskInstanceFilter) [TaskInstance] [TaskInstance] MyWorld
instancesOfType = applyTransformation filteredInstances (\(t,f) -> {f & filterByType = Just t})

persistentInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
persistentInstances = applyTransformation instancesOfType (\f -> (PersistentTask,f))

sessionInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
sessionInstances = applyTransformation instancesOfType (\f -> (SessionTask,f))

persistentWithTag :: PView (String,TaskInstanceFilter) [TaskInstance] [TaskInstance] MyWorld
persistentWithTag = applyTransformation persistentInstances (\(t,f) -> {f & filterByTag = Just t})

importantInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
importantInstances = applyTransformation persistentWithTag (\f -> ("important",f))

instanceById :: PView Int TaskInstance TaskInstance MyWorld
instanceById = applyLens (applyTransformation filteredInstances (\i -> {emptyFilter & filterById = Just i})) singletonLens

singletonLens :: Lens [a] [a] a a
singletonLens = {lget = \[x:_] -> x, lput = \_ x -> [x]}

instancesOfTypeSplit = {sget = sget`, sput = sput`}
where
	sget` type is = filter (\{instanceType} -> instanceType == type) is
	sput` type is ws = (filter (\{instanceType} -> instanceType <> type) is ++ ws, (==) type)

tagSplit = {sget = sget`, sput = sput`}
where
	sget` tag is = [i \\ i <- is | isMember tag i.instanceTags]
	sput` tag is ws = (kept ++ ws, \tag = isMember tag affectedtags)
	where
		kept = [i \\ i <- is | not (isMember tag i.instanceTags)]
		replaced = [i \\ i <- is | isMember tag i.instanceTags]
		affectedtags = removeDup (flatten (map (\{instanceTags} -> instanceTags) (replaced ++ ws)))

instanceIdSplit = {sget = sget`, sput = sput`}
where
	sget` iid is = filter (\{instanceId} -> instanceId == iid) is
	sput` iid is ws = (filter (\{instanceId} -> instanceId <> iid) is ++ ws, (==) iid)

idtagifun _ os is tag = any (\{instanceTags} -> isMember tag instanceTags) (is++os)
tagidifun _ os is id = any (\{instanceId} -> id == instanceId) (is++os)

Start world 
	# myworld = createMyWorld world
	
	//# (instanceTable, myworld) 	= createMemoryView instanceTableData myworld

/*
	# instancesOfType 			= applySplit instanceTable instancesOfTypeSplit tr1
	# sessionInstances 			= fixP instancesOfType SessionTask
	# persistentInstances 		= fixP instancesOfType PersistentTask

	# persistentWithTag			= applySplit persistentInstances tagSplit
	# (val, myworld) = get (fixP instancesOfType PersistentTask) myworld
*/

	# instanceOfId 			= applySplit instanceTable instanceIdSplit tr1
	# instancesOfTag 		= applySplit instanceTable tagSplit tr1	
	# instancesOfIdTag 		= union instanceOfId instancesOfTag idtagifun tagidifun

	# myworld = registerForNotification instancesOfIdTag (Left 1) "Id 1" myworld
	# myworld = registerForNotification instancesOfIdTag (Right "new") "Tag 'new'" myworld	
	# myworld = registerForNotification instancesOfIdTag (Right "old") "Tag 'old'" myworld	

	# (_, myworld) = put (fixP instancesOfIdTag (Left 1)) [{instanceId=1,instanceType=SessionTask,instanceTags = ["new"],instanceState = "Hansje2"}] myworld
	

	# (val, myworld) = get instanceTable myworld
	//# (val, myworld) = get (fixP instancesOfIdTag (Left 1)) myworld

	= (val, myworld.world)
	
	
	
	
	
	
	
