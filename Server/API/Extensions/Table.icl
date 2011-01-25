implementation module Table

import StdTuple, StdList, StdMisc, StdBool, StdFunc, Text, Types

derive JSONEncode	Table, TUIGridColumn
derive JSONDecode	Table, TUIGridColumn
derive bimap Maybe, (,)

gUpdate{|Table|} _ ust=:{USt|mode=UDCreate,newMask} 
	= (Table [] [], {USt | ust & newMask = appendToMask newMask Untouched})
gUpdate{|Table|} table ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om)	= popMask oldMask
	# ust		= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| currentPath == searchPath
		= (table,ust) 
	| otherwise
		= (table,{ust & newMask = appendToMask newMask (cleanUpdMask cm)})
gUpdate{|Table|} table ust=:{USt|mode=UDMask,currentPath,newMask}
	# mask = Touched True []
	= (table,{USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask mask})

gVerify{|Table|} v vst = customVerify Nothing (const True) (const "") v vst

gVisualize{|Table|} val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & updateMask = um, verifyMask = vm}
	= case val of
		Nothing
			= ([TextFragment "Empty table"],{VSt|vst & currentPath = stepDataPath currentPath})
		Just (Table columns data)
			= case vizType of
				VEditorDefinition
					# (err,hnt)	= verifyElementStr cmu cmv
					# id		= dp2id idPrefix currentPath
					= ([TUIFragment (TUIGridControl	{ TUIGridControl
													| name = id 
													, id = id
													, columns = columns
													, gridData = data
													})]
					, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= abort "not implemented yet!"

toTable :: ![a] -> Table | tableRow a
toTable [] = Table [] []
toTable l=:[v:_] = Table cols data
where
	cols = fst (gMakeColumns{|*|} v [] Nothing startDataPath)
	data = map (\row -> mkObj (gMakeRow{|*|} row startDataPath)) l
	where
		mkObj = JSONObject o map (app2 (id,JSONString)) o fst

generic gMakeColumns a :: !a ![String] !(Maybe Int) !DataPath -> (![TUIGridColumn],!DataPath)

gMakeColumns{|OBJECT|} fx (OBJECT o) labels mbIdx dp
	# (res,_) = fx o (maybeToList (mapMaybe toString mbIdx) ++ labels) Nothing (shiftDataPath dp)
	= (res,stepDataPath dp)

gMakeColumns{|CONS of d|} fx (CONS c) labels mbIdx dp
	= fx c nLabels nMbIdx dp
where
	// for records labels are determined by FIELDs
	nLabels		= if notRecord [d.gcd_name:labels] labels
	nMbIdx		= if (notRecord && d.gcd_arity > 1) (Just 1) Nothing
	notRecord	= isEmpty d.gcd_fields

gMakeColumns{|FIELD of d|} fx (FIELD f) labels mbIdx dp
	= fx f [d.gfd_name:labels] mbIdx dp

gMakeColumns{|PAIR|} fx fy (PAIR x y) labels mbIdx dp
	# (colsx,dp) = fx x labels mbIdx dp
	# (colsy,dp) = fy y labels (mapMaybe inc mbIdx) dp
	= (colsx ++ colsy,dp)
		
gMakeColumns{|Int|}			v labels mbIdx dp = basicMakeColumn labels mbIdx dp
gMakeColumns{|String|}		v labels mbIdx dp = basicMakeColumn labels mbIdx dp
gMakeColumns{|Bool|}		v labels mbIdx dp = basicMakeColumn labels mbIdx dp
gMakeColumns{|Real|}		v labels mbIdx dp = basicMakeColumn labels mbIdx dp
gMakeColumns{|Date|}		v labels mbIdx dp = basicMakeColumn labels mbIdx dp
gMakeColumns{|Currency|}	v labels mbIdx dp = basicMakeColumn labels mbIdx dp

basicMakeColumn labels mbIdx dp = ([col],stepDataPath dp)
where
	col =	{ header = join " " (map formatLabel (reverse labels ++ maybeToList (mapMaybe toString mbIdx)))
			, dataIndex = dp2s dp
			}

generic gMakeRow a :: !a !DataPath -> (![(!String,!String)],!DataPath)

gMakeRow{|OBJECT|} fx (OBJECT o) dp
	# (res,_) = fx o (shiftDataPath dp)
	= (res,stepDataPath dp)
	
gMakeRow{|CONS|} fx (CONS c) dp
	= fx c dp
	
gMakeRow{|FIELD|} fx (FIELD f) dp
	= fx f dp
	
gMakeRow{|PAIR|} fx fy (PAIR x y) dp
	# (colsx,dp) = fx x dp
	# (colsy,dp) = fy y dp
	= (colsx ++ colsy,dp)
	
gMakeRow{|Int|}			v dp = basicMakeRow v dp
gMakeRow{|String|}		v dp = basicMakeRow v dp
gMakeRow{|Bool|}		v dp = basicMakeRow v dp
gMakeRow{|Real|}		v dp = basicMakeRow v dp
gMakeRow{|Date|}		v dp = basicMakeRow v dp
gMakeRow{|Currency|}	v dp = basicMakeRow v dp

basicMakeRow v dp = ([(dp2s dp,toString v)],stepDataPath dp)
