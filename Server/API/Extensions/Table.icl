implementation module Table

import StdTuple, StdList, StdMisc, StdBool, StdFunc, Text, Types

derive JSONEncode	Table, TableCol
derive JSONDecode	Table, TableCol
derive bimap		Maybe, (,), TableCol

gUpdate{|Table|} _ _ ust=:{USt|mode=UDCreate,newMask} 
	= (Table [] [], {USt | ust & newMask = appendToMask newMask Untouched})
	
gUpdate{|Table|} _ table=:(Table cols data) ust=:{USt|mode=UDSearch,searchPath,currentPath,update,oldMask,newMask}
	# (cm,om)		= popMask oldMask
	# ust			= {ust & currentPath = stepDataPath currentPath, oldMask = om}
	| searchPath <== currentPath
		# searchDPList	= dataPathList searchPath
		# curDPList		= dataPathList currentPath
		# inTableDP		= take (length searchDPList - length curDPList) searchDPList
		| length inTableDP >= 2
			# [row:l]	= reverse inTableDP
			# dp		= (dataPathFromList (reverse l))
			| length data > row
				= (Table cols (updateAt row (updObj update dp (data !! row)) data),{ust & newMask = appendToMask newMask (Touched True [])})
			| otherwise
				= undef
		| otherwise
			= undef
	| otherwise
		= (table,{ust & newMask = appendToMask newMask (cleanUpdMask cm)})
where
	updObj v dp (JSONObject obj) = JSONObject [(dp2s dp,JSONString v):filter (\(idx,_) -> idx <> dp2s dp) obj]
	
gUpdate{|Table|} _ table ust=:{USt|mode=UDMask,currentPath,newMask}
	# mask = Touched True []
	= (table,{USt|ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask mask})

gVerify{|Table|} _ v vst = customVerify Nothing (const True) (const "") v vst

gVisualize{|Table|} _ val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
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
													| name = dp2s currentPath
													, id = id
													, columns = map toTUICols columns
													, gridData = data
													})]
					, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= abort "not implemented yet!"
where
	toTUICols col =	{ header	= col.TableCol.header
					, dataIndex	= col.TableCol.dataIndex
					, editor	= col.editorType
					}

toTable :: ![a] -> Table a | tableRow a
toTable l = Table cols data
where
	cols = fst (gMakeColumns{|*|} [] Nothing startDataPath)
	data = map (\row -> toObj (gToRow{|*|} row startDataPath)) l
	where
		toObj = JSONObject o map (app2 (id,JSONString)) o fst
		
fromTable :: !(Table a) -> [a] | tableRow a
fromTable (Table _ rows) = map (\row -> fst (gFromRow{|*|} (fromObj row) startDataPath)) rows
where
	fromObj = fromList o map (app2 (id,\(JSONString str) -> str)) o (\(JSONObject o) -> o)

generic gMakeColumns a :: ![String] !(Maybe Int) !DataPath -> (![TableCol a],!DataPath)

gMakeColumns{|OBJECT|} fx labels mbIdx dp
	# (cols,_) = fx (maybeToList (mapMaybe toString mbIdx) ++ labels) Nothing (shiftDataPath dp)
	= (changeTColsType cols,stepDataPath dp)

gMakeColumns{|CONS of d|} fx labels mbIdx dp
	# (cols,dp) = fx nLabels nMbIdx dp
	= (changeTColsType cols,dp)
where
	// for records labels are determined by FIELDs
	nLabels		= if notRecord [d.gcd_name:labels] labels
	nMbIdx		= if (notRecord && d.gcd_arity > 1) (Just 1) Nothing
	notRecord	= isEmpty d.gcd_fields

gMakeColumns{|FIELD of d|} fx labels mbIdx dp
	# (cols,dp) = fx [d.gfd_name:labels] mbIdx dp
	= (changeTColsType cols,dp)

gMakeColumns{|PAIR|} fx fy labels mbIdx dp
	# (colsx,dp) = fx labels mbIdx dp
	# (colsy,dp) = fy labels (mapMaybe inc mbIdx) dp
	= (changeTColsType colsx ++ changeTColsType colsy,dp)
		
gMakeColumns{|Int|}			labels mbIdx dp = basicMakeColumn (Just "itasks.tui.Int")		labels mbIdx dp			
gMakeColumns{|String|}		labels mbIdx dp = basicMakeColumn (Just "itasks.tui.String")	labels mbIdx dp						
gMakeColumns{|Bool|}		labels mbIdx dp = basicMakeColumn (Just "itasks.tui.Bool")		labels mbIdx dp
gMakeColumns{|Real|}		labels mbIdx dp = basicMakeColumn (Just "itasks.tui.Real")		labels mbIdx dp
gMakeColumns{|Date|}		labels mbIdx dp = basicMakeColumn (Just "itasks.tui.Date")		labels mbIdx dp
gMakeColumns{|Note|}		labels mbIdx dp = basicMakeColumn (Just "itasks.tui.Note")		labels mbIdx dp

basicMakeColumn mbEditorType labels mbIdx dp = ([col],stepDataPath dp)
where
	col =	{ header = join " " (map formatLabel (reverse labels ++ maybeToList (mapMaybe toString mbIdx)))
			, dataIndex = dp2s dp
			, editorType = mbEditorType
			}
			
changeTColsType :: ![TableCol a] -> [TableCol b]
changeTColsType cols = map (\{header,dataIndex,editorType} -> {header = header, dataIndex = dataIndex, editorType = editorType}) cols

generic gToRow a :: !a !DataPath -> (![(!String,!String)],!DataPath)

gToRow{|OBJECT|} fx (OBJECT o) dp
	# (res,_) = fx o (shiftDataPath dp)
	= (res,stepDataPath dp)
	
gToRow{|CONS|} fx (CONS c) dp
	= fx c dp
	
gToRow{|FIELD|} fx (FIELD f) dp
	= fx f dp
	
gToRow{|PAIR|} fx fy (PAIR x y) dp
	# (colsx,dp) = fx x dp
	# (colsy,dp) = fy y dp
	= (colsx ++ colsy,dp)
	
gToRow{|Int|}		v dp = basicToRow v dp
gToRow{|String|}	v dp = basicToRow v dp
gToRow{|Bool|}		v dp = basicToRow v dp
gToRow{|Real|}		v dp = basicToRow v dp
gToRow{|Date|}		v dp = basicToRow v dp
gToRow{|Note|}		v dp = basicToRow v dp

basicToRow v dp = ([(dp2s dp,toString v)],stepDataPath dp)

generic gFromRow a :: !(Map String String) !DataPath -> (!a,!DataPath)

gFromRow{|OBJECT|} fx data dp
	# (res,_) = fx data (shiftDataPath dp)
	= (OBJECT res,stepDataPath dp)
	
gFromRow{|CONS|} fx data dp
	= app2 (CONS,id) (fx data dp)
	
gFromRow{|FIELD|} fx data dp
	= app2 (FIELD,id) (fx data dp)
	
gFromRow{|PAIR|} fx fy data dp
	# (resx,dp) = fx data dp
	# (resy,dp) = fy data dp
	= (PAIR resx resy,dp)
	
gFromRow{|Int|}			data dp = basicFromRow toInt						data dp
gFromRow{|String|}		data dp = basicFromRow id							data dp
gFromRow{|Bool|}		data dp = basicFromRow ((==) "true" o toLowerCase)	data dp
gFromRow{|Real|}		data dp = basicFromRow toReal						data dp
gFromRow{|Date|}		data dp = basicFromRow fromString					data dp
gFromRow{|Note|}		data dp = basicFromRow Note							data dp

basicFromRow :: (String -> a) (Map String String) DataPath -> (a,DataPath)
basicFromRow str2V data dp = case get (dp2s dp) data of
	Just str	= (str2V str,stepDataPath dp)
	Nothing		= abort ("basicFromRow: can't find datapath " +++ (dp2s dp))
