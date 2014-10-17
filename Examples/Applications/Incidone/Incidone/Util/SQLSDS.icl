implementation module Incidone.Util.SQLSDS
import iTasks
import iTasks.API.Extensions.SQLDatabase, Database.SQL, Incidone.OP.Concepts
import Text
import qualified Data.Map as DM
import StdMisc

derive class iTask QueryDef, RowFilterDef, ColumnSourceDef, TableDef, RowOrderDef

toReadSQL :: QueryDef -> (String,[SQLValue])
toReadSQL {QueryDef|columns,rows,order}
    = (qselect +++ qfrom +++ qwhere +++ qorderby, qvals)
where
    qselect         = toSelectSQL columns
    qfrom           = toFromSQL columns
    (qwhere,qvals)  = maybe ("",[]) toWhereSQL rows
    qorderby        = maybe "" toOrderBySQL order

toWriteSQL :: QueryDef -> (String,[SQLValue])
toWriteSQL {QueryDef|columns,rows}
    = (qinsert +++ qupdatedup, [v \\ (c,v) <- fixed | not (ignore c)])
where
    (qinsert,qcols) = toInsertSQL (map fst fixed) columns
    qupdatedup      = toUpdateOnDupKeySQL (map fst fixed) columns
    fixed           = maybe [] collectFixedColumns rows
    ignore  c       = isMember c qcols

toWriteNewSQL :: QueryDef -> (String,[SQLValue])
toWriteNewSQL {QueryDef|columns,rows}
    = (qinsert, [v \\ (c,v) <- fixed | not (ignore c)])
where
    (qinsert,qcols) = toInsertSQL (map fst fixed) columns
    fixed           = maybe [] collectFixedColumns rows
    ignore  c       = isMember c qcols

toWriteExistingSQL :: QueryDef -> (String,[SQLValue])
toWriteExistingSQL {QueryDef|columns,rows}
    = (qupdate +++ qwhere, [v \\ (c,v) <- fixed | not (ignore c)])
where
    (qupdate,qcols) = toUpdateSQL (map fst fixed) columns
    (qwhere,qvals)  = maybe ("",[]) toWhereSQL rows
    fixed           = maybe [] collectFixedColumns rows
    ignore  c       = isMember c qcols

toEraseSQL :: QueryDef -> (String,[SQLValue])
toEraseSQL {QueryDef|columns,rows}
    = (qdelete +++ qwhere,qvals)
where
    qdelete         = toDeleteSQL columns
    (qwhere,qvals)  = maybe ("",[]) toWhereSQL rows

toInsertSQL :: [ColumnDef] ColumnSourceDef -> (String,[ColumnDef])
toInsertSQL fixed (BaseTable {name,alias,columns})
    = ("INSERT INTO " +++ name +++ " ("+++join "," (map snd tableColumns) +++ ") VALUES ("
    +++ join "," (repeatn (length tableColumns) "?") +++ ")",ignoreFixed)
where
    tableColumns = [(name,column) \\ column <- fixedColumns ++ columns]
    fixedColumns = [c \\ (t,c) <- fixed | t == name && not (isMember c columns)]
    ignoreFixed  = [(t,c) \\ (t,c) <- fixed | t == name && isMember c columns]

toInsertSQL _ _ = abort "SDS Write: Only single basetables supported for now"

toUpdateSQL :: [ColumnDef] ColumnSourceDef -> (String,[ColumnDef])
toUpdateSQL fixed (BaseTable {name,alias,columns})
    = ("UPDATE " +++ name +++ " SET "+++ join "," [snd col +++ " = ?" \\ col <- tableColumns],ignoreFixed)
where
    tableColumns = [(name,column) \\ column <- fixedColumns ++ columns]
    fixedColumns = [c \\ (t,c) <- fixed | t == name && not (isMember c columns)]
    ignoreFixed  = [(t,c) \\ (t,c) <- fixed | t == name && isMember c columns]

toUpdateSQL _ _ = abort "SDS Write: Only single basetables supported for now"

toUpdateOnDupKeySQL :: [ColumnDef] ColumnSourceDef -> String
toUpdateOnDupKeySQL fixed (BaseTable {name,alias,columns})
    //= " ON DUPLICATE KEY UPDATE " +++ join " , " ["`"+++column +++ "` = VALUES(`"+++column+++"`)"\\ column <- fixedColumns ++ columns]
    = " ON DUPLICATE KEY UPDATE " +++ join " , " [column +++ " = VALUES("+++column+++")"\\ column <- fixedColumns ++ columns]
where
    fixedColumns = [c \\ (t,c) <- fixed | t == name && not (isMember c columns)]
toUpdateOnDupKeySQL _ _ = abort "SDS Write: Only single basetables supported for now"

toDeleteSQL :: ColumnSourceDef -> String
toDeleteSQL (BaseTable {TableDef|name})
    = "DELETE FROM " +++ name
toDeleteSQL _ = abort "SDS Write: Only single basetables supported for now"

toColumnSQL :: ColumnDef -> String
toColumnSQL (table,column) = table+++".`"+++column+++"`"

toSelectSQL :: ColumnSourceDef -> String
toSelectSQL def = "SELECT DISTINCT " +++ join "," (format def)
where
    format (BaseTable {alias,columns})           = [toColumnSQL (alias,col) \\ col <- columns]
    format (InnerJoin base {alias,columns} _ _ ) = format base ++ [toColumnSQL (alias,col) \\ col <- columns]
    format (LeftJoin base {alias,columns} _ _)   = format base ++ [toColumnSQL (alias,col) \\ col <- columns]
    format (RightJoin base {alias,columns}_ _)   = format base ++ [toColumnSQL (alias,col) \\ col <- columns]

toFromSQL :: ColumnSourceDef -> String
toFromSQL def = " FROM " +++ format def
where
    format (BaseTable {name,alias}) = name +++ " AS " +++ alias
    format (InnerJoin base {name,alias} col1 col2)
        = format base +++ " INNER JOIN " +++ name +++ " AS " +++ alias +++ " ON " +++ toColumnSQL col1 +++ " = " +++ toColumnSQL col2
    format (LeftJoin base {name,alias} col1 col2)
        = format base +++ " LEFT JOIN " +++ name +++ " AS " +++ alias +++ " ON " +++ toColumnSQL col1 +++ " = " +++ toColumnSQL col2
    format (RightJoin base {name,alias} col1 col2)
        = format base +++ " RIGHT JOIN " +++ name +++ " AS " +++ alias +++ " ON " +++ toColumnSQL col1 +++ " = " +++ toColumnSQL col2

toWhereSQL :: RowFilterDef -> (String,[SQLValue])
toWhereSQL def = let (w,v) = format def in (" WHERE " +++ w,v)
where
    format (EqualsValue column [])
        = ("1",[])
    format (EqualsValue column [value])
        = (toColumnSQL column +++ " = ?",[value])
    format (EqualsValue column values)
        = (toColumnSQL column +++ " IN (" +++ join "," ["?" \\ _ <- values]+++")",values)
    format (EqualsNull column)
        = (toColumnSQL column +++ " IS NULL",[])
    format (GreaterEqualsValue column value)
        = (toColumnSQL column +++ " >= ?",[value])
    format (SmallerEqualsValue column value)
        = (toColumnSQL column +++ " <= ?",[value])
    format (LikeValue column value)
        = (toColumnSQL column +++ " LIKE '" +++ replaceSubString "'" "\\'" value +++ "'", [])
    format (NotCondition condition)
        = let (sql,values) = format condition in ("NOT ("+++sql+++")",values)
    format (OrCondition conditionl conditionr)
        # (sqll,valuesl) = format conditionl
        # (sqlr,valuesr) = format conditionr
        = ("("+++sqll+++") OR ("+++sqlr+++")",valuesl++valuesr)
    format (AndCondition conditionl conditionr)
        # (sqll,valuesl) = format conditionl
        # (sqlr,valuesr) = format conditionr
        = ("("+++sqll+++") AND ("+++sqlr+++")",valuesl++valuesr)

toOrderBySQL :: [RowOrderDef] -> String
toOrderBySQL defs = " ORDER BY " +++ join "," (map format defs)
where
    format (OrderDesc column) = toColumnSQL column +++ " DESC"
    format (OrderAsc column) = toColumnSQL column +++ " ASC"

collectFixedColumns :: RowFilterDef -> [(ColumnDef,SQLValue)]
collectFixedColumns (EqualsValue column [value])    = [(column,value)]
collectFixedColumns _                               = []

fromSQLWithId :: [SQLValue] -> (Int,a) | mbFromSQL a
fromSQLWithId row = (fromSQL [last row],fromSQL (init row))

//UTIL SDS Combinators
(>++>) infixl 6 :: (RWShared () SQLDatabaseDef SQLDatabaseDef) (RWShared (SQLDatabaseDef,p) r w) -> RWShared p r w | iTask p
(>++>) db sds = sdsSequence ">++>" (\p db -> (db,p)) snd (SDSWriteConst (\_ _ -> Ok Nothing)) (SDSWriteConst (\_ w -> Ok (Just w))) (sdsFocus () db) sds

sqlReadSDS :: String -> ROShared (SQLDatabaseDef,QueryDef) [r] | mbFromSQL r
sqlReadSDS notifyId = sqlShare notifyId readFun writeFun
where
    readFun query cur
        # (sql,values)      = toReadSQL query
        # (err,cur)         = execute sql values cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
		# (err,rows,cur)	= fetchAll cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
                            = (Ok (map fromSQL rows), cur)

    writeFun _ _ cur = (Ok (), cur)

sqlReadWriteOneSDS :: String -> RWShared (SQLDatabaseDef,QueryDef) r r | mbFromSQL, mbToSQL r & gDefault{|*|} r
sqlReadWriteOneSDS notifyId = sqlShare notifyId readFun writeFun
where
    readFun query cur
        # (sql,values)      = toReadSQL query
        # (err,cur)         = execute sql values cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
                            = (Ok (maybe defaultValue fromSQL mbRow),cur)
    writeFun query val cur
        //TEMPORARY FIX: SELECT BEFORE INSERT/UPDATE
        //The 'Upsert' solutions for MySQL and SQLite are different, so there is no good way yet
        //to choose. This crude method always works, but should really be in a transaction
        //Check to insert or update
        # (sql,vals)        = toReadSQL query
        # (err,cur)         = execute sql vals cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
		# (err,mbRow,cur)	= fetchOne cur
		| err=:(Just _)		= (Error (toString (fromJust err)),cur)
        //Insert or update
        # (sql,vals)        = if (isNothing mbRow) (toWriteNewSQL query) (toWriteExistingSQL query)
        # args              = vals ++ toSQL val
        # (err,cur)         = execute sql args cur
        | isJust err        = (Error (toString (fromJust err)),cur)
                            = (Ok (), cur)
/*
        # (sql,vals)        = toWriteSQL query
        # args              = vals ++ toSQL val
        # (err,cur)         = execute sql args cur
        | isJust err        = (Error (toString (fromJust err)),cur)
                            = (Ok (), cur)
*/

sqlLinkSDS :: String String String String-> RWShared (SQLDatabaseDef,Maybe [Int]) [(Int,Int)] [(Int,Int)]
sqlLinkSDS notifyId table col1 col2 = sqlShare notifyId readFun writeFun
where
    query match
        = {columns = BaseTable {name=table,alias=table,columns=[col1,col2]}
          ,rows = fmap (\m -> EqualsValue (table,col1) [SQLVInteger i \\ i <-m]) match
          ,order = Nothing
          }
	readFun match cur
        # (sql,vals) = toReadSQL (query match)
		# (res,cur) = execSelect sql vals cur
		= case res of
			Error e	= (Error e, cur)
            Ok rows = (Ok [(v1,v2) \\ [SQLVInteger v1,SQLVInteger v2] <- rows],cur)

	writeFun match links cur
		//Erase old links
        # (sql,vals)    = toEraseSQL (query match)
		# (err,cur)		= execute sql vals cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		//Write new links
        # (sql,vals)    = toWriteNewSQL (query match)
		# (err,cur)		= executeMany sql [[SQLVInteger v1,SQLVInteger v2:vals] \\ (v1,v2) <- links] cur
		| isJust err	= (Error (toString (fromJust err)),cur)
		= (Ok (), cur)

groupByFst :: [(a,b)] -> Map a [b] | Eq a & Ord a
groupByFst rows = foldl add 'DM'.newMap rows
where
    add m (key,val) = 'DM'.put key (maybe [val] (\vals -> vals ++ [val]) ('DM'.get key m)) m

ungroupByFst :: (Map a [b]) -> [(a,b)]
ungroupByFst index = flatten [[(a,b) \\ b <- bs] \\ (a,bs) <- 'DM'.toList index]

roMaybe :: (RWShared p (Maybe r) ()) -> RWShared (Maybe p) (Maybe r) () | iTask p
roMaybe sds = sdsSelect "roMaybe" choose (\_ _ _ _ -> False) (\_ _ _ _ -> False) (constShare Nothing) sds
where
    choose Nothing  = Left ()
    choose (Just p) = Right p

