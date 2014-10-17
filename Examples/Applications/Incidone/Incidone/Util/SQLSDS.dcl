definition module Incidone.Util.SQLSDS

import iTasks, Data.Maybe, Database.SQL, iTasks.API.Extensions.SQLDatabase, Incidone.OP.Concepts
//Utility types and functions for defining SQL based shares

:: QueryDef =
    {columns    :: ColumnSourceDef
    ,rows       :: Maybe RowFilterDef
    ,order      :: Maybe [RowOrderDef]
    }

:: ColumnDef :== (TableName,ColumnName) //(table,column)
:: TableName :== String
:: ColumnName :== String

:: ColumnSourceDef
    = BaseTable TableDef
    | InnerJoin ColumnSourceDef TableDef ColumnDef ColumnDef
    | LeftJoin ColumnSourceDef TableDef ColumnDef ColumnDef
    | RightJoin ColumnSourceDef TableDef ColumnDef ColumnDef

:: TableDef =
    { name      :: TableName
    , alias     :: TableName
    , columns   :: [ColumnName]
    }

:: RowFilterDef
    = EqualsValue ColumnDef [SQLValue]
    | EqualsNull ColumnDef
    | GreaterEqualsValue ColumnDef SQLValue
    | SmallerEqualsValue ColumnDef SQLValue
    | LikeValue ColumnDef String
    //Operators
    | NotCondition RowFilterDef
    | OrCondition RowFilterDef RowFilterDef
    | AndCondition RowFilterDef RowFilterDef

:: RowOrderDef
    = OrderDesc ColumnDef
    | OrderAsc ColumnDef

derive class iTask QueryDef, RowFilterDef, ColumnSourceDef, TableDef, RowOrderDef

toReadSQL           :: QueryDef -> (String,[SQLValue])
toWriteSQL          :: QueryDef -> (String,[SQLValue])
toEraseSQL          :: QueryDef -> (String,[SQLValue])
toInsertSQL         :: [ColumnDef] ColumnSourceDef -> (String,[ColumnDef])
toUpdateOnDupKeySQL :: [ColumnDef] ColumnSourceDef -> String
toDeleteSQL         :: ColumnSourceDef -> String
toColumnSQL         :: ColumnDef -> String
toSelectSQL         :: ColumnSourceDef -> String
toFromSQL           :: ColumnSourceDef -> String
toWhereSQL          :: RowFilterDef -> (String,[SQLValue])
toOrderBySQL        :: [RowOrderDef] -> String

fromSQLWithId       :: [SQLValue] -> (Int,a) | mbFromSQL a

(>++>) infixl 6     :: (RWShared () SQLDatabaseDef SQLDatabaseDef) (RWShared (SQLDatabaseDef,p) r w) -> RWShared p r w | iTask p

sqlReadSDS          :: String -> ROShared (SQLDatabaseDef,QueryDef) [r] | mbFromSQL r
sqlReadWriteOneSDS  :: String -> RWShared (SQLDatabaseDef,QueryDef) r r | mbFromSQL, mbToSQL r & gDefault{|*|} r
sqlLinkSDS          :: String String String String-> RWShared (SQLDatabaseDef,Maybe [Int]) [(Int,Int)] [(Int,Int)]

groupByFst          :: [(a,b)] -> Map a [b] | Eq a & Ord a
ungroupByFst        :: (Map a [b]) -> [(a,b)]
roMaybe             :: (RWShared p (Maybe r) ()) -> RWShared (Maybe p) (Maybe r) () | iTask p
