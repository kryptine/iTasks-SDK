implementation module Incidone.Configuration
import iTasks, iTasks.API.Extensions.SQLDatabase

derive class iTask DBConfig, AISConfig, AsteriskConfig, SMTPConfig, WebLinksConfig, WebLink

//Shared stores
databaseConfig :: Shared DBConfig
databaseConfig = sharedStore "databaseConfig" InternalSQLiteDB

aisLinkConfig :: Shared AISConfig
aisLinkConfig = sharedStore "aisLinkConfig" {AISConfig|host="localhost",port=2000}

asteriskLinkConfig :: Shared AsteriskConfig
asteriskLinkConfig = sharedStore "asteriskLinkConfig" {AsteriskConfig|host="localhost",port=5038,username="admin",password="secret"}

smtpConfig :: Shared SMTPConfig
smtpConfig = sharedStore "smtpConfig" {SMTPConfig|host="localhost",port=25}

webLinksConfig :: Shared WebLinksConfig
webLinksConfig = sharedStore "webLinksConfig" {WebLinksConfig|weatherWidgets=Nothing,vesselLinks=[]}

adminPassword :: Shared Password
adminPassword = sharedStore "adminPassword" (Password "admin")

//Derived configuration shares
databaseDef :: RWShared () SQLDatabaseDef SQLDatabaseDef
databaseDef = mapReadWrite (toDatabaseDef,\_ _ -> Nothing) (toReadOnly databaseConfig)

//Conversion functions
toDatabaseDef :: DBConfig -> SQLDatabaseDef
toDatabaseDef InternalSQLiteDB = SQLiteDatabase "incidone.db"
toDatabaseDef (ExternalMySQLDB cfg) = MySQLDatabase cfg


