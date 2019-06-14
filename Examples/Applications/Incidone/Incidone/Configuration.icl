implementation module Incidone.Configuration
import iTasks, iTasks.Extensions.SQLDatabase, iTasks.Extensions.Web

derive class iTask DBConfig, AISConfig, AsteriskConfig, SMTPConfig, WebLinksConfig, WebLink

//Shared stores
databaseConfig :: SimpleSDSLens DBConfig
databaseConfig = sharedStore "databaseConfig" InternalSQLiteDB

aisLinkConfig :: SimpleSDSLens AISConfig
aisLinkConfig = sharedStore "aisLinkConfig" {AISConfig|host="localhost",port=2000}

asteriskLinkConfig :: SimpleSDSLens AsteriskConfig
asteriskLinkConfig = sharedStore "asteriskLinkConfig" {AsteriskConfig|host="localhost",port=5038,username="admin",password="secret"}

smtpConfig :: SimpleSDSLens SMTPConfig
smtpConfig = sharedStore "smtpConfig" {SMTPConfig|host="localhost",port=25}

webLinksConfig :: SimpleSDSLens WebLinksConfig
webLinksConfig = sharedStore "webLinksConfig" {WebLinksConfig|weatherWidgets=Nothing,vesselLinks=[]}

adminPassword :: SimpleSDSLens Password
adminPassword = sharedStore "adminPassword" (Password "admin")

//Derived configuration shares
databaseDef :: SimpleSDSLens SQLDatabaseDef
databaseDef = mapReadWrite (toDatabaseDef,\_ r -> Nothing) Nothing (toReadOnly databaseConfig)

//Conversion functions
toDatabaseDef :: DBConfig -> SQLDatabaseDef
toDatabaseDef InternalSQLiteDB = SQLiteDatabase "incidone.db"
toDatabaseDef (ExternalMySQLDB cfg) = MySQLDatabase cfg


