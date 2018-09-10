implementation module Incidone.Configuration
import iTasks, iTasks.Extensions.SQLDatabase, iTasks.Extensions.Web

derive class iTask DBConfig, AISConfig, AsteriskConfig, SMTPConfig, WebLinksConfig, WebLink

//Shared stores
databaseConfig :: SDSLens () DBConfig DBConfig
databaseConfig = sharedStore "databaseConfig" InternalSQLiteDB

aisLinkConfig :: SDSLens () AISConfig AISConfig
aisLinkConfig = sharedStore "aisLinkConfig" {AISConfig|host="localhost",port=2000}

asteriskLinkConfig :: SDSLens () AsteriskConfig AsteriskConfig
asteriskLinkConfig = sharedStore "asteriskLinkConfig" {AsteriskConfig|host="localhost",port=5038,username="admin",password="secret"}

smtpConfig :: SDSLens () SMTPConfig SMTPConfig
smtpConfig = sharedStore "smtpConfig" {SMTPConfig|host="localhost",port=25}

webLinksConfig :: SDSLens () WebLinksConfig WebLinksConfig
webLinksConfig = sharedStore "webLinksConfig" {WebLinksConfig|weatherWidgets=Nothing,vesselLinks=[]}

adminPassword :: SDSLens () Password Password
adminPassword = sharedStore "adminPassword" (Password "admin")

//Derived configuration shares
databaseDef :: SDSLens () SQLDatabaseDef SQLDatabaseDef
databaseDef = mapReadWrite (toDatabaseDef,\_ r -> Nothing) Nothing (toReadOnly databaseConfig)

//Conversion functions
toDatabaseDef :: DBConfig -> SQLDatabaseDef
toDatabaseDef InternalSQLiteDB = SQLiteDatabase "incidone.db"
toDatabaseDef (ExternalMySQLDB cfg) = MySQLDatabase cfg


