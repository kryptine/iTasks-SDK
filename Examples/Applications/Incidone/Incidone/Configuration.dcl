definition module Incidone.Configuration
import iTasks
from iTasks.Extensions.SQLDatabase import :: SQLDatabase, :: SQLDatabaseDef
from iTasks.Extensions.Web import :: URL

:: DBConfig
    = InternalSQLiteDB
    | ExternalMySQLDB SQLDatabase

:: AISConfig
    = {host     :: !String
      ,port     :: !Int
      }
:: AsteriskConfig
    = {host     :: !String
      ,port     :: !Int
      ,username :: !String
      ,password :: !String
      }
:: SMTPConfig
    = {host     :: !String
      ,port     :: !Int
      }

:: WebLinksConfig
    = {weatherWidgets   :: Maybe String
      ,vesselLinks      :: [WebLink]
      }
:: WebLink =
    { title :: String
    , url   :: URL
    }

derive class iTask DBConfig, AISConfig, AsteriskConfig, SMTPConfig, WebLinksConfig, WebLink

//Master configuration shares
databaseConfig     :: SimpleSDSLens DBConfig
aisLinkConfig      :: SimpleSDSLens AISConfig
asteriskLinkConfig :: SimpleSDSLens AsteriskConfig
smtpConfig         :: SimpleSDSLens SMTPConfig
webLinksConfig     :: SimpleSDSLens WebLinksConfig
adminPassword      :: SimpleSDSLens Password

//Derived configuration shares
databaseDef                 :: SimpleSDSLens SQLDatabaseDef

//Conversion functions
toDatabaseDef               :: DBConfig -> SQLDatabaseDef



