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
databaseConfig				      :: SDSLens () DBConfig DBConfig
aisLinkConfig               :: SDSLens () AISConfig AISConfig
asteriskLinkConfig          :: SDSLens () AsteriskConfig AsteriskConfig
smtpConfig                  :: SDSLens () SMTPConfig SMTPConfig
webLinksConfig              :: SDSLens () WebLinksConfig WebLinksConfig
adminPassword               :: SDSLens () Password Password

//Derived configuration shares
databaseDef                 :: SDSLens () SQLDatabaseDef SQLDatabaseDef

//Conversion functions
toDatabaseDef               :: DBConfig -> SQLDatabaseDef



