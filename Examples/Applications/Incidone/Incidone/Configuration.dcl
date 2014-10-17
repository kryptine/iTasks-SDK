definition module Incidone.Configuration
import iTasks
from iTasks.API.Extensions.SQLDatabase import :: SQLDatabase, :: SQLDatabaseDef

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
    = {weatherWidgets   :: Maybe Note
      ,vesselLinks      :: [WebLink]
      }
:: WebLink =
    { title :: String
    , url   :: URL
    }

derive class iTask DBConfig, AISConfig, AsteriskConfig, SMTPConfig, WebLinksConfig, WebLink

//Master configuration shares
databaseConfig				:: Shared DBConfig
aisLinkConfig               :: Shared AISConfig
asteriskLinkConfig          :: Shared AsteriskConfig
smtpConfig                  :: Shared SMTPConfig
webLinksConfig              :: Shared WebLinksConfig
adminPassword               :: Shared Password

//Derived configuration shares
databaseDef                 :: RWShared () SQLDatabaseDef SQLDatabaseDef

//Conversion functions
toDatabaseDef               :: DBConfig -> SQLDatabaseDef



