definition module iTasks.Internal.Distributed.Domain

import iTasks

derive class iTask Domain

:: Hostname :== String
:: Port :== Int
:: Domain = Domain Hostname Port

instance == Domain
