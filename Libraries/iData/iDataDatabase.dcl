definition module iDataDatabase

import iDataExceptions

universalDB :: !(!Init,!Lifespan,!a,!String) !(String a -> Judgement) !*HSt -> (a,!*HSt) | iData a
