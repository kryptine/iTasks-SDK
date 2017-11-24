implementation module PlatformExts.Tuple

import Data.Tuple

class fromTuple a b c :: !(!a,!b) -> c
class toTuple   a b c :: !c -> (!a,!b)
