definition module PlatformExts.Tuple

/**	This module extends Data.Tuple with a few more functions.
*/

import Data.Tuple

class fromTuple a b c :: !(!a,!b) -> c
class toTuple   a b c :: !c -> (!a,!b)
