definition module PlatformExts.List

/** This module extends Data.List with a few more functions.
*/

import Data.List

/** hasDup @as = True:
       @as has at least one element that occurs twice.
    hasDup @as = False:
       @as has no elements that occur twice.
*/
hasDup :: ![a] -> Bool | Eq a

// variants of the fold functions that use a queue instead of a fixed size list:
qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a
