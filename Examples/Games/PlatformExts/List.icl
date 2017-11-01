implementation module PlatformExts.List

import Data.List
import StdBool

/** hasDup @as = True:
       @as has at least one element that occurs twice.
    hasDup @as = False:
       @as has no elements that occur twice.
*/
hasDup :: ![a] -> Bool | Eq a
hasDup []     = False
hasDup [x:xs] = isMember x xs || hasDup xs

// variants of the fold functions that use a queue instead of a fixed size list:
qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldl _ _ a []
	= a
qfoldl f g a [b:bs]
	= let a` = g a b in qfoldl f g a` (bs ++ f a` b)

qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a
qfoldr _ _ a []
	= a
qfoldr f g a [b:bs]
	= let a` = g b a in qfoldr f g a` (bs ++ f a` b)
