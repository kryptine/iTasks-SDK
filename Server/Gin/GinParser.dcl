definition module GinParser

import StdList
import GenPrint
import JSON
import Monad
import GinSyntax
import GinAbstractSyntax

:: GPath = GRoot | GChildNode String GPath | GChildNodeNr String Int GPath
derive gPrint GPath
derive JSONEncode GPath
instance toString GPath

:: GParseResult a = GSuccess a | GError [(GPath, String)]
:: GParseState a

isParseError :: (GParseResult a) -> Bool
getParseSuccess :: (GParseResult a) -> a
getParseError :: (GParseResult a) -> [(GPath, String)] 

instance Monad GParseState

parseChild :: String (GParseState a) -> GParseState a
parseChildN :: String Int (GParseState a) -> GParseState a
parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseChildMap :: String (a -> GParseState b) [a] -> GParseState [b]
orElse :: (GParseState a) (GParseState a) -> GParseState a

parseError :: String -> GParseState a
parseErrorInChild :: String String -> GParseState a
parseErrorInChildN :: String Int String ->GParseState a
parseErrorInChildren :: String [Int] String -> GParseState a

getCurrentPath :: GParseState GPath
withPath :: GPath (GParseState a) -> GParseState a

runParse :: (GParseState a) -> GParseResult a

gToAModule :: GModule -> GParseState AModule

//Utils

foldl1 :: (a a -> a) [a] -> a
