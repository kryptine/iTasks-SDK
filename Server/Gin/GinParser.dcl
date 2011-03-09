definition module GinParser

import StdList
import GenPrint
import JSON
import Monad

import GinConfig
import GinSyntax
import GinAbstractSyntax

from iTasks import ::JSONNode, ::VerSt, ::UpdateMask, ::USt, ::UpdateMode, ::VSt, ::Visualization
from iTasks import class iTask, generic gVisualize, generic gUpdate, generic gDefaultMask, generic gVerify, generic JSONEncode, generic JSONDecode, generic gEq

:: GPath = GRoot | GChildNode String GPath | GChildNodeNr String Int GPath

instance toString GPath

:: GParseResult a = GSuccess a | GError [(GPath, String)]
:: GParseState a = GParseState (GPath -> GParseResult a)

derive class iTask GPath, GParseResult, GParseState

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

gToAModule :: !GModule !GinConfig !*World -> (GParseState AModule, *World)

//Utils

foldl1 :: (a a -> a) [a] -> a
