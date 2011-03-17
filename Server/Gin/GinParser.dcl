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

:: GPath	:== [GPathNode]

:: GPathNode	= PNDefinition Int
				| PNBody
				| PNNode Int
				| PNEdge Int
				| PNActualParam Int
				| PNPattern
				| PNListItem Int
				
instance toString GPath

:: GParseResult a = GSuccess a | GError [(GPath, String)]
:: GParseState a = GParseState (GPath -> GParseResult a)

derive class iTask GPathNode, GParseResult, GParseState

isParseError :: (GParseResult a) -> Bool
getParseSuccess :: (GParseResult a) -> a
getParseError :: (GParseResult a) -> [(GPath, String)] 

instance Monad GParseState

parseChild :: GPathNode (GParseState a) -> GParseState a
parseMap :: (a -> GParseState b) [a] -> GParseState [b]
parseChildMap :: (Int -> GPathNode) (a -> GParseState b) [a] -> GParseState [b]
orElse :: (GParseState a) (GParseState a) -> GParseState a

parseError :: String -> GParseState a
parseErrorInChild :: GPathNode String -> GParseState a
parseErrorInChildren :: (Int -> GPathNode) [Int] String -> GParseState a

getCurrentPath :: GParseState GPath
withPath :: GPath (GParseState a) -> GParseState a

runParse :: (GParseState a) -> GParseResult a

gToAModule :: !GModule !GinConfig !*World -> (GParseState AModule, *World)

//Utils

foldl1 :: (a a -> a) [a] -> a
