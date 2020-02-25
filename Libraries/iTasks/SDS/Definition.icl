implementation module iTasks.SDS.Definition

import StdEnv

import Data.GenEq
import Data.Either
import Data.Error
import Data.Func
import Data.List
import Data.Maybe
import Internet.HTTP
import Text.GenJSON

import dynamic_string

import iTasks.Internal.IWorld
import iTasks.Internal.Task

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import iTasks.WF.Derives

derive gText SDSNotifyRequest, RemoteNotifyOptions

gText{|SDSIdentity|} _ Nothing = []
gText{|SDSIdentity|} f (Just id) =
	[ id.id_name
	: case id.id_child_a of
		NoChild = []
		Child a =
			[ "("
			: gText{|*|} f (Just a) ++ case id.id_child_b of
				NoChild = [")"]
				Child b = [",":gText{|*|} f (Just b) ++ [")"]]
			]
	]

instance < MaybeSDSIdentityChild
where
	(<) NoChild   NoChild   = True
	(<) NoChild   _         = False
	(<) (Child _) NoChild   = False
	(<) (Child a) (Child b) = a < b

instance < SDSIdentity
where
	(<) a b
		| a.id_name < b.id_name = True
		| a.id_name > b.id_name = False
		| a.id_child_a < b.id_child_a = True
		| a.id_child_a > b.id_child_a = False
		| otherwise = a.id_child_b < b.id_child_b

instance == MaybeSDSIdentityChild
where
	(==) a b = case a of
		NoChild = b=:NoChild
		Child a = case b of
			NoChild = False
			Child b = a == b

instance == SDSIdentity
where
	(==) a b =
		a.id_name == b.id_name &&
		a.id_child_a == b.id_child_a &&
		a.id_child_b == b.id_child_b

instance toString SDSIdentity where toString sdsId = toSingleLineText sdsId

JSONEncode{|SDSIdentity|} _ id = [JSONObject
	[ ("name",JSONString id.id_name)
	: case id.id_child_a of
		NoChild = []
		Child a =
			[ ("child_a",hd (JSONEncode{|*|} True a))
			: case id.id_child_b of
				NoChild = []
				Child b = [("child_b",hd (JSONEncode{|*|} True b))]
			]
	]]
JSONDecode{|SDSIdentity|} _ org=:[JSONObject obj:rest] = case lookup "name" obj of
	Just (JSONString name) = case lookup "child_a" obj of
		Just child_a = case JSONDecode{|*|} True [child_a] of
			(Just a,[]) = case lookup "child_b" obj of
				Just child_b = case JSONDecode{|*|} True [child_b] of
					(Just b,[]) = if (length obj==3)
						(Just {id_name=name,id_child_a=Child a,id_child_b=Child b},rest)
						(Nothing, org)
					_ = (Nothing, org)
				_ = if (length obj==2)
					(Just {id_name=name,id_child_a=Child a,id_child_b=NoChild},rest)
					(Nothing, org)
			_ = (Nothing, org)
		_ = if (length obj==1)
			(Just {id_name=name,id_child_a=NoChild,id_child_b=NoChild},rest)
			(Nothing, org)
	_ = (Nothing, org)

dependsOnShareWithName :: !String !SDSIdentity -> Bool
dependsOnShareWithName name id = depends id
where
	depends {id_name,id_child_a,id_child_b} =
		id_name == name ||
		dependsChild id_child_a ||
		dependsChild id_child_b

	dependsChild NoChild = False
	dependsChild (Child i) = depends i

instance toString (WebServiceShareOptions p r w)
where
	toString (HTTPShareOptions {HTTPHandlers|host, port}) = "http://" +++ host +++ ":" +++ toString port
	toString (TCPShareOptions {TCPHandlers|host, port}) = "tcp://" +++ host +++ ":" +++ toString port

// some efficient order to be able to put notify requests in sets
instance < SDSNotifyRequest where
	(<) x y =
			((x.reqTaskId, x.reqSDSId, dynamic_to_string $ hyperstrict x.cmpParam), x.remoteOptions)
		<
			((y.reqTaskId, y.reqSDSId, dynamic_to_string $ hyperstrict y.cmpParam), y.remoteOptions)

instance < RemoteNotifyOptions where
	(<) left right = (left.hostToNotify, left.portToNotify, left.remoteSdsId) <
	                 (right.hostToNotify, right.portToNotify, right.remoteSdsId)
