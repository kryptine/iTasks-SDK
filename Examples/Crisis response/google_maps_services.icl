implementation module google_maps_services

import JSON
import RPC
import TSt

reverse_geocoding :: String String Bool String (String -> String) -> Task String
reverse_geocoding q output sensor api_key parsefun = mkRpcTask "Reverse Geocoding"
	{ RPCExecute
	| taskId = ""
	, interface = { RPCInterface | protocol = HTTP GET, type = JSONRPC }
	, operation = { RPCOperation | name = "Reverse Geocoding", parameters = [{ RPCParam | name = "q", type = RPCString }, { RPCParam | name = "output", type = RPCString }, { RPCParam | name = "sensor", type = RPCBool }, { RPCParam | name = "api_key", type = RPCString }], location = "http://maps.google.com/maps/geo", callType = RequestResponse, returnType = "String" }
	, paramValues = [{ RPCParamValue | name="q", serializedValue = toString q},{ RPCParamValue | name="output", serializedValue = toString output},{ RPCParamValue | name="sensor", serializedValue = toString sensor},{ RPCParamValue | name="api_key", serializedValue = toString api_key}]
	, status = ""
	} parsefun

