implementation module geonames_webservices

import JSON
import RPC
import TSt

ocean_names :: Real Real (String -> String) -> Task String
ocean_names lat lng parsefun = mkRpcTask "Ocean Names"
	{ RPCExecute
	| taskId = ""
	, interface = { RPCInterface | protocol = HTTP GET, type = JSONRPC }
	, operation = { RPCOperation | name = "Ocean Names", parameters = [{ RPCParam | name = "lat", type = RPCReal }, { RPCParam | name = "lng", type = RPCReal }], location = "http://ws.geonames.org/oceanJSON", callType = RequestResponse, returnType = "String" }
	, paramValues = [{ RPCParamValue | name="lat", serializedValue = toString lat},{ RPCParamValue | name="lng", serializedValue = toString lng}]
	, status = ""
	} parsefun

country_code :: Real Real String (String -> String) -> Task String
country_code lat lng type parsefun = mkRpcTask "Country Code"
	{ RPCExecute
	| taskId = ""
	, interface = { RPCInterface | protocol = HTTP GET, type = JSONRPC }
	, operation = { RPCOperation | name = "Country Code", parameters = [{ RPCParam | name = "lat", type = RPCReal }, { RPCParam | name = "lng", type = RPCReal }, { RPCParam | name = "type", type = RPCString }], location = "http://ws.geonames.org/countryCode", callType = RequestResponse, returnType = "String" }
	, paramValues = [{ RPCParamValue | name="lat", serializedValue = toString lat},{ RPCParamValue | name="lng", serializedValue = toString lng},{ RPCParamValue | name="type", serializedValue = toString type}]
	, status = ""
	} parsefun

