implementation module Http

import StdOverloaded, StdString, StdList

http_emptyRequest :: HTTPRequest
http_emptyRequest	= {	req_method		= ""
					,	req_path		= ""
					,	req_query		= ""
					,	req_version		= ""
					,	req_protocol	= HTTPProtoHTTP
					,	req_headers		= []
					,	req_data		= ""
					,	arg_get			= []
					,	arg_post		= []
					,	arg_uploads		= []	
					,	server_name		= ""
					,	server_port		= 0
					,	client_name		= ""
					}
					
http_emptyResponse :: HTTPResponse					
http_emptyResponse	= {	rsp_headers		= []
					,	rsp_data		= ""
					}

http_emptyUpload :: HTTPUpload
http_emptyUpload	= {	upl_name		= ""
					,	upl_filename	= ""
					,	upl_mimetype	= ""
					,	upl_content		= ""
					}

http_getValue :: String [(String, String)] a -> a | fromString a
http_getValue name values def = hd ([fromString v \\ (n,v) <- values | n == name] ++ [def])

