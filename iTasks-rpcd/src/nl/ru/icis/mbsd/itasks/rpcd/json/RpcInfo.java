package nl.ru.icis.mbsd.itasks.rpcd.json;

import java.util.ArrayList;
import java.util.Iterator;

import org.codehaus.jackson.JsonNode;
/**
 * Container class to store the Rpc-call information sent by the iTasks-system.
 * @author Erik Crombag
 */
public class RpcInfo {

	/**
	 * The protocol through which the remote server will be called.
	 */
	public enum RpcProtocol {HTTP,TCP,System}
	
	/**
	 * If the call is through HTTP: Either GET or Post
	 * At the moment only GET is supported
	 */
	public enum RpcHttpMethod {GET, POST}
	
	/**
	 * The communication format used. At the moment, the daemon passes the 
	 * response directly to the iTasks-system. So this is unused. In the future
	 * this will be used to pass correct POST-messages.
	 */
	public enum RpcMessageType {JSONRPC, XMLRPC, SOAP, Plain}
	
	/**
	 * The direction of the message flow. Either: 
	 * <li>One Way: Only a request to the server 
	 * <li>Request Response: A request is sent to the server and the server answers
	 * <li>Solicit Response: The server sends a question and the client answers
	 * <li>Notification: The server only sends a message
	 */
	public enum RpcCallType {OneWay,RequestResponse,SolicitResponse,Notification}
	
	/**
	 * Parameter types.
	 */
	public enum RpcParameterType {RPCString, RPCBool, RPCInt, RPCReal}
	
	/**
	 * The interface description of the call
	 */
	public class RpcInterface {
		public RpcProtocol protocol;
		public RpcHttpMethod httpMethod = null;
		public RpcMessageType type;
	}
	
	/**
	 * The operation description of the call
	 */
	public class RpcOperation {
		public String name;
		public RpcParameter[] parameters;
		public String location;
		public RpcCallType callType;		
	}
	
	/**
	 * Call parameter description. (Name and Type)
	 */
	public class RpcParameter {
		public String name;
		public RpcParameterType type;
	}
	
	/**
	 * Call parameter value. (Name and Serialized Value)
	 */
	public class RpcParameterValue {
		public String name;
		public String serializedValue;
	}
	
	/**
	 * The Task Id of the task doing the RPC-call
	 */
	public String taskId;
	
	/**
	 * The interface
	 */
	public RpcInterface rpcInterface;
	
	/**
	 * The operation
	 */
	public RpcOperation operation;
	
	/**
	 * The parameter values
	 */
	public RpcParameterValue[] paramValues;
	
	/**
	 * The default constructor
	 * @param info The call info received from the iTasks-system in JSON-format
	 */
	public RpcInfo(JsonNode info){
		this.taskId = info.path("taskId").getTextValue();
				
		this.rpcInterface = new RpcInterface();
		JsonNode protocol = info.path("interface").path("protocol");
		if(protocol.isArray()){
			this.rpcInterface.protocol = RpcProtocol.HTTP;
			//"HTTP httpMethod"
			this.rpcInterface.httpMethod = RpcHttpMethod.valueOf(protocol.path(1).getValueAsText());
		}else{
			this.rpcInterface.protocol = RpcProtocol.valueOf(protocol.getValueAsText());
		}
		this.rpcInterface.type = RpcMessageType.valueOf(info.path("interface").path("type").getTextValue());
		
		this.operation = new RpcOperation();
		this.operation.name = info.path("operation").path("name").getTextValue();
		this.operation.location = info.path("operation").path("location").getTextValue();
		this.operation.callType = RpcCallType.valueOf(info.path("operation").path("callType").getTextValue());
		
		Iterator<JsonNode> paramList = info.path("parameters").getElements();
		ArrayList<RpcParameter> parameters = new ArrayList<RpcParameter>();
		while(paramList.hasNext()){
			JsonNode param = paramList.next();
			RpcParameter rparam = new RpcParameter();
			
			rparam.name = param.path("name").getTextValue();
			rparam.type = RpcParameterType.valueOf(param.path("type").getTextValue());
			
			parameters.add(rparam);
		}		
		this.operation.parameters = parameters.toArray(new RpcParameter[parameters.size()]);
		
		ArrayList<RpcParameterValue> paramValues = new ArrayList<RpcParameterValue>();
		paramList = info.path("paramValues").getElements();
		while(paramList.hasNext()){
			JsonNode pValue = paramList.next();
			RpcParameterValue paramValue = new RpcParameterValue();
			
			paramValue.name = pValue.path("name").getTextValue();
			paramValue.serializedValue = pValue.path("serializedValue").getTextValue();
			
			paramValues.add(paramValue);
		}
		this.paramValues = paramValues.toArray(new RpcParameterValue[paramValues.size()]);		
	}
}
