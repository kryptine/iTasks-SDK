package nl.ru.icis.mbsd.itasks.rpcd.worker;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import nl.ru.icis.mbsd.itasks.rpcd.Constants;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo;
import nl.ru.icis.mbsd.itasks.rpcd.util.Base64Coder;

import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.log4j.Logger;
import org.codehaus.jackson.JsonNode;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * Super-class which needs to be extended by specialized worker threads. 
 * This class already contains methods for the sending of messages.
 * @author Erik Crombag
 */
public abstract class RemoteServiceWorker implements Runnable{
	
	/**
	 * The default http-client used for sending and receiving messages.
	 */
	protected DefaultHttpClient httpClient = new DefaultHttpClient();
	private String url;
	private String handler;
	private String session;
	private int interval;
	
	/**
	 * The call information
	 */
	protected RpcInfo execInfo;
	
	/**
	 * Worker thread
	 */
	protected Thread t;
	
	/**
	 * Logger
	 */
	protected Logger log;
	
	/**
	 * The default constructor
	 * @param url The url to the iTasks-system
	 * @param handler The path to the handlers
	 * @param session The session key
	 * @param execInfo The call info
	 * @param interval The interval between different retries, multiplied by the slow-down factor
	 */
	protected RemoteServiceWorker(String url, String handler, String session, RpcInfo execInfo, int interval){
		this.url = url;
		this.handler = handler;
		this.session = session;
		this.execInfo = execInfo;
		this.interval = interval;
	}
	
	/**
	 * Retrieve the task-id of this worker
	 * @return The iTasks task-id
	 */
	public String getTaskId(){
		return execInfo.taskId;
	}
	
	/**
	 * Stop the worker and clean up
	 */
	public abstract void stop();
	
	/**
	 * Whether this worker is finished.
	 * @return True if the worker is finished
	 */
	public abstract boolean isFinished();
	
	/**
	 * Send a Base64-encoded result message to the iTasks-system
	 * @param result the unencoded result.
	 * @return True if the message is received correctly
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	protected boolean sendResult(byte[] result) throws ClientProtocolException, IOException{
		String encodedResult = new String(Base64Coder.encode(result));
		String msg = mkJsonMsg(true,false,true,encodedResult,"","");
		return sendMessage(msg,0);
	}

	/**
	 * Send an Error message to the iTasks-system
	 * @param error The error message
	 * @return True if the message is received correctly
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	protected boolean sendError(String error) throws ClientProtocolException, IOException{
		String msg = mkJsonMsg(false,true,true,"","",error);
		return sendMessage(msg,0);
	}
	
	/**
	 * Send a status message to the iTasks-system
	 * @param status The status message
	 * @return True if the message is received correctly
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	protected boolean sendStatus(String status) throws ClientProtocolException, IOException{
		String msg = mkJsonMsg(true,false,false,"",status,"");
		return sendMessage(msg,0);
	}

	
	private String mkJsonMsg(boolean success, boolean error, boolean finished, String result, String status, String errormsg){
		return "{\"success\":"+success+",\"error\":"+error+",\"finished\":"+finished+",\"result\":\""+result+"\",\"status\":\""+status+"\",\"errormsg\":\""+error+"\"}";
	}

	/**
	 * Does the actual sending of the message
	 * @param msg The message in Json-format
	 * @param attempt The amount of attempts taken so far
	 * @return True if the message is received correctly
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	private boolean sendMessage(String msg,int attempt) throws ClientProtocolException, IOException{

		if(log == null) log = Logger.getLogger("rpcd.remoteserviceworker.super");
		
		List<NameValuePair> params = new ArrayList<NameValuePair>();
		params.add(new BasicNameValuePair("_session",session));
		params.add(new BasicNameValuePair("_rpctaskid",execInfo.taskId));
		params.add(new BasicNameValuePair("_rpcresult",msg));

		HttpPost post = new HttpPost(url+handler+Constants.RESPONSEHANDLER);
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(params,"UTF-8");
		post.setEntity(entity);

		String response = httpClient.execute(post,new BasicResponseHandler());
		JsonNode root = new ObjectMapper().readValue(response,JsonNode.class);

		if(!root.path("success").getBooleanValue()){
			if(attempt > Constants.MAXATTEMPTS) {
				log.fatal("Fatal error in sending message to iTasks-system for taskId "+execInfo.taskId+". Stopping attempts");
				return false;
			}else{	
				log.error("("+attempt+" of "+Constants.MAXATTEMPTS+") Failed to send message to iTasks-system for taskId "+execInfo.taskId+", error message returned: "+root.path("error").getTextValue());
				synchronized(t){
					try { t.wait(interval*Constants.SLOWDOWN); } catch (InterruptedException e) {}
				}
				return sendMessage(msg,attempt++);
			}
		}else{
			return true;
		}				
	}
}
