package nl.ru.icis.mbsd.itasks.rpcd.worker;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo.RpcHttpMethod;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo.RpcParameterValue;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.utils.URIUtils;
import org.apache.http.client.utils.URLEncodedUtils;
import org.apache.http.message.BasicNameValuePair;
import org.apache.log4j.Logger;

/**
 * Does a call to a Web Service. 
 * This worker is started if the protocol tag in {@link RpcInfo} equals 'HTTP'.  
 * How the call is made depends on the configuration of the {@code HTTPMethod}.
 * Parameters are passed in URL-encoded format in the request and the result is passed
 * to the iTasks-system without any conversion.
 * 
 * @see RpcInfo
 * @author Erik Crombag
 */
public class WebServiceWorker extends RemoteServiceWorker {

	/**
	 * Whether the daemon is finished.
	 */
	private boolean done = false;

	private Logger log;

	/**
	 * Default Constructor
	 * @see RemoteServiceWorker
	 */
	public WebServiceWorker(String url, String handlerpath, String session, RpcInfo execInfo, int interval){

		super(url,handlerpath,session,execInfo, interval);

		this.log = Logger.getLogger("rpcd.remoteserviceworker.webserviceworker");

		t = new Thread(this);
		t.start();
	}

	@Override
	public void run() {
		log.debug("WebServiceWorker for task "+execInfo.taskId+" has started");

		try {
			sendStatus("Started");
			try {
				HttpResponse response = null;

				if(execInfo.rpcInterface.httpMethod == RpcHttpMethod.GET) response = doGet();
				else response = doPost();

				//TODO: Create something for single way messages...

				if(response != null){
					HttpEntity entity = response.getEntity();

					if(entity != null){
						InputStream in = entity.getContent();

						ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
						byte[] tmp = new byte[1024];

						while(true){
							int r = in.read(tmp);
							if(r == -1) break;
							out.write(tmp,0,r);
						}

						sendResult(out.toByteArray());	
					}
				}		
			} catch (ClientProtocolException e) {
				log.error("Exception occurred while executing HTTP request in WebServiceWorker for task "+execInfo.taskId, e);
				sendError("Error in executing HTTP Request");
			} catch (IOException e) {
				log.error("Exception occurred while executing HTTP request in WebServiceWorker for task "+execInfo.taskId, e);
				sendError("IO Exception for task "+execInfo.taskId);
			} catch (URISyntaxException e) {
				log.error("Location "+execInfo.operation.location+" is not in valid URI format for task "+execInfo.taskId, e);
				sendError("URI Syntax exception for task "+execInfo.taskId);
			}
		}catch(Exception e){
			log.fatal("Cannot send error message to iTask-system for task "+execInfo.taskId,e);
		}

		log.debug("WebServiceWorker for task "+execInfo.taskId+" has finished");
		this.done = true;
	}

	// Do this in case of a HTTP Get
	private HttpResponse doGet() throws ClientProtocolException, IOException, URISyntaxException{

		List<NameValuePair> params = new ArrayList<NameValuePair>();

		RpcParameterValue[] callParams = execInfo.paramValues;
		for(RpcParameterValue callParam : callParams){
			params.add(new BasicNameValuePair(callParam.name,callParam.serializedValue));
		}

		URI location = new URI(execInfo.operation.location);
		URI uri = URIUtils.createURI(location.getScheme(),location.getHost(),location.getPort(),location.getPath(),URLEncodedUtils.format(params, "UTF-8"),null);

		HttpGet get = new HttpGet(uri);

		return this.httpClient.execute(get);
	}

	// Do this in case of a HTTP Post
	private HttpResponse doPost() throws ClientProtocolException, IOException, URISyntaxException{
	
		List<NameValuePair> params = new ArrayList<NameValuePair>();
		RpcParameterValue[] callParams = execInfo.paramValues;
		
		for(RpcParameterValue callParam : callParams){
			params.add(new BasicNameValuePair(callParam.name,callParam.serializedValue));
		}

		URI location = new URI(execInfo.operation.location);
		
		HttpPost post = new HttpPost(location);
		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(params,"UTF-8");
		post.setEntity(entity);		
		
		return this.httpClient.execute(post);
	}

	@Override
	public void stop() {
		this.t.interrupt();
	}

	@Override
	public boolean isFinished() {
		return this.done;
	}
}
