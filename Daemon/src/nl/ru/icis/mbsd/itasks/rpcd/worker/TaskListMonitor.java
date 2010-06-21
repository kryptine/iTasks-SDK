package nl.ru.icis.mbsd.itasks.rpcd.worker;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.ru.icis.mbsd.itasks.rpcd.Constants;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo.RpcProtocol;
import nl.ru.icis.mbsd.itasks.rpcd.util.Queue;

import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.BasicResponseHandler;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.log4j.Logger;
import org.codehaus.jackson.JsonNode;
import org.codehaus.jackson.map.ObjectMapper;

/**
 * Worker class which is basically the heart of the Daemon. While alive it does the following routine:
 * <ol>
 * <li>Retrieve a list of all RPC-tasks from the iTasks system
 * <li>Check whether there are new calls. If so start the appropriate {@link RemoteServiceWorker}
 * <li>Garbage collect any finished workers
 * <li>Sleep for a specific time, specified by the interval parameter in the constructor.
 * </ol>
 * 
 * If the retrieval of the list fails, the TaskListMonitor retries for {@code Constants.MAXATTEMPTS}. If it still fails, the daemon is exited.
 * 
 * @author Erik Crombag
 *
 */
public class TaskListMonitor implements Runnable {

	private String url;
	private String handlerpath;
	private int interval;

	private boolean running;
	private boolean loggedIn;

	private Queue callQueue;

	private Thread t;

	private Logger log;

	private String sessionKey;

	private DefaultHttpClient http;

	/**
	 * Default Constructor
	 * @param url Url to the iTasks System
	 * @param handlerpath Path the to the handlers
	 * @param interval The amount of sleep-time in ms, between each cycle.
	 */
	public TaskListMonitor(String url, String handlerpath, int interval){

		this.log = Logger.getLogger("rpcd.tasklistmonitor");

		this.url = url;
		this.interval = interval;
		this.handlerpath = handlerpath;

		this.callQueue = new Queue();
		
		start();
	}

	@Override
	public void run() {
		int attempt = 0;

		log.debug("TaskListMonitor started");

		while(running){
			attempt++;

			log.debug("Begin TaskListMonitor execution-loop. Attempt "+attempt);
			
			while(!loggedIn){
				
				log.debug("Begin Login sequence");
				
				try {
					if(login()) loggedIn = true;
				} catch (ClientProtocolException e) {
					log.error("ClientProtocolException while logging in to iTasks-system",e);
					synchronized(t){ try { t.wait(interval*Constants.SLOWDOWN); } catch (InterruptedException e1) {} }
				} catch (IOException e) {
					log.error("IOException while logging in to iTasks-system. "+e.getLocalizedMessage());
					synchronized(t){ try { t.wait(interval*Constants.SLOWDOWN); } catch (InterruptedException e1) {} }
				}				
			}
		
			if(attempt > Constants.MAXATTEMPTS){
				log.fatal("Maximum attempts reached. Exiting Daemon.");
				this.running = false;
			}else{
				try{
					// XXX: This is the main daemon routine
					//1. Check for Tasks-list, retrieve info and start new workers
					checkList();
					//2. Garbage collect finished workers
					garbageCollect();
					//3. Reset attempt watch dog.
					attempt = 0;
									
				} catch (ClientProtocolException e) {
					log.fatal("("+attempt+" of "+Constants.MAXATTEMPTS+") ClientProtocolException while communicating with iTasks-system.",e);
					synchronized(t){ try { t.wait(interval*(Constants.SLOWDOWN-1)); } catch (InterruptedException e1) {} }
				} catch (IOException e) {
					log.fatal("("+attempt+" of "+Constants.MAXATTEMPTS+") IOException while communicating with iTasks-system.",e);
					synchronized(t){ try { t.wait(interval*(Constants.SLOWDOWN-1)); } catch (InterruptedException e1) {} }
				}	
				
				synchronized(t){
					try {
						t.wait(interval);
					} catch (InterruptedException e) {
						log.trace("TaskListMonitor Interrupted");
					}
				}					
			}
		}

		log.debug("TaskListMonitor stopped");
	}

	/**
	 * Stops and cleans up the worker.
	 */
	public void stop(){
		log.debug("Stopping TaskListMonitor");
		
		this.running = false;
		http.getConnectionManager().shutdown();
		t.interrupt();
	}
	
	/**
	 * Initialize and start the worker
	 */
	public void start(){
		log.debug("Starting TaskListMonitor");
		
		this.running = true;
		this.http = new DefaultHttpClient();
		t = new Thread(this);
		t.start();
	}

	private boolean login() throws ClientProtocolException, IOException{
		log.debug("Starting login procedure");

		HttpPost post = new HttpPost(url+handlerpath+Constants.AUTHENTICATIONHANDLER);

		List<NameValuePair> params = new ArrayList<NameValuePair>();

		params.add(new BasicNameValuePair("username","root"));
		params.add(new BasicNameValuePair("password","root"));

		UrlEncodedFormEntity entity = new UrlEncodedFormEntity(params,"UTF-8");
		post.setEntity(entity);

		BasicResponseHandler handler = new BasicResponseHandler();

		String response = http.execute(post,handler);
		http.getConnectionManager().closeExpiredConnections();
		
		JsonNode root = new ObjectMapper().readValue(response, JsonNode.class);
				
		if(root.path("success").getBooleanValue()){
			sessionKey = root.path("sessionId").getTextValue();
			log.trace("Login successful. Session key: "+sessionKey);
			return true;
		}else{
			log.fatal("Cannot log in to the iTasks System. The returned message is: "+root.path("error").getValueAsText()+" Daemon is exiting.");
			return false;
		}
	}

	private void checkList() throws ClientProtocolException, IOException{
		log.debug("Requesting Call List");

		String url = this.url+this.handlerpath+Constants.REQUESTLISTHANDLER+"?_session="+this.sessionKey;

		HttpGet get = new HttpGet(url);
		BasicResponseHandler handler = new BasicResponseHandler();

		String response = http.execute(get,handler);
		http.getConnectionManager().closeExpiredConnections();
		
		JsonNode root = new ObjectMapper().readValue(response,JsonNode.class);
		
		if(root.path("success").getValueAsText() != null && !root.path("success").getBooleanValue()){
			if(root.path("session").getValueAsText() != null && !root.path("session").getBooleanValue()){
				log.error(root.path("error").getValueAsText()+" Retrying login in next attempt.");
				this.loggedIn = false;
			}else{
				log.fatal("Unsuccesful response from iTasks-system: "+root.path("error").getValueAsText()+" Exiting daemon.");
				this.stop();
			}
		}else{
			Iterator<JsonNode> elements = root.getElements();

			while(elements.hasNext()){
				JsonNode rpcExecute = elements.next();

				String taskId = rpcExecute.path("taskId").getTextValue();

				if(!callQueue.contains(taskId)){
					RpcInfo execInfo = parseInfo(rpcExecute);
					RemoteServiceWorker worker = startWorker(execInfo);

					log.debug("Stored worker "+taskId);

					if(worker == null){
						log.error("Cannot start worker for task "+taskId+". Unsupported parameters");
					}

					callQueue.store(taskId, worker);
				}			
			}
		}
	}

	private RpcInfo parseInfo(JsonNode exec){
		return new RpcInfo(exec);
	}

	private RemoteServiceWorker startWorker(RpcInfo execInfo){
		RemoteServiceWorker worker = null;

		//At the moment the daemon does not make any difference between the message types.
		if(execInfo.rpcInterface.protocol == RpcProtocol.HTTP){
			worker = new WebServiceWorker(url,handlerpath,sessionKey,execInfo,interval);
		}

		if(execInfo.rpcInterface.protocol == RpcProtocol.System){
			worker = new SystemCallWorker(url,handlerpath,sessionKey,execInfo,interval);
		}
		
		return worker;
	}

	private void garbageCollect(){
		ArrayList<String> markedTasks = new ArrayList<String>();

		for(RemoteServiceWorker worker : callQueue.getContents()){
			if(worker.isFinished()) markedTasks.add(worker.getTaskId());
		}

		for(String taskId : markedTasks){
			callQueue.remove(taskId);
		}	
	}
}
