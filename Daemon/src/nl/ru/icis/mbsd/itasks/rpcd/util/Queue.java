package nl.ru.icis.mbsd.itasks.rpcd.util;

import java.util.HashMap;

import nl.ru.icis.mbsd.itasks.rpcd.worker.RemoteServiceWorker;

/**
 * Queue utility Class for storing the active worker threads.
 * @author Erik Crombag
 *
 */
public class Queue {

	private HashMap<String,RemoteServiceWorker> store;
	
	/**
	 * Default constructor
	 */
	public Queue(){
		store = new HashMap<String,RemoteServiceWorker>();
	}
	
	/**
	 * Store a RemoteServiceWorker in the Queue
	 * @param key The Task Id of the worker
	 * @param value The worker class itself
	 */
	public void store(String key, RemoteServiceWorker value){
		store.put(key, value);
	}
	
	/**
	 * Remove a RemoteServiceWorker
	 * @param key The Task Id to be removed
	 */
	public void remove(String key){
		store.remove(key);
	}
	
	/**
	 * Retrieve a RemoteServiceWorker
	 * @param key The Task Id to be retrieved
	 * @return The RemoteServiceWorker or null if not found.
	 */
	public RemoteServiceWorker retrieve(String key){
		return store.get(key);
	}
	
	/**
	 * Whether a worker with a specific task id resides in the Queue.
	 * @param key The Task Id
	 * @return True if the Task Id exists in the Queue.
	 */
	public boolean contains(String key){
		return store.containsKey(key);
	}
	
	/**
	 * Generate an Array of the contents of the Queue
	 * @return An array of RemoteServiceWorkers
	 */
	public RemoteServiceWorker[] getContents(){
		return store.values().toArray(new RemoteServiceWorker[store.size()]);
	}
}
