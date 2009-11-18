package nl.ru.icis.mbsd.itasks.rpcd;

import nl.ru.icis.mbsd.itasks.rpcd.worker.TaskListMonitor;

import org.apache.log4j.Logger;

/**
 * This is the main class of the daemon. In this class all different
 * sub-classes are initialized and started.
 * @author Erik Crombag
 *
 */
public class Rpcd {

	private TaskListMonitor monitor;
	private Logger log;
	
	/**
	 * The default constructor	
	 * @param url The URL on which the iTasks-system is residing.
	 * @param handlerPath the path on the server to the handlers.
	 * @param interval The interval on which the iTasks system is polled for new RP-calls.
	 */
	public Rpcd(String url, String handlerPath, int interval){
		log = Logger.getLogger("rpcd");		
		log.info("Starting RPC daemon");
		
		//Start the main monitor routine
		monitor = new TaskListMonitor(url,handlerPath, interval);
	
		//Add shutdown hook for stopping all threads in case of a shutdown.
		Runtime.getRuntime().addShutdownHook(new Thread(){
			public void run(){
				monitor.stop();
				log.info("RPC Daemon has stopped");			
			}
		});
	}	
}
