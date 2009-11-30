package nl.ru.icis.mbsd.itasks.rpcd;
/**
 * Some default Constants.
 * @author Erik Crombag
 *
 */
public class Constants {
		
	/**
	 * The default interval between different polls in milliseconds.
	 * It can be set using the -i option on the command line. Default: 10000
	 */
	public static final int POLLINTERVAL = 10000;
	
	/**
	 * The default URL where the iTasks system is residing. It can be
	 * set using the -u option on the command line. Default: http://localhost:8080
	 */
	public static final String DEFAULTURL = "http://localhost:8080";
	
	/**
	 * The default path to the handlers. It can be set using the -h option on the 
	 * command line. Default:/handlers
	 */
	public static final String DEFAULTHANDLERPATH = "/handlers";
	
	/**
	 * The Request Rpc-tasks Handler
	 */
	public static final String REQUESTLISTHANDLER = "/rpc/request";
	
	/**
	 * The Rpc Response handler
	 */
	public static final String RESPONSEHANDLER = "/rpc/response";
	
	/**
	 * The authentication handler
	 */
	public static final String AUTHENTICATIONHANDLER = "/authenticate";
	
	/**
	 * The maximum amount of retries.
	 */
	public static int MAXATTEMPTS = 10;
	
	/**
	 * Factor of in which the wait get's lengthened in case of a retry.
	 */
	public static int SLOWDOWN = 5;
}
