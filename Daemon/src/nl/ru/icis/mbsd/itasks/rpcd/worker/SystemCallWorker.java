package nl.ru.icis.mbsd.itasks.rpcd.worker;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo;
import nl.ru.icis.mbsd.itasks.rpcd.json.RpcInfo.RpcParameterValue;

import org.apache.log4j.Logger;

/**
 * Does a system call on the system. 
 * The system call gets formated using the following convention:
 * <li> The call is made if the protocol-tag in the {@link RpcInfo} equals 'System'
 * <li> The executable or script called is specified in the location-tag
 * <li> Parameters with a name of 1 character are prefixed with a '-'
 * <li> Parameters with a name of more characters are prefixed with '--'
 * <li> Parameters with no name are not prefixed. Only their value is displayed.
 * @author Erik Crombag
 */
public class SystemCallWorker extends RemoteServiceWorker {

	private boolean done = false;
	private Process proc;

	/**
	 * Default constructor
	 * @see RemoteServiceWorker
	 */
	public SystemCallWorker(String url, String handlerpath, String session, RpcInfo execInfo, int interval){
		super(url,handlerpath,session,execInfo, interval);

		log = Logger.getLogger("rpcd.remoteserviceworker.systemcallworker");

		t = new Thread(this);
		t.start();
		
	}

	@Override
	public boolean isFinished() {
		return done;
	}

	@Override
	public void stop() {
		t.interrupt();
	}

	@Override
	public void run() {		
		Runtime rt = Runtime.getRuntime();

		String operation = execInfo.operation.location;

		for(RpcParameterValue param : execInfo.paramValues){
			if(param.name.length() == 0){
				if(param.serializedValue.length() > 0){
					operation += " "+param.serializedValue;
				}
			}else if(param.name.length() == 1){
				operation +=" -"+param.name;
				if(param.serializedValue.length() > 0){
					operation += " "+param.serializedValue;
				}
			}else{
				operation += " --"+param.name;
				if(param.serializedValue.length() > 0){
					operation += " "+param.serializedValue;
				}
			}			
		}
		try {
			sendStatus("Started "+operation);
			
			try {
				
				log.debug("("+execInfo.taskId+") Executing system call: "+operation);

				proc = rt.exec(operation);
				proc.waitFor();

				InputStream in = proc.getInputStream();
				
				ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
				byte[] tmp = new byte[1024];

				while(true){
					int r = in.read(tmp);
					if(r == -1) break;
					out.write(tmp,0,r);
				}

				sendResult(out.toByteArray());	
				
			} catch (IOException e) {
				log.error("("+execInfo.taskId+") IO Exception while executing system call",e);
				proc.destroy();
				sendError("("+execInfo.taskId+") IO Exception while execucting system call: "+e.getLocalizedMessage());
				
			} catch (InterruptedException e) {
				log.error("("+execInfo.taskId+") Process interrupted during execution. No result",e);
				proc.destroy();
				sendError("("+execInfo.taskId+") Process interrupted during execution. No result");
			}
		} catch(Exception e){
			log.fatal("Cannot send message to iTask-system for task "+execInfo.taskId,e);
		}
		
		done = true;
	}
}
