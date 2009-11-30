package nl.ru.icis.mbsd.itasks.rpcd;

import java.io.IOException;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.GnuParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Layout;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.apache.log4j.RollingFileAppender;

/**
 * Launcher Class
 * @author Erik Crombag
 */

public class Launcher {

	/**
	 * This daemon handles the operation of RPC-tasks in the iTasks-system. 
	 * @param args <br>Command line arguments:
	 * <li> <b>-i &#60;amount&#62;</b> Set the interval in ms
	 * <li> <b>-u &#60;url&#62;</b> Url to the iTasks System
	 * <li> <b>-h &#60;path&#62;</b> Path the the handlers
	 * <li> <b>-v</b> Verbose output
	 * <li> <b>-d</b> Debug Level (0 (nothing), 1 (fatal) - 6 (trace))
	 */
		
	@SuppressWarnings("static-access")
	public static void main(String[] args) {
		
		//Using the Apache Commons command line parser
		Options options = new Options();
		
		//Add the various command line options		
		options.addOption(
				OptionBuilder.withLongOpt("interval")
							 .withDescription("The amount of milliseconds between each poll of the iTask-server. Default: 10000")
							 .hasArg()
							 .withArgName("INTERVAL")
							 .create("i")
		);
		
		options.addOption(
				OptionBuilder.withLongOpt("url")
							 .withDescription("URL of the server on which the iTasks-system is residing. Default: http://localhost:8080")
							 .hasArg()
							 .withArgName("URL:[PORT]")
							 .create("u")
		);
		
		options.addOption(
				OptionBuilder.withLongOpt("handler")
							 .withDescription("Path to the handler collection. Default: /handler")
							 .hasArg()
							 .withArgName("PATH")
							 .create("p")
		);
		
		options.addOption(
				OptionBuilder.withLongOpt("debug")
							 .withDescription("Debug Level. Default: 2")
							 .hasArg()
							 .withArgName("LEVEL")
							 .create("d")
		);	
		
		options.addOption(
				OptionBuilder.withDescription("Factor in which the interval get's lengthened in case of a non responsive server. Default: 5")
							 .hasArg()
							 .withArgName("FACTOR")
							 .create("slowdown")
		);	
		
		options.addOption(
				OptionBuilder.withDescription("Maximum amounts of retries before the daemon quits. Default: 10")
							 .hasArg()
							 .withArgName("RETRIES")
							 .create("maxattempts")
		);
		
		options.addOption(
				OptionBuilder.withDescription("Maximum amounts of retries before the daemon quits. Default: 10")
							 .hasArg()
							 .withArgName("RETRIES")
							 .create("maxattempts")
		);	
				
		options.addOption("v","verbose",false,"Verbose Output");		
		
		options.addOption(
				OptionBuilder.withLongOpt("help")
							 .withDescription("Shows this help statement")
							 .create("h")
		);
		
		//Parse the command line
		CommandLineParser parser = new GnuParser();
		
		//Start logging messages
		Layout layout = new PatternLayout("%d (%F:%L) [%-5p] %c - %m%n");
		Logger rootlog = Logger.getRootLogger();
		
		//Add file handler
		try {
						
			RollingFileAppender file = new RollingFileAppender(layout,"log/rpcdaemon.log",true);
			file.setMaxBackupIndex(20);
			file.setMaxFileSize("1MB");
		
			rootlog.addAppender(file);
		} catch (IOException e1) {
			System.err.println("Cannot start logger. Exiting daemon.");
			System.exit(1);
		}
		
		
		try {
			int interval = Constants.POLLINTERVAL;
			String url = Constants.DEFAULTURL;
			String handler = Constants.DEFAULTHANDLERPATH;
			
			//Parse the current command line
			CommandLine cl = parser.parse(options, args);
			
			//Polling interval
			if(cl.hasOption("i")){
				interval = new Integer(cl.getOptionValue("i")).intValue();
			}
			
			//iTasks URL
			if(cl.hasOption("u")){
				url = cl.getOptionValue("u");
			}
			
			//handler Path
			if(cl.hasOption("h")){
				handler = cl.getOptionValue("h");
			}
			
			//Verbose output
			if(cl.hasOption("v")){
				rootlog.addAppender(new ConsoleAppender(layout,ConsoleAppender.SYSTEM_ERR));
			}
			
			//Debug level
			if(cl.hasOption("d")){
				int lvl = new Integer(cl.getOptionValue("d")).intValue();
				
				switch(lvl){
				case 0:
					rootlog.setLevel(Level.OFF);
					break;
				case 1:
					rootlog.setLevel(Level.FATAL);
					break;
				case 2:
					rootlog.setLevel(Level.ERROR);
					break;
				case 3:
					rootlog.setLevel(Level.WARN);
					break;
				case 4:
					rootlog.setLevel(Level.INFO);
					break;
				case 5:
					rootlog.setLevel(Level.DEBUG);
					break;
				case 6:
					rootlog.setLevel(Level.TRACE);
					break;
				default:
					rootlog.setLevel(Level.ALL);
					break;
				}
				
			}else{
				rootlog.setLevel(Level.ERROR);
			}
			
			//Slowdown factor
			if(cl.hasOption("slowdown")){
				Constants.SLOWDOWN = new Integer(cl.getOptionValue("slowdown")).intValue();
			}
			
			//Maximum of retries
			if(cl.hasOption("slowdown")){
				Constants.MAXATTEMPTS = new Integer(cl.getOptionValue("maxattempts")).intValue();
			}
			
			//Show either the help statement or..
			if(cl.hasOption("help")){
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp("java -jar rpcd.jar",options);
				System.exit(0);
			}
			
			//..start the main daemon class
			new Rpcd(url,handler,interval);
			
			
		} catch (Exception e) {
			rootlog.error("Exception while initializing daemon. "+e.getLocalizedMessage());
			e.printStackTrace();
		}
	}

}
