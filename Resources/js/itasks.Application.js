/**
* Main application
*/
Ext.ns('itasks');

itasks.Application = function () {
	return {
		viewport: new Ext.Viewport({
			layout: 'card',
			activeItem: 0,
			layoutConfig: {
				deferredRender: false
			},
			items: {
				baseCls: 'bg',
				xtype: 'panel'
			}	
		}),
		
		/**
		* Starts the client GUI framework
		*/
		start: function(errorMsg) {
			//Create the login window
			var loginWindow = new itasks.LoginWindow({errorMsg: errorMsg});
			var startPanel = this.viewport.getComponent(0);
			
			startPanel.add(loginWindow);
			
			loginWindow.continuation = this.loadUserInterface.createDelegate(this);
			loginWindow.show();
		},	
		/**
		* Loads and builds the GUI
		*/	
		loadUserInterface: function(displayName, sessionId) {
			
			//Remove the login window
			var startPanel = this.viewport.getComponent(0);
			var loginWindow = startPanel.getComponent(0);
			
			loginWindow.hide();
			loginWindow.destroy();
		
			startPanel.remove(loginWindow);
	
			//Create the loader window
			var loaderWindow = new itasks.LoaderWindow();
			startPanel.add(loaderWindow);
			
			loaderWindow.continuation = this.startUserInterface.createDelegate(this);			
			loaderWindow.show();
			
			//Start building the GUI
			loaderWindow.updateProgress(0.2,'Building User Interface...');
	
			this.gui = new itasks.ApplicationPanel({application: this, displayName: displayName, sessionId: sessionId});
			this.viewport.add(this.gui);
			this.viewport.doLayout();
				
			loaderWindow.updateProgress(0.6,'Initializing User Interface...');
			this.gui.init();
			
			//Finish the loader
			loaderWindow.updateProgress(1.0,'Done.');
			loaderWindow.finish();
		},
		/**
		* Starts the main interface
		*/
		startUserInterface: function() {
			var startPanel = this.viewport.getComponent(0);
			var loaderWindow = startPanel.getComponent(0);
			
			//Remove the loader window and startpanel
			loaderWindow.hide();
			loaderWindow.destroy();
			
			startPanel.remove(loaderWindow);
			
			//Switch to the main interface
			this.viewport.layout.setActiveItem(1);
			//Remove start panel
			this.viewport.remove(0);
		},
		/**
		* Resets the main viewport to show the start screen
		*/
		reset: function() {
			this.viewport.remove(0);
			this.viewport.add(new Ext.Panel({baseCls: 'bg'}));
			this.viewport.layout.setActiveItem(0);
		},
		restart: function (errorMsg) {
			this.reset();
			this.start(errorMsg);
		}
	}
};