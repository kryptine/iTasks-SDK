/**
* Main application
*/
Ext.ns('itasks');

itasks.Application = function () {

	return {
		//ATTRIBUTES
		viewport: new Ext.Viewport({
			layout: 'fit',
			items: {
				baseCls: 'bg',
				xtype: 'panel'
			}	
		}),
		
		//METHODS
		start: function() {
			//Create the login window
			var loginWindow = new itasks.LoginWindow();
			var startPanel = this.viewport.getComponent(0);
			
			startPanel.add(loginWindow);
			
			loginWindow.continuation = this.loadUserInterface.createDelegate(this);
			loginWindow.show();
		},		
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
	
			this.gui = new itasks.ApplicationPanel({displayName: displayName, sessionId: sessionId});
			
			loaderWindow.updateProgress(0.6,'Initializing User Interface...');
			this.gui.init();
			
			//Finish the loader
			loaderWindow.updateProgress(1.0,'Done.');
			loaderWindow.finish();
		},
		startUserInterface: function() {
			var startPanel = this.viewport.getComponent(0);
			var loaderWindow = startPanel.getComponent(0);
			
			//Remove the loader window and startpanel
			loaderWindow.hide();
			loaderWindow.destroy();
			
			startPanel.remove(loaderWindow);
			
			this.viewport.remove(startPanel);
			this.viewport.add(this.gui);
			
			this.gui.show();
			this.viewport.doLayout();
		}	
	}
};