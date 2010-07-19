/**
* Main application
*/
Ext.ns('itasks');

itasks.Application = function () {
	return {
		//Application-wide state
		session: null,
		displayName: null,
		googleMapsState: 'unloaded',
		waitingForGoogleMaps: new Ext.util.MixedCollection(true),
		scrollbarWidth: 0,
		
		viewport: new Ext.Viewport({
			layout: 'card',
			activeItem: 0,
			unstyled: true,
			items: {
				baseCls: 'bg',
				xtype: 'panel'
			}	
		}),
		
		loginWindow: null,
		mainGui: null,
		/**
		* Starts the client GUI framework
		*/
		start: function(errorMsg) {
			//Store message
			this.errorMsg = errorMsg;
			
			//Load the config
			this.loadConfig();
			
			//Set cookie provider
			Ext.state.Manager.setProvider(new Ext.state.CookieProvider({
				expires: new Date(new Date().getTime()+(1000*60*60*24*30)), // 30 days from now
			}));
		},
		loadConfig: function() {
			Ext.Ajax.request({url:"config.json",success: this.continueConfig, scope: this});
		},
		continueConfig: function(response) {
			//Globally store config
			itasks.config = Ext.decode(response.responseText);
			
			//Load skin
			this.loadSkin();
			
			//Create the login window
			if(!this.loginWindow) {
				this.loginWindow = new itasks.LoginWindow({
					errorMsg: this.errorMsg,
					continuation:  this.loadUserInterface.createDelegate(this)
					});
				this.viewport.getComponent(0).add(this.loginWindow);
			} else {
				this.loginWindow.setError(this.errorMsg);
			}
			
			this.loginWindow.show();
		},
		loadSkin: function() {
			if(this.skinLoaded) return;
			
			var link = document.createElement("link");
			link.rel = "stylesheet";
			link.type = "text/css";
			link.href = "skins/" + itasks.config.skin + "/main.css";
			
			document.body.appendChild(link);
			document.title = itasks.config.appTitle;
			
			this.skinLoaded = true;
		},	
		/**
		* Loads and builds the GUI
		*/	
		loadUserInterface: function(session) {
			
			//Update global state
			this.session = session.sessionId;
			this.displayName = itasks.util.formatUser(session.user);
			this.scrollbarWidth = itasks.util.getScrollerWidth();
			
			//Remove the login window
			this.loginWindow.hide();
		
			var startPanel = this.viewport.getComponent(0);
		
			this.mainGui = new itasks.ApplicationPanel();
			
			this.viewport.add(this.mainGui);
			this.viewport.layout.setActiveItem(1);
			this.viewport.doLayout();
		},
		/**
		* Resets the main viewport to show the start screen
		*/
		reset: function() {
			this.viewport.layout.setActiveItem(0);
			this.viewport.remove(1,true);
		},
		restart: function (errorMsg) {
			this.reset();
			this.start(errorMsg);
		}
	}
};