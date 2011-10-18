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
        oryxState: 'unloaded',
        waitingForOryx: new Ext.util.MixedCollection(true),
		//scrollbarWidth: itasks.util.getScrollerWidth(),
		
		viewport: new Ext.Viewport({
			layout: "fit",
			items: [{xtype: 'itasks.tui.panel', forceLayout: true}]
		}),
		
		/**
		* Starts the client GUI framework
		*/
		start: function(errorMsg) {
			//Store message
			this.errorMsg = errorMsg;
			
			//Reset session & timestamp
			delete(this.sessionId);
			delete(this.lastSync);
			
			//Set cookie provider
			Ext.state.Manager.setProvider(new Ext.state.CookieProvider({
				expires: new Date(new Date().getTime()+(1000*60*60*24*30)), // 30 days from now
			}));
			
			//Load the initial GUI
			//this.pollServer();
		},
		pollServer: function() {
			var params = {};
			if(this.sessionId)
				params['session'] = this.sessionId;
			if(this.lastSync)
				params['timestamp'] = this.lastSync;	
		
			Ext.Ajax.request({
				url: '?show=gui',
				method: 'POST',
				params: params,	
				scripts: false,
				callback: this.processServerMessage,
				scope: this });
		},
		processServerMessage: function(options,success,response) {
			var message;
			
			try {
				message = Ext.decode(response.responseText);
			} catch(SyntaxError) {
				return;
			}
			if(typeof message != 'object') {
				return;
			}
		
			//this.viewport.items.get(0).update(message);
		},
		refreshGUI: function() {
		
		},
		restart: function (errorMsg) {
			this.start(errorMsg);
		},
		refresher: {
			run: function() {
				itasks.app.refreshGUI();
			},
			interval: 5000
		},
		startAutoRefresh: function() {
			Ext.TaskMgr.start(this.refresher);
		},
		stopAutoRefresh: function() {
			Ext.TaskMgr.stop(this.refresher);
		}
	}
};