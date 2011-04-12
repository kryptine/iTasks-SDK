/**
* Shared base class for panels that reflect a remote server resource.
*/
Ext.ns("itasks");

itasks.RemoteDataPanel = Ext.extend(Ext.Panel, {

	url: undefined,
	timestamp: undefined,
	params: {},
	busy: false,
	
	initComponent: function() {		
		itasks.RemoteDataPanel.superclass.initComponent.apply(this,arguments);
		this.addEvents("remoteCallStart","remoteCallEnd");
	},
	
	/*
	* Requests a remote resource
	*/
	remoteCall: function(url,params,callback,timestamp,incremental) {
		if(this.busy)
			return;
			
		this.busy = true;
		this.fireEvent("remoteCallStart");
		
		//Add session id parameter
		params["session"] = itasks.app.session
		
		//Send timestamp for incremental refresh
		if(incremental) {
			params["timestamp"] = timestamp;
		} else {
			delete(params["timestamp"]);
		}
		
		Ext.Ajax.request({
			method: 'POST',
			url: url,
			params: params,
			scripts: false,
			callback: function (options,success,response) {
				if(success) {
					var data;
					try {
						data = Ext.decode(response.responseText);
					} catch(SyntaxError) {
						data = response.responseText;
					}
					if(typeof data == 'object') {
						//Check for (session) errors
						if(data.error) {
							itasks.app.restart(data.error);
							return;
						}
						callback.call(this,data,true,response.status);
					} else {
						callback.call(this,response.responseText,true,response.status);
					}					
				} else {
					callback.call(this,response.statusText,false,response.status);
				}
				this.busy = false;
				this.fireEvent("remoteCallEnd");
			},
			scope: this
		});
	},
	/*
	* Refreshes this panel to update the remote information
	*/
	refresh: function(incremental) {
		this.remoteCall(this.url,this.params,this.update,this.timestamp,incremental);	
	},
	/*
	* This method must be implemented to handle a refresh event
	*/
	update: function(data,success) {
	}
});

Ext.reg('itasks.remotedata',itasks.RemoteDataPanel);