/**
* Shared base class for panels that reflect a remote server resource.
*/
Ext.ns("itasks");

itasks.RemoteDataPanel = Ext.extend(Ext.Panel, {

	url: undefined,
	params: {},
	busy: false,
	
	initComponent: function() {		
		itasks.RemoteDataPanel.superclass.initComponent.apply(this,arguments);
		this.addEvents("remoteCallStart","remoteCallEnd");
	},
	
	/*
	* Requests a remote resource
	*/
	remoteCall: function(url,params,callback) {
		if(this.busy)
			return;
			
		this.busy = true;
		this.fireEvent("remoteCallStart");
		
		//Add session id parameter
		params["session"] = itasks.app.session
		
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
						callback.call(this,data);
					} else {
						callback.call(this,response.responseText);
					}
					
				} else {
					var win = new Ext.Window({
						title: "Error",
						html: response.statusText,
						width: 200,
						height: 100,
						modal: true,
						closable: true,
						bodyStyle: "padding: 5px",
						buttons: [{
							text: "Ok",
							handler: function() {win.close();}
						}],
						buttonAlign: "center"
					});
					win.show();
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
	refresh: function() {
		this.remoteCall(this.url,this.params,this.update);	
	},
	/*
	* This method must be implemented to handle a refresh event
	*/
	update: function(data) {
	}
});

Ext.reg('itasks.remotedata',itasks.RemoteDataPanel);