/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns('itasks');

itasks.NewWorkPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function() {
		Ext.apply(this, {
			title: 'Start new work',
			iconCls: 'icon-newwork',
			bodyStyle: 'padding: 10px 0px 0px 15px;'
		});
		
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
	},
	setApplicationPanel: function (panel) {
		this.applicationPanel = panel;
	},
	refresh: function() {
	
		//Add the session id
		params = this.applicationPanel.addSessionParam({});
		
		//Send the data to the server
		Ext.Ajax.request({
			url: 'handlers/new/list',
			method: "GET",
			params: params,
			scripts: false,
			callback: this.processList,
			scope: this
		});
	},
	processList: function (el,success,response,options) {
		if(success) {
			var data = Ext.decode(response.responseText);

			//Check for session errors.
			this.applicationPanel.checkSessionResponse(data);

			//Build the html
			var list = this.body.createChild({tag: 'ul', cls: 'newwork'});
			
			//var html = "";
			
			var num = data.length;
			for(var i = 0; i < num; i++) {
			
				var label = data[i].label;
				var icon = data[i].icon;
				
				var li = list.createChild({tag: 'li', cls : 'icon-' + icon, children: [
						{tag: 'a', href: '#', html: label}
					]});
				
				//Attach click handler
				li.on('click', function(el,evt,options) {
					this.startWork(options.flowLabel);
					
				},this,{flowLabel : label});
			}
		}
	},
	
	startWork: function (workflow) {
		
		var params = {workflow: workflow};
		
		//Add the session id
		params = this.applicationPanel.addSessionParam(params);
		
		//Send the data to the server
		Ext.Ajax.request({
			url: 'handlers/new/start',
			method: "GET",
			params: params,
			scripts: false,
			callback: this.processStart,
			scope: this
		});
	},
	processStart: function (el,success,response,options) {
		if(success) {
			var data = Ext.decode(response.responseText);
			
			//Check for session errors.
			this.applicationPanel.checkSessionResponse(data);

			//Fire event
			this.fireEvent('processStarted',data.taskid);
		}
	}
});

Ext.reg('itasks.nwpanel', itasks.NewWorkPanel);