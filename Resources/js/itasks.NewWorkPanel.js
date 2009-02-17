/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns('itasks');

itasks.NewWorkPanel = Ext.extend(itasks.RemoteDataPanel, {

	initComponent: function() {
		Ext.apply(this, {
			title: 'Start new work',
			iconCls: 'icon-newwork',
			url: 'handlers/new/list',
			bodyStyle: 'padding: 10px 0px 0px 15px;'
		});
		
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
	},
	update: function (data) {
		//Build the html
		var list = this.body.createChild({tag: 'ul', cls: 'newwork'});		
		var num = data.length;
		
		for(var i = 0; i < num; i++) {
		
			var name = data[i].name;
			var label = data[i].label;
			var icon = data[i].icon;
			
			var li = list.createChild({tag: 'li', cls : 'icon-' + icon, children: [
					{tag: 'a', href: '#', html: label}
				]});
			
			//Attach click handler
			li.on('click', function(el,evt,options) {
				this.startWork(options.flowName);
				
			},this,{flowName : name});
		}
	},
	startWork: function (workflow) {
		this.remoteCall('handlers/new/start',{workflow: workflow},this.startWorkCallback);
	},
	startWorkCallback: function(data){
		//Fire event
		this.fireEvent('processStarted',data.taskid);
	}
});

Ext.reg('itasks.nwpanel', itasks.NewWorkPanel);