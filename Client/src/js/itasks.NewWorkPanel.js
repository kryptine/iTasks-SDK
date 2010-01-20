/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns("itasks");

itasks.NewWorkPanel = Ext.extend(Ext.tree.TreePanel ,{

	initComponent: function() {

		Ext.apply(this, {
			title: "New task...",
			iconCls: "icon-newwork",	
			loader: new Ext.tree.TreeLoader({
				dataUrl: itasks.config.serverUrl + "/new/list",
				baseParams: {_session: itasks.app.session},
				requestMethod: "POST"
			}),
			root: {text: "_ROOT_", nodeType: "async", id: "_ROOT_", expanded: true},
			rootVisible: false,
			cls: "newtask-container"
		});
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
		
		this.on("click", function (node, event) {
			if(node.leaf) {
				this.startWorkflow(node.id);
			}
		},this);
	},
	startWorkflow: function (workflow) {
	
		Ext.Ajax.request({
			method: "POST",
			url: itasks.config.serverUrl + "/new/start",
			params: {_session: itasks.app.session, workflow: workflow},
			scripts: false,
			callback: this.startWorkflowCB,
			scope: this
		});
	},
	startWorkflowCB: function(el, success, response, options){
		try {
			var data = Ext.decode(response.responseText);

			this.fireEvent("processStarted",data.taskid);
		} catch(SyntaxError) {}
	}
});

Ext.reg("itasks.nwpanel", itasks.NewWorkPanel);