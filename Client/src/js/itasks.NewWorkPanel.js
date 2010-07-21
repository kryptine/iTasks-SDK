/**
* Panel for starting new work, used to initiate new work flows
*/
Ext.ns("itasks");

itasks.NewWorkTreeLoader = Ext.extend(Ext.tree.TreeLoader, {
	load: function (node, callback, scope) {
		this.dataUrl = "/services/json/workflows" + (node.id == "_ROOT_" ? "" : "/" + node.id);
		itasks.NewWorkTreeLoader.superclass.load.apply(this,arguments);
	},
	processResponse: function (response, node, callback, scope) {
		try {
			var o = Ext.decode(response.responseText);
			node.beginUpdate();
			for(var i = 0, len = o.workflows.length; i < len; i++ ) {
				var wf = o.workflows[i];
				var n = this.createNode({ id : wf.name
										, text: wf.label
										, iconCls: wf.folder ? "icon-folder" : "icon-workflow"
										, leaf: ! wf.folder
										, singleClickExpand: true
										});
				if(n) {
					node.appendChild(n);
				}
			}
			node.endUpdate();
			this.runCallback(callback, scope || node, [node]);
		} catch(e) {
			handleFailure(response);
		}
	}
});

itasks.NewWorkPanel = Ext.extend(Ext.tree.TreePanel ,{

	initComponent: function() {

		Ext.apply(this, {
			title: "New task...",
			iconCls: "icon-newwork",	
			loader: new itasks.NewWorkTreeLoader({
				baseParams: {_session: itasks.app.session}
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
			url: "/services/json/tasks/create",
			params: {_session: itasks.app.session, workflow: workflow},
			scripts: false,
			callback: this.startWorkflowCB,
			scope: this
		});
	},
	startWorkflowCB: function(el, success, response, options){
		try {
			var o = Ext.decode(response.responseText);
			this.fireEvent("processStarted",o.taskId);
		} catch(SyntaxError) {}
	}
});

Ext.reg("itasks.nwpanel", itasks.NewWorkPanel);