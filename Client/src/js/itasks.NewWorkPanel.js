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
										, description: wf.description
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

itasks.NewWorkPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {
		
		var startButton = new Ext.Button({ xtype: 'button'
									, text: 'Start task'
									, iconCls: 'icon-play'
									, disabled: true
									});
	
		var wtpanel = new itasks.WorkTreePanel();
		var wdpanel = new itasks.WorkDescriptionPanel({bbar: ['->',startButton]});
	
		Ext.apply(this, {
			title: "New task",
			iconCls: "icon-newwork",
			layout: "border",
			border: false,
			items: [wtpanel,wdpanel]
		});
		
		itasks.NewWorkPanel.superclass.initComponent.apply(this,arguments);
		
		wtpanel.on("click", function(node,event){
			if(node.leaf){
				wdpanel.update("<h3>"+node.attributes.text+"</h3>"+node.attributes.description);
				startButton.enable();
			}		
		},this);
		
		wtpanel.on("dblclick", function (node, event) {
			if(node.leaf) {
				this.startWorkflow(node.id);
			}
		},this);
		
		startButton.on("click", function(button, event){
			var node = wtpanel.getSelectionModel().getSelectedNode();
			
			if(node.leaf){
				this.startWorkflow(node.id);
			}			
		},this);
	},	
	startWorkflow: function (workflow) {
	
		Ext.Ajax.request({
			method: "POST",
			url: "/services/json/tasks/create",
			params: {session: itasks.app.session, workflow: workflow},
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

itasks.WorkTreePanel = Ext.extend(Ext.tree.TreePanel ,{

	initComponent: function() {

		Ext.apply(this, {
			region: "center",
			id: 'newpanel',
			header: false,
			loader: new itasks.NewWorkTreeLoader({
				baseParams: {session: itasks.app.session}
			}),
			root: {text: "_ROOT_", nodeType: "async", id: "_ROOT_", expanded: true},
			rootVisible: false,
			cls: "newtask-container",
			bodyStyle: "border-top: none" //HACK: remove ugly double border at the top
		});
		itasks.WorkTreePanel.superclass.initComponent.apply(this,arguments);
	}
});

itasks.WorkDescriptionPanel = Ext.extend(Ext.Panel, {
	
	initComponent: function(){
		Ext.apply(this,{
			title: "Task description",
			cls: "newtask-description-container",
			iconCls: "icon-description",
			region: "south",
			bodyStyle: "padding: 4px",
			split: true,
			autoScroll: true,
			height: 150,
			html: ""
		});
		
		itasks.WorkDescriptionPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.nwpanel", itasks.NewWorkPanel);