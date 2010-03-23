/**
* Tab panel which shows the global task forest.
*/

Ext.ns("itasks");

itasks.TaskForestTabPanel = Ext.extend( Ext.Panel, {
	
	initComponent: function () {
				
		this.tree = new Ext.ux.tree.TreeGrid({
			id : 'treepanel',
			region : 'center',
			
			rootVisible: false,
			autoScroll: true,
			title: "Task Overview",

			border: false,
			bodyStyle: "background-color: white",
			
			iconCls : "icon-task-tree",
			
			columns:[{
						header:'Task Id',
						dataIndex:'taskId',
						width: 500
					},{
						header:'Assigned to..',
						dataIndex:'user',
						renderer: Ext.util.Format.htmlEncode,
						width: 100
					},{
						header:'Task Label',
						dataIndex:'taskLabel',
						width: 200
					},{
						header:'Trace Value',
						dataIndex: 'traceValue',
						width: 200
					}],	
			
			loader: new Ext.tree.TreeLoader({
						dataUrl: itasks.config.serverUrl + "/debug/taskforest",
						baseParams: {_session: itasks.app.session},
						requestMethod: "POST",
						uiProviders: { col : Ext.ux.tree.TreeGridNodeUI}
					}),	
			
			root: new Ext.tree.AsyncTreeNode({
					text:'Tasks'
				})			
		});
		
		this.iconPanel = new Ext.Panel({
			id: 'iconpanel', 
			region: 'south',
			height: 20,
			border: false,
			
			html: '<div class="taskForestLegend task-mnt">Main Task</div>'+
			      '<div class="taskForestLegend task-int">Edit Task</div>'+
				  '<div class="taskForestLegend task-mon">Monitor Task</div>'+
				  '<div class="taskForestLegend task-fin">Finished Task</div>'+
				  '<div class="taskForestLegend task-seq">Sequential Combinator</div>'+
				  '<div class="taskForestLegend task-grp">Grouping Combinator</div>'+
				  '<div class="taskForestLegend task-par">Parallel Combinator</div>'+
				  '<div class="taskForestLegend task-rpc">Remote Procedure Call</div>'+
				  '<div class="x-clear"></div>'
		
		});
						
		Ext.apply(this, {
			
			title: "Task forest",
			closable: true,
			autoScroll: false,
			
			cls: "worktab-container",
			iconCls: "icon-task-tree",
			
			layout: "border",
			bodyStyle: "background-color: white",
			
			items: [this.tree, this.iconPanel],
			
			tbar: [{
					text: "Refresh",
					iconCls: "x-tbar-loading",
					listeners: {click: {fn: function (btn) {this.refresh();},scope: this}}
				  },{
				    text: "Expand Tree",
					iconCls: "icon-expand-tree",
					listeners: {click: {fn: function (btn) {this.expandTree();},scope: this}}
				  },{
					text: "Collapse Tree",
					iconCls: "icon-collapse-tree",
					listeners: {click: {fn: function (btn) {this.collapseTree();},scope: this}}
				  }]			
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	},
	
	refresh: function(){
		this.tree.getRootNode().reload();
	},
	
	expandTree: function(){
		var rootNode = this.tree.getRootNode();
		var toggleChild = function(child){child.expand(true,true);};
		rootNode.eachChild(toggleChild);
	},
	
	collapseTree: function(){
		var rootNode = this.tree.getRootNode();
		var toggleChild = function(child){child.collapse(true,false);};
		rootNode.eachChild(toggleChild);
	}
});

Ext.reg("itasks.taskforesttab",itasks.TaskForestTabPanel);