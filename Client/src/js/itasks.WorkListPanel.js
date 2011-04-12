/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.Panel,{ 
	initComponent: function(){

		this.treeGrid = new Ext.ux.tree.TreeGrid({
			rootVisible: false,
			border: false,
			columns: [
				{id: 'subject', header: 'Subject', dataIndex: 'subject', width: 300},
				{id: 'priority', header: 'Priority', dataIndex: 'priority', width: 100},
				{id: 'progress', header: 'Progress', dataIndex: 'progress', width: 100},
				{id: 'manager', header: 'Managed by', dataIndex: 'manager', width: 150},
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', width: 120},
				{id: 'deadline', header: 'Deadline', dataIndex: 'deadline', width: 100}
			],
			root: new Ext.tree.TreeNode({
				text: 'Empty'
			})
		});
	
		Ext.apply(this,{
			unstyled: true,
			layout: 'fit',
			items: [this.treeGrid],	
			tbar: [{
				id: 'refreshbutton',
				xtype: 'tbbutton',
				text: 'Refresh worklist',
				iconCls: 'x-tbar-loading',
				listeners: {
					click: { 
						scope: this,
						fn: function (btn) {
							this.refresh();
						}
					}
				}	
			},'->',{
				id: 'userdisplay',
				xtype: 'label',
				text: 'Welcome ' + itasks.app.displayName
			},{
				id: 'logoutbutton',
				xtype: 'tbbutton',
				text: 'Logout',
				iconCls: 'icon-logout',
				listeners: {
					click: {
						scope: this,
						fn: function() {
							this.findParentByType(itasks.ApplicationPanel).logout();
						}	
					}
				}
			},{
				id: 'autorefreshbutton',
				xtype: 'tbbutton',
				text: 'Auto refresh',
				enableToggle: true,
				iconCls: 'x-tbar-loading',
				listeners: {
					click: {
						scope: this,
						fn: function(e) {
							if(e.pressed) {
								itasks.app.startAutoRefresh();
							} else {
								itasks.app.stopAutoRefresh();
							}
						}
					}
				}
			}]
		});

		itasks.WorkListPanel.superclass.initComponent.apply(this,arguments);	
		this.addEvents("workListRefreshed");
	},
	
	buildTree : function(data){		
		if(!data) return;
		
		var buildNode = function(d, isLeaf){
			
			var newCls = (d.systemProperties.firstEvent == d.systemProperties.latestEvent)?'new-task-node':''
			
			return new Ext.tree.TreeNode({
				cls: 'worklist-node '+newCls,
				uiProvider: Ext.ux.tree.TreeGridNodeUI,
				leaf: isLeaf,
				iconCls: 'task-int',
				subject: Ext.util.Format.htmlEncode(d.taskProperties.taskDescription.title),
				priority: itasks.util.formatPriority(d.managerProperties.priority),
				progress: itasks.util.formatProgress(d.progress),
				manager: Ext.util.Format.htmlEncode(itasks.util.formatUser(d.systemProperties.manager)),
				timestamp: itasks.util.formatDate(d.systemProperties.issuedAt),
				deadline: itasks.util.formatDeadline(d.managerProperties.deadline),
				taskId: d.systemProperties.taskId
			});
		}
		
		var buildSubTree = function(parent){
		
			var children = [];
			
			for(var i=0; i < treeData.length; i++){
				var d = treeData[i];
				
				if(d.systemProperties.parent == parent){					
					var childNodes = buildSubTree(d.systemProperties.taskId);	
					var node = buildNode(d, (childNodes.length > 0)?false:true);					
					treeData.splice(i,1);
					i--;
					node.appendChild(childNodes);
					children.push(node);
				}
			}	

			return children;		
		};
				
		var treeData = data;
		
		//first build top level (parent == undefined)
		var children = buildSubTree();
		
		//build all other trees
		for(var i=0; i < treeData.length; i++){
			var d = treeData[i];
			var c = buildSubTree(d.taskId);
			var n = buildNode(d, (c.length > 0)?false:true);
			n.appendChild(c);
			children.push(n);
		}
		
		this.treeGrid.getRootNode().removeAll();
		this.treeGrid.getRootNode().appendChild(children);
		this.treeGrid.expandAll();
	},
	
	refresh : function(){	
		var ct = this;
		var conn = new Ext.data.Connection();
		
		conn.request({
			url: itasks.config.serviceUrl + "/json/tasks",
			params: { session : itasks.app.session },
			callback: function(options,success,response) {
				if(!success) itasks.app.restart('Cannot retrieve work list');
				
				var data = Ext.decode(response.responseText);
				if(!data.success) itasks.app.restart(data.error);

				ct.buildTree(data.tasks);
				ct.fireEvent("workListRefreshed", ct);
			}
		});
	},
	
	markRead : function(taskId){
				
		var f = function(){
			if(this.attributes.taskId == taskId) this.ui.removeClass('new-task-node');
			return true;
		}
		
		this.treeGrid.getRootNode().cascade(f);
	}
});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
