/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {

		Ext.apply(this, {
			id: 'apppanel',
			layout: 'border',
			hidden: true,
			hideMode: 'offsets',
			unstyled: true,
			items: [{
					id: 'leftpanel',
					xtype: 'itasks.nwpanel',
					region: 'west',
					layoutConfig: {animate: true},
					collapsible: true,
					collapseMode: 'mini',
					split: true,
					border: false,
					deferredRender: false,
					width: 200,
					minWidth: 200,
					maxWidth: 400,
					autoScroll: true
				},{
					id: 'centerpanel',
					region: 'center',
					xtype: 'panel',
					layout: 'border',
					border: false,
					deferredRender: false,
					items: [ {
						id: 'worklist',
						xtype: 'itasks.worklist',
						region: 'north',
						collapsible: true,
						collapseMode: 'mini',
						header: false,
						split: true,
						height: 150
					},{
						id: 'worktabs',
						xtype: 'itasks.worktabs',
						border: false,
						region: 'center'
					}]
				}]
		});
	
		itasks.ApplicationPanel.superclass.initComponent.apply(this, arguments);
	},
	afterRender: function () {
		itasks.ApplicationPanel.superclass.afterRender.apply(this, arguments);
		
		//Initializing the gui...
		var newpanel 	= this.getComponent('leftpanel');
		var worklist 	= this.getComponent('centerpanel').getComponent('worklist');
		var worktabs 	= this.getComponent('centerpanel').getComponent('worktabs');
	
		//Refresh initial overviews
		worklist.refresh();
		
		//Connect event handlers
		var attachTabHandlers = function(tab) {
			if(tab[1]) { //The tab is new
				tab[0].on("taskDone",function(taskid) {
					worklist.refresh();
				});
				tab[0].on("taskRedundant",function(taskid) {
					worklist.refresh();
				});
				tab[0].on("propertyChanged",function(taskid) {
					worklist.refresh();
				});
			}
			tab[0].refresh();
		};
		
		worklist.treeGrid.on("click", function(node,evt) {		
			attachTabHandlers(worktabs.openWorkTab(node.attributes.taskId));			
		});
		
		worklist.on("workListRefreshed",function(worklist) {
			var f = function(node){
				if(!node.attributes.taskId) return true;
				
				var tab = worktabs.getComponent("worktab-"+node.attributes.taskId);
				
				if(tab != null){
					var wlTStamp = node.attributes.latestExtEvent;
					var tTStamp;
					if(tab.properties && tab.properties.systemProperties) {
						tTStamp = tab.properties.systemProperties.latestEvent;
					} else {
						tTStamp = 0;
					}
					if(wlTStamp && tTStamp && wlTStamp > tTStamp) {
						tab.refresh();
					}
				}
				
				return true;
			}

			worklist.treeGrid.getRootNode().cascade(f);
		});
		
		newpanel.on("processStarted",function(taskid) {
			//When new work is started, refresh the worklist
			//and immediately open a tab for the work
			attachTabHandlers(worktabs.openWorkTab(taskid));
			worklist.refresh();
		},this);
		
		//Mark a task as read when it is opened
		worktabs.on("taskOpened",worklist.markRead, worklist);
	
		//Add debug button
		if(itasks.config.debug) {
		
			var tb = worklist.getTopToolbar()
			var button = {
				xtype: "tbbutton",
				text: "Debug...",
				iconCls: "icon-debug",
				listeners: {
					click: {
						fn: function() {
							worktabs.openDebugTab();
						},
						scope: this
					}
				}
			};
			tb.add(button);
		}
	},
	logout: function() {	
		//Send logout request to the server
		Ext.Ajax.request({
			url: "/services/json/sessions/" + itasks.app.session + "/delete",
			scripts: false,
			callback: function () {
				//On return, restart the app
				itasks.app.restart();
			},
			scope: this
		});
	},
	refreshGUI: function(){
		//Refresh the worklist and the active tab
		var worklist 	= this.getComponent('centerpanel').getComponent('worklist');
		var worktabs 	= this.getComponent('centerpanel').getComponent('worktabs');
		var tab = worktabs.getActiveTab();
		
		
		worklist.refresh();
		tab.refresh();
	}
});

Ext.reg('itasks.application',itasks.ApplicationPanel);
