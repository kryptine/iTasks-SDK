/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	initComponent: function() {

		Ext.apply(this, {
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
					split: true,
					border: false,
					deferredRender: false,
					width: 200,
					minWidth: 200,
					maxWidth: 400
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
		worklist.on("cellclick",function (grid,row,col,event) {
			attachTabHandlers(worktabs.openWorkTab(grid.getTaskId(row)));
		});
		
		worklist.on("workListRefreshed",function(worklist) {
			worklist.workStore.each(function(){
				var tab = worktabs.getComponent("worktab-"+this.data.taskid);
				var wlTStamp = this.data.latestExtEvent;
				
				if(tab != null){
					var tTStamp = tab.properties.systemProps.latestEvent
				
					if(wlTStamp > tTStamp){
						tab.refresh();
					}				
				}				
			});
		});
		
		newpanel.on("processStarted",function(taskid) {
			//When new work is started, refresh the worklist
			//and immediately open a tab for the work
			worklist.refresh();	
			attachTabHandlers(worktabs.openWorkTab(taskid));
		},this);
		
		//Fix for tabpanel resize event bug
		worktabs.on("resize",function(c) {
			c.doLayout();
		});
		
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
			url: itasks.config.serverUrl + "/deauthenticate",
			method: "POST",
			params: {session: itasks.app.session},
			scripts: false,
			callback: function () {
				//On return, restart the app
				itasks.app.restart();
			},
			scope: this
		});
	}
});