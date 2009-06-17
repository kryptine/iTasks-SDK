/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	sessionId: undefined,
	application: undefined,
	
	displayName: undefined,

	initComponent: function() {

		Ext.apply(this, {
			layout: 'border',
			hidden: true,
			hideMode: 'offsets',
			deferredRender: false,
			items: [{
					id: 'northpanel',
					xtype: 'panel',
					region: 'north',
					height: 40,
					html: '<div id="logo" ></div><div id="user">Welcome ' + this.displayName + ' | <a id="logout" href="javascript:void(0);">Log out &raquo;</a></div>'
				},{
					id: 'leftpanel',
					xtype: 'panel',
					region: 'west',
					layout: 'accordion',
					layoutConfig: {animate: true},
					split: true,
					border: false,
					deferredRender: false,
					width: 200,
					minWidth: 200,
					maxWidth: 400,
					items: [
						{id: 'newpanel', xtype: 'itasks.nwpanel', sessionId: this.sessionId, application: this.application },
						{id: 'debugpanel', xtype: 'itasks.debug', sessionId: this.sessionId, application: this.application }
					]
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
						height: 150,
						sessionId: this.sessionId,
						application: this.application
					},{
						id: 'worktabs',
						xtype: 'itasks.worktabs',
						border: false,
						region: 'center',
						sessionId: this.sessionId,
						application: this.application
					}]
				}]
		});
	
		itasks.ApplicationPanel.superclass.initComponent.apply(this, arguments);
	},
	init: function () {
		
		//Initializing the gui...
		var apppanel	= this;
		
		var worklist 	= this.getComponent('centerpanel').getComponent('worklist');
		var worktabs 	= this.getComponent('centerpanel').getComponent('worktabs');
		var debugpanel	= this.getComponent('leftpanel').getComponent('debugpanel');
		var newpanel 	= this.getComponent('leftpanel').getComponent('newpanel');
	
		//Refresh initial overviews
		worklist.refresh();
		
		//Connect event handlers	
		worklist.on("cellclick",function (grid,row,col,event) {
		
			var trace = debugpanel.getTraceCheckbox().getValue();
			var tab = worktabs.openWorkTab(grid.getTaskId(row),trace);
			
			if(tab[1]) { //The tab is new
				tab[0].on("taskfinished",function(taskid) {
					worklist.refresh();
				},this);
				tab[0].on("taskdeleted",function(taskid) {
					worklist.refresh();
				},this);
				tab[0].on("tasksuggestsrefresh",function(taskid) {
					worklist.refresh();
				},this);
				
				debugpanel.getTraceCheckbox().on("check",function(cb,val) {
					tab[0].setTrace(val);
				});
			}
			tab[0].refresh();
		});
		
		newpanel.on("processStarted",function(taskid) {
			//When new work is started, refresh the worklist
			//and immediately open a tab for the work
			worklist.refresh();
			
			var trace = debugpanel.getTraceCheckbox().getValue();
			var tab = worktabs.openWorkTab(taskid, trace);
			if(tab[1]) { //The tab is new
				tab[0].on("taskDone",function(taskid) {
					worklist.refresh();
				},this);
				tab[0].on("taskRedundant",function(taskid) {
					worklist.refresh();
				},this);
				/*
				tab[0].on("tasksuggestsrefresh",function(taskid) {
					worklist.refresh();
				},this);
				*/
				debugpanel.getTraceCheckbox().on("check",function(cb,val) {
					tab[0].setTrace(val);
				});
			}
			tab[0].refresh();

		},this);
		
		debugpanel.getTaskForestButton().on("click",function() {
			worktabs.openTaskForestTab();
		});
		debugpanel.getProcessTableButton().on("click",function() {
			worktabs.openProcessTableTab();
		});
		Ext.get("logout").on("click",function() {
			apppanel.logout();
		});
	},
	
	logout: function() {	
		//Send logout request to the server
		Ext.Ajax.request({
			url: 'handlers/deauthenticate',
			method: "POST",
			params: {session: this.sessionId},
			scripts: false,
			callback: function () {
				//On return, restart the app
				this.application.restart();
			},
			scope: this
		});
	}
});