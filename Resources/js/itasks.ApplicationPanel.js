/**
* Main iTasks user interface
*/
Ext.ns('itasks');

itasks.ApplicationPanel = Ext.extend(Ext.Panel, {

	application: undefined,
	
	sessionId: undefined,
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
					html: '<div id="logo" ></div><div id="user">Welcome ' + this.displayName + ' | <a id="logout" href="#">Log out &raquo;</a></div>'
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
						{xtype: 'itasks.nwpanel', id: 'newpanel' },
						//{xtype: 'itasks.cwpanel' },
						{xtype: 'itasks.debug', id: 'debugpanel' }
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
	init: function () {
		
		//Initializing the gui...
		var apppanel	= this;
		
		var worklist 	= this.getComponent('centerpanel').getComponent('worklist');
		var worktabs 	= this.getComponent('centerpanel').getComponent('worktabs');
		var debugpanel	= this.getComponent('leftpanel').getComponent('debugpanel');
		var newpanel 	= this.getComponent('leftpanel').getComponent('newpanel');
	
		//Set worklist applicationPanel reference and refresh
		worklist.setApplicationPanel(apppanel);
		worklist.refresh();
			
		newpanel.setApplicationPanel(apppanel);
		newpanel.refresh();
		
		//Connect event handlers	
		worklist.on('cellclick',function (grid,row,col,event) {
			var newtab = worktabs.openWorkTab(grid.getTaskId(row), grid.getTaskInfo(row));

			if(newtab[1]) {
				newtab[0].setDebugPanel(debugpanel);
				newtab[0].setApplicationPanel(apppanel);
				newtab[0].on('taskfinished',function(taskid) {
					worklist.refresh();
				},this);
				newtab[0].on('taskdeleted',function(taskid) {
					worklist.refresh();
				},this);
				newtab[0].on('tasksuggestsrefresh',function(taskid) {
					worklist.refresh();
				},this);
			}
			newtab[0].refresh();
		});
		
		newpanel.on('processStarted',function(startTask) {
			worklist.refresh();
			//TODO: Automatically open a tab
		},this);
		
		debugpanel.getTaskForestButton().on('click',function() {
			worktabs.openTaskForestTab(apppanel);
		});
		debugpanel.getThreadTableButton().on('click',function() {
			worktabs.openThreadTableTab(apppanel);
		});
		debugpanel.getProcessTableButton().on('click',function() {
			worktabs.openProcessTableTab(apppanel);
		});

		Ext.get('logout').on('click',function() {
			apppanel.logout();
		});
	},
	
	addSessionParam: function (params) {
		params['session'] = this.sessionId;
		return params;
	},
	checkSessionResponse: function (response) {
		if(response.error) {
			this.application.restart(response.error);
		}
	},	
	getSessionId: function() {
		return this.sessionId;
	},
	logout: function() {	
		//Send logout request to the server
		Ext.Ajax.request({
			url: 'handlers/deauthenticate',
			method: "POST",
			params: this.addSessionParam({}),
			scripts: false,
			callback: function () {
				//On return, restart the app
				this.application.restart();
			},
			scope: this
		});
	}
});