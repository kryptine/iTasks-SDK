/**
* Tab panel which shows the global task forest.
*/

Ext.ns('itasks');

itasks.TaskForestTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Task forest',
			closable: true,
			autoScroll: true,
			cls: 'worktab-container'
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	},
	setApplicationPanel: function (panel) {
		this.applicationPanel = panel;
	},
	processResponse: function (el, success, response, options) {
		if(response.responseText.substr(0,4) != '<div') {
			this.applicationPanel.checkSessionResponse(Ext.decode(response.responseText));
		}
	},
	refresh: function() {
		this.load({
			method: 'GET',
			url: 'handlers/tasktreeforest',
			params: this.applicationPanel.addSessionParam({}),
			callback: this.processResponse,
			scope: this
		});
	}
});

Ext.reg('itasks.taskforesttab',itasks.TaskForestTabPanel);