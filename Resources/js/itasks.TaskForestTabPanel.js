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
	refresh: function() {
		this.load({method: 'GET', url: 'handlers/tasktreeforest?session=' + this.applicationPanel.getSessionId()});
	}
});

Ext.reg('itasks.taskforesttab',itasks.TaskForestTabPanel);