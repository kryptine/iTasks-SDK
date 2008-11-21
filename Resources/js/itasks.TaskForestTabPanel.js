/**
* Tab panel which shows the global task forest.
*/

Ext.ns('itasks');

itasks.TaskForestTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: 'Task forest',
			closable: true,
			autoLoad: {
				url: 'handlers/tasktreeforest',
				method: 'GET'
			}
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.taskforesttab',itasks.TaskForestTabPanel);