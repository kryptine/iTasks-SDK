/**
* Tab panel which shows the global task forest.
*/

Ext.ns('itasks');

itasks.TaskForestTabPanel = Ext.extend(itasks.RemoteDataPanel, {
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Task forest',
			closable: true,
			autoScroll: true,
			url: 'handlers/debug/tasktreeforest',
			cls: 'worktab-container',
			tbar: [{
					text: 'Refresh',
					iconCls: 'x-tbar-loading',
					listeners: {click: {fn: function (btn) {this.refresh();},scope: this}}
				}]
		});
		
		itasks.TaskForestTabPanel.superclass.initComponent.apply(this, arguments);
	},
	update: function(data) {
		this.body.dom.innerHTML = data;
	}
});

Ext.reg('itasks.taskforesttab',itasks.TaskForestTabPanel);