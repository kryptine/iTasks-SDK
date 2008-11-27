/**
* Tab panel which shows the global thread table.
*/

Ext.ns('itasks');

itasks.ThreadTableTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Thread table',
			closable: true,
			autoScroll: true,
			cls: 'worktab-container'
		});
		
		itasks.ThreadTableTabPanel.superclass.initComponent.apply(this, arguments);
	},
	setApplicationPanel: function(panel) {
		this.applicationPanel = panel;
	},
	refresh: function() {
		this.load({method: 'GET', url: 'handlers/threadtable?session=' + this.applicationPanel.getSessionId()});
	}
});

Ext.reg('itasks.threadtabletab',itasks.ThreadTableTabPanel);