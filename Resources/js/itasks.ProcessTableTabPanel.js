/**
* Tab panel which shows the global process table.
*/

Ext.ns('itasks');

itasks.ProcessTableTabPanel = Ext.extend(Ext.Panel, {

	applicationPanel: undefined,
	
	initComponent: function () {
		Ext.apply(this, {
			title: 'Process table',
			closable: true,
			autoScroll: true,
			cls: 'worktab-container'
		});
		
		itasks.ProcessTableTabPanel.superclass.initComponent.apply(this, arguments);
	},
	setApplicationPanel: function(panel) {
		this.applicationPanel = panel;
	},
	refresh: function() {
		this.load({method: 'GET', url: 'handlers/processtable?session=' + this.applicationPanel.getSessionId()});
	}
});

Ext.reg('itasks.processtabletab',itasks.ProcessTableTabPanel);