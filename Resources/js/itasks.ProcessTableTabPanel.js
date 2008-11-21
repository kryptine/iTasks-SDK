/**
* Tab panel which shows the global process table.
*/

Ext.ns('itasks');

itasks.ProcessTableTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: 'Process table',
			closable: true,
			autoLoad: {
				url: 'handlers/processtable',
				method: 'GET'
			}
		});
		
		itasks.ProcessTableTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.processtabletab',itasks.ProcessTableTabPanel);