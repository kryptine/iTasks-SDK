/**
* Tab panel which shows the global thread table.
*/

Ext.ns('itasks');

itasks.ThreadTableTabPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: 'Thread table',
			closable: true,
			autoLoad: {
				url: 'handlers/threadtable',
				method: 'GET'
			}
		});
		
		itasks.ThreadTableTabPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.threadtabletab',itasks.ThreadTableTabPanel);