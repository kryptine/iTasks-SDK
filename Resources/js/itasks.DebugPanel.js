/**
* Panel for debugging iTask workflows. Here we will put stuff such as
* task tree visualization and communication state inspection.
*/

Ext.namespace('itasks');

itasks.DebugPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			title: 'Debug',
			html: 'This panel will show debug information',
			bodyStyle: 'padding: 5px'
		});
		
		itasks.DebugPanel.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.debug',itasks.DebugPanel);