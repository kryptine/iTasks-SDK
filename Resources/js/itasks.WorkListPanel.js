/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.Panel, {

	initComponent: function () {
		Ext.apply(this, {
			border: false,
			html: 'This panel will show the filtered list of work',
			bodyStyle: 'padding: 5px'
		});
		
		itasks.WorkListPanel.superclass.initComponent.apply(this, arguments);
	}

});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
