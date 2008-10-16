/**
* Panel which displays a number of filters and folders of work
* that the user has to do.
*/
Ext.namespace('itasks');

itasks.CurrentWorkPanel = Ext.extend(Ext.tree.TreePanel, {

	initComponent: function () {
		Ext.apply(this,{
			title: 'Current work',
			dataUrl: 'handlers/filters',
			useArrows: true,
			root: {text: 'All work', nodeType: 'async', id: 'source'}
		});
		
		itasks.CurrentWorkPanel.superclass.initComponent.apply(this, arguments);
	}
});

Ext.reg('itasks.cwpanel', itasks.CurrentWorkPanel);