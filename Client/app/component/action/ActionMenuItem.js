Ext.define('itwc.component.action.ActionMenuItem',{
	extend: 'Ext.menu.Item',
	mixins: ['itwc.component.edit.Editable'], //Add editable mixin for the findViewport function
	alias: 'widget.itwc_actionmenuitem',
	floating: false,
	
	initComponent: function() {
		this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		this.viewport = this.findViewport();
		this.viewport.fireEvent('action',this.taskId, this.actionId);
		return this.callParent(arguments);
	}
});
