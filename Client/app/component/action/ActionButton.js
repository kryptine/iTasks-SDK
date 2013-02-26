Ext.define('itwc.component.action.ActionButton',{
	extend: 'Ext.Button',
	mixins: ['itwc.component.edit.Editable'], //Add editable mixin for the findViewport function
	alias: 'widget.itwc_actionbutton',
	
	initComponent: function() {
		this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		this.viewport = this.findViewport();
		this.viewport.fireEvent('action',this.taskId, this.actionId);
	},
	//Update operations
	setEnabled: function(enabled) {
		if(enabled) {
			this.enable();
		} else {
			this.disable();
		}
	}
});
