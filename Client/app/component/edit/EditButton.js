Ext.define('itwc.component.edit.EditButton',{
	extend: 'Ext.Button',
	alias: 'widget.itwc_editbutton',
	mixins: ['itwc.component.edit.Editable'],
	
	initComponent: function() {
		this.addEvents('edit');
		this.callParent(arguments);
	},
	onClick: function() {
		this.viewport = this.viewport || this.findViewport();
		this.viewport.fireEvent('edit',this.taskId, this.editorId,this.value);
	},
	setValue: function(value) {
		this.value = value;
	}
});
