Ext.define('itasks.component.edit.String',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itasks_edit_string',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
		Ext.applyIf(this,{
			width: 400
		});
		this.callParent(arguments);
	}
});
