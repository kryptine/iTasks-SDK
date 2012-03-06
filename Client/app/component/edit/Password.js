Ext.define('itasks.component.edit.Password',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itasks_edit_password',
	mixins: ['itasks.mixin.Editable'],
	inputType: 'password',
	initComponent: function() {
		Ext.applyIf(this,{
			width: 400
		});
		this.callParent(arguments);
	}
});
