Ext.define('itasks.component.edit.Bool',{
	extend: 'Ext.form.field.Checkbox',
	alias: 'widget.itasks.edit.bool',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
	
		this.checked = this.value;
		this.callParent();
	}
});
