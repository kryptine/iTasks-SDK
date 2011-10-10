Ext.define('itasks.component.Bool',{
	extend: 'Ext.form.field.Checkbox',
	alias: 'widget.ibool',
	mixins: ['itasks.mixin.Editable'],
	initComponent: function() {
	
		this.checked = this.value;
		this.callParent();
	}
});
