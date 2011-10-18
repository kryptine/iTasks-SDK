Ext.define('itasks.component.Date',{
	extend: 'Ext.form.field.Date',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.idate',
	format: 'Y-m-d',
	getEditValue: function() {
		return this.getRawValue();
	}
});