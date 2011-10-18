Ext.define('itasks.component.edit.Date',{
	extend: 'Ext.form.field.Date',
	alias: 'widget.itasks.edit.date',
	mixins: ['itasks.mixin.Editable'],
	format: 'Y-m-d',
	getEditValue: function() {
		return this.getRawValue();
	}
});
