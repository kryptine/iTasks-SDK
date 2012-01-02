Ext.define('itasks.component.edit.Date',{
	extend: 'Ext.form.field.Date',
	alias: 'widget.itasks_edit_date',
	mixins: ['itasks.mixin.Editable'],
	format: 'Y-m-d',
	getEditValue: function() {
		return this.getRawValue();
	}
});
