Ext.define('itasks.component.edit.Real',{
	extend: 'Ext.form.field.Number',
	alias: 'widget.itasks_edit_real',
	mixins: ['itasks.mixin.Editable'],
	allowDecimals: true,
	decimalPrecision: 20,
	hideTrigger: true
});
