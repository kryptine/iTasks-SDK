Ext.define('itasks.component.edit.Real',{
	extend: 'Ext.form.field.Number',
	alias: 'widget.itasks.edit.real',
	mixins: ['itasks.mixin.Editable'],
	allowDecimals: true,
	decimalPrecision: 20,
	hideTrigger: true
});
