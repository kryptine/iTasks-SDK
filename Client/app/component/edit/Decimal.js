Ext.define('itwc.component.edit.Decimal',{
	alias: 'widget.itwc_edit_decimal',
	extend: 'Ext.form.field.Number',
	mixins: ['itwc.component.edit.Editable'],
	allowDecimals: true,
	hideTrigger: true,

	decimalPrecision: 20,

	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	}
});
