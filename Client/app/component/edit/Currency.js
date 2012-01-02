Ext.define('itasks.component.edit.Currency',{
	alias: 'widget.itasks_edit_currency',
	extend: 'Ext.form.field.Number',
	mixins: ['itasks.mixin.Editable'],
	allowDecimals: true,
	decimalPrecision: 2,
	hideTrigger: true,
	currency: '',
	sign: '',
	initComponent: function() {	
		this.setEditValue(this.value);
		this.callParent(arguments);
	},
	setEditValue: function(value) {
		this.currency = value[0];
		
		this.suspendEvents();
		this.setValue(value[1] / 100.0);	
		this.resumeEvents();
	},
	getEditValue: function() {
		return [this.currency,Math.round(this.value * 100)];
	}
});
