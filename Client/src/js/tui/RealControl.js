Ext.ns("itasks.tui");

itasks.tui.RealControl = itasks.tui.extendBase(Ext.form.NumberField,{
	width: 100,
	allowDecimals: true,
	decimalPrecision: 20 //Maximum precision
});

Ext.reg("itasks.tui.Real",itasks.tui.RealControl);