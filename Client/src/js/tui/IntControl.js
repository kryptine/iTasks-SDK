Ext.ns("itasks.tui");

itasks.tui.IntControl = itasks.tui.extendControl(Ext.form.NumberField,{
	width: 100,
	allowDecimals: false
});

Ext.reg("itasks.tui.Int",itasks.tui.IntControl);