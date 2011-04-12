Ext.ns("itasks.tui");

itasks.tui.IntControl = itasks.tui.extendBase(Ext.form.NumberField,{
	width: 100,
	allowDecimals: false
});

Ext.reg("itasks.tui.Int",itasks.tui.IntControl);