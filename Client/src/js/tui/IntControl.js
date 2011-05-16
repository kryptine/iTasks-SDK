Ext.ns("itasks.tui");

itasks.tui.IntControl = itasks.tui.extendControl(Ext.form.NumberField,{
	defaultWidth: ['Fixed',100],
	defaultHeight: ['Fixed',25],
	allowDecimals: false
});

Ext.reg("itasks.tui.Int",itasks.tui.IntControl);