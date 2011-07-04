Ext.ns("itasks.tui");

itasks.tui.StringControl = itasks.tui.extendControl(Ext.form.TextField,{
	defaultWidth: ['Fixed',330],
	defaultHeight: ['Fixed',25]
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);