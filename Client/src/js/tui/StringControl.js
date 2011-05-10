Ext.ns("itasks.tui");

itasks.tui.StringControl = itasks.tui.extendControl(Ext.form.TextField,{
	defaultWidth: 330
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);