Ext.ns("itasks.tui");

itasks.tui.StringControl = itasks.tui.extendControl(Ext.form.TextField,{
	width: 330,
	height: 25
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);