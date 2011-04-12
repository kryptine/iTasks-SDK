Ext.ns("itasks.tui");

itasks.tui.StringControl = itasks.tui.extendBase(Ext.form.TextField,{
	width: 330
});

Ext.reg("itasks.tui.String",itasks.tui.StringControl);