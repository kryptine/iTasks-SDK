Ext.ns("itasks.tui");

itasks.tui.CharControl = itasks.tui.extendControl(Ext.form.TextField,{
	width: 40,
	height: 25,
	maxLength: 1
});

Ext.reg("itasks.tui.Char",itasks.tui.CharControl);