Ext.ns("itasks.tui");

itasks.tui.CharControl = itasks.tui.extendBase(Ext.form.TextField,{
	width: 40,
	maxLength: 1
});

Ext.reg("itasks.tui.Char",itasks.tui.CharControl);