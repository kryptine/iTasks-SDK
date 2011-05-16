Ext.ns("itasks.tui");

itasks.tui.CharControl = itasks.tui.extendControl(Ext.form.TextField,{
	defaultWidth: ['Fixed',40],
	defaultHeight: ['Fixed',25],
	maxLength: 1
});

Ext.reg("itasks.tui.Char",itasks.tui.CharControl);