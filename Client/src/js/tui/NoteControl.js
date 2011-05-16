Ext.ns("itasks.tui");

itasks.tui.NoteControl = itasks.tui.extendControl(Ext.form.TextArea,{
	defaultWidth: ['Fixed',400],
	defaultHeight: ['Fixed',100]
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);