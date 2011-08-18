Ext.ns("itasks.tui");

itasks.tui.NoteControl = itasks.tui.extendControl(Ext.form.TextArea,{
	width: 400,
	height: 100
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);