Ext.ns("itasks.tui");

itasks.tui.NoteControl = itasks.tui.extendBase(Ext.form.TextArea,{
	width: 400,
	grow: true
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);