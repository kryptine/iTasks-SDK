Ext.ns("itasks.tui");

itasks.tui.NoteControl = itasks.tui.extendControl(Ext.form.TextArea,{
	defaultWidth: 400,
	grow: true
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);