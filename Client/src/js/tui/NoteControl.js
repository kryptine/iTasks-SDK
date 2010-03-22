Ext.ns("itasks.tui");

itasks.tui.NoteControl = Ext.extend(Ext.form.TextArea,{
	width: 400,
	grow: true,
	initComponent: function() {
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.NoteControl.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);