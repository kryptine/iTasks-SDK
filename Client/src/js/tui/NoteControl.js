Ext.ns("itasks.tui");

itasks.tui.NoteControl = Ext.extend(Ext.form.TextArea,{
	width: 330,
	grow: true,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'width:330px; overflow: auto', html: this.value.replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<br>$2')};
			this.autoHeight = true;
		}
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.NoteControl.superclass.initComponent.apply(this,arguments);
	},
	
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value.replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<br>$2'));
		}else{
			itasks.tui.NoteControl.superclass.setValue.call(this,value);
		}
	}
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);