Ext.ns("itasks.tui");

itasks.tui.NoteControl = Ext.extend(Ext.form.TextArea,{
	width: 400,
	grow: true,
	initComponent: function() {
		this.msgTarget = 'side';
		this.listeners = {change: {fn: this.onChange, scope: this}};
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
			
		if(this.value == "") delete this.value;
		itasks.tui.NoteControl.superclass.initComponent.apply(this,arguments);
	
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange',this.taskId,this.name,this.getValue());
	},
	afterRender: function(){
		itasks.tui.NoteControl.superclass.afterRender.call(this,arguments);
		
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	setValue: function(value){
		itasks.tui.NoteControl.superclass.setValue.call(this,value);
	
		if(this.activeError)
			this.setError(this.activeError);
	},
	setError: function(msg){		
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	}
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);