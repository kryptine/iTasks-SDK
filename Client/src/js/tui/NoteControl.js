Ext.ns("itasks.tui");

itasks.tui.NoteControl = Ext.extend(Ext.form.TextArea,{
	width: 400,
	grow: true,
	initComponent: function() {
		if(this.staticDisplay){
			this.autoCreate = {tag: 'div', style: 'width:330px; overflow: auto', html: this.value.replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<br>$2')};
			this.autoHeight = true;
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.allowBlank = this.optional;
		if(this.value == "") delete this.value;
		itasks.tui.NoteControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.NoteControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
		
	setValue: function(value){
		if(this.staticDisplay){
			this.update(value.replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<br>$2'));
		}else{
			itasks.tui.NoteControl.superclass.setValue.call(this,value);
		}
		
		if(this.activeError) this.setError(this.activeError);
	},
	
	setError: function(msg){		
		(function() {
			if(msg == "") itasks.tui.common.clearError(this);
			else itasks.tui.common.markError(this,msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
			if(msg == "") itasks.tui.common.clearHint(this);
			else itasks.tui.common.markHint(this,msg);
		}).defer(50,this);
	}
});

Ext.reg("itasks.tui.Note",itasks.tui.NoteControl);