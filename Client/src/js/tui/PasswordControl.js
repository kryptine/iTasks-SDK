Ext.ns('itasks.tui');

itasks.tui.PasswordControl = Ext.extend(Ext.form.TextField,{
	width: 100,
	fieldClass: 'x-form-field',

	initComponent: function(){
		if(this.staticDisplay){
			this.autoCreate = {tag: 'span', html: '***********'};
		}
		
		this.msgTarget = 'side';
		
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		//this.allowBlank = this.optional;
		this.inputType = 'password';
		
		if(this.value == "") delete this.value;
		
		itasks.tui.PasswordControl.superclass.initComponent.apply(this,arguments);
	},
	
	afterRender: function(){
		itasks.tui.PasswordControl.superclass.afterRender.call(this,arguments);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	setValue: function(value){		
		if(this.staticDisplay){
			this.update('***********');
		}else{
			itasks.tui.PasswordControl.superclass.setValue.call(this,value);
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

Ext.reg('itasks.tui.Password',itasks.tui.PasswordControl);