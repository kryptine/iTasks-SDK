Ext.ns('itasks.tui');

itasks.tui.PasswordControl = Ext.extend(Ext.form.TextField,{
	width: 100,
	fieldClass: 'x-form-field',

	initComponent: function(){
		if(this.staticDisplay){
			this.autoCreate = {tag: 'span', html: '***********'};
		}
		
		this.msgTarget = 'side';
		this.listeners = {change: {fn: this.onChange, scope: this}};
				
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);

		this.inputType = 'password';
		
		if(this.value == "") delete this.value;
		
		itasks.tui.PasswordControl.superclass.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	onChange: function() {
		this.fireEvent('tuichange',this.name,this.getValue());
	},	
	afterRender: function(){
		itasks.tui.PasswordControl.superclass.afterRender.call(this,arguments);
		
		if(this.errorMsg)
			itasks.tui.common.markError(this,this.errorMsg);
		else if(this.hintMsg)
			itasks.tui.common.markHint(this,this.hintMsg);
	},
	setValue: function(value){		
		if(this.staticDisplay){
			this.update('***********');
		}else{
			itasks.tui.PasswordControl.superclass.setValue.call(this,value);

			if(this.activeError)
				this.setError(this.activeError);
		}
	},
	setError: function(msg) {		
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

Ext.reg('itasks.tui.Password',itasks.tui.PasswordControl);