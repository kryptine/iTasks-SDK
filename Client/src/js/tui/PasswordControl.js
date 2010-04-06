Ext.ns('itasks.tui');

itasks.tui.PasswordControl = Ext.extend(Ext.form.TextField,{
	width: 100,
	fieldClass: 'x-form-field',

	initComponent: function(){
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		this.allowBlank = this.optional;
		this.inputType = 'password';
		
		if(this.value == "") delete this.value;
		
		itasks.tui.PasswordControl.superclass.initComponent.apply(this,arguments);
	}

});

Ext.reg('itasks.tui.Password',itasks.tui.PasswordControl);