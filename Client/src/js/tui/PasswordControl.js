Ext.ns('itasks.tui');

itasks.tui.PasswordControl = itasks.tui.extendBase(Ext.form.TextField,{
	defaultWidth: 100,
	fieldClass: 'x-form-field',
	inputType: 'password'
});

Ext.reg('itasks.tui.Password',itasks.tui.PasswordControl);