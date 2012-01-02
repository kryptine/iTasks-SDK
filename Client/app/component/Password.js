Ext.define('itasks.component.Password',{
	extend: 'Ext.form.field.Text',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.ipassword',
	inputType: 'password'
});