Ext.define('itasks.component.Char',{
	extend: 'Ext.form.field.Text',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.ichar',
	size: 1,
	maxLength: 1,
	enforceMaxLength: true
});
