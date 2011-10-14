Ext.define('itasks.component.edit.Char',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itasks.edit.char',
	mixins: ['itasks.mixin.Editable'],
	size: 1,
	maxLength: 1,
	enforceMaxLength: true
});
