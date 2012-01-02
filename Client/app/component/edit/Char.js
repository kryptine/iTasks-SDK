Ext.define('itasks.component.edit.Char',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itasks_edit_char',
	mixins: ['itasks.mixin.Editable'],
	size: 1,
	maxLength: 1,
	enforceMaxLength: true
});
