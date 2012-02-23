Ext.define('itasks.component.edit.Int',{
	alias: 'widget.itasks_edit_int',
	extend: 'Ext.form.field.Number',
	mixins: ['itasks.mixin.Editable'],
	allowDecimals: false
});
