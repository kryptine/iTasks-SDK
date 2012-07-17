Ext.define('itwc.component.edit.Note',{
	extend: 'Ext.form.field.TextArea',
	alias: 'widget.itwc_edit_note',
	mixins: ['itwc.component.edit.Editable'],
	initComponent: function() {
		Ext.applyIf(this,{ width: 400});
		this.callParent(arguments);
		this.initEditable();
	}
});
