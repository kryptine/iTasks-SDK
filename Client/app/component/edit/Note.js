Ext.define('itwc.component.edit.Note',{
	extend: 'Ext.form.field.TextArea',
	alias: 'widget.itwc_edit_note',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	
	itwcWidth: 'flex',
	itwcMinWidth: 400,
	
	initComponent: function() {
        this.initSize();
		this.callParent(arguments);
		this.initEditable();
	}
});
