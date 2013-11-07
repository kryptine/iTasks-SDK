Ext.define('itwc.component.edit.String',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itwc_edit_string',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],

	itwcWidth: 'flex',
	itwcMinWidth: 200,

	initComponent: function() {
        this.initSize();
		this.callParent(arguments);
		this.initEditable();
	}
});
