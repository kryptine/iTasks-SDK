Ext.define('itwc.component.edit.Password',{
	extend: 'Ext.form.field.Text',
	alias: 'widget.itwc_edit_password',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	inputType: 'password',

	itwcWidth: 'flex',
	itwcMinWidth: 200,

	initComponent: function() {
        this.initSize();
		this.callParent(arguments);
		this.initEditable();
	}
});
