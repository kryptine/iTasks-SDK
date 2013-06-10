Ext.define('itwc.component.edit.Note',{
	extend: 'Ext.form.field.TextArea',
	alias: 'widget.itwc_edit_note',
	mixins: ['itwc.component.edit.Editable'],
	
	width: 'flex',
	minWidth: 400,
	
	initComponent: function() {
		this.callParent(arguments);
		this.initEditable();
	},
	setValue: function (value, noEvent) {
		var me = this;
		if(noEvent) {
			me.suspendEvent('change');
			me.callParent([value]);
			me.resumeEvent('change');
		} else {
			me.callParent([value]);
		}	
	}
});
