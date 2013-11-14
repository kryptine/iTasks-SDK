Ext.define('itwc.component.edit.EditButton',{
	extend: 'Ext.Button',
	alias: 'widget.itwc_editbutton',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	
	initComponent: function() {
        this.initSize();
		this.addEvents('edit');
		this.callParent(arguments);
	},
	onClick: function() {

		var val = this.value;
	
		this.lastEditNo = itwc.global.controller.sendEditEvent(this.taskId,this.getEditorId(),val);
		this.lastEditVal = val;
	},
	setValue: function(value) {
		this.value = value;
	}
});
