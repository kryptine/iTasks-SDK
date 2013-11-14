Ext.define('itwc.component.action.ActionButton',{
	extend: 'Ext.Button',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'], //Add editable mixin for the findViewport function
	alias: 'widget.itwc_actionbutton',
	
	initComponent: function() {
        this.initSize();
        this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		itwc.global.controller.sendActionEvent(this.taskId,this.actionId);
	},
	//Update operations
	setEnabled: function(enabled) {
		if(enabled) {
			this.enable();
		} else {
			this.disable();
		}
	},
	setTaskId: function(taskId) {
		this.taskId = taskId;
	},
	setActionId: function(actionId) {
		this.actionId = actionId;
	}
});
