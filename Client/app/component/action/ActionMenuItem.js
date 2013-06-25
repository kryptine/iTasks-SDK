Ext.define('itwc.component.action.ActionMenuItem',{
	extend: 'Ext.menu.Item',
	mixins: ['itwc.component.edit.Editable'], //Add editable mixin for the findViewport function
	alias: 'widget.itwc_actionmenuitem',
	floating: false,
	
	initComponent: function() {
		this.addEvents('action');
		this.callParent(arguments);
	},
	onClick: function() {
		itwc.global.controller.sendActionEvent(this.taskId,this.actionId);

		return this.callParent(arguments);
	},
	setTaskId: function(taskId) {
		this.taskId = taskId;
	},
	setActionId: function(actionId) {
		this.actionId = actionId;
	}
});
