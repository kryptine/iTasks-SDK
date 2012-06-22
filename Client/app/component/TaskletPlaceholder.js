Ext.define('itasks.component.TaskletPlaceholder', {
	extend: 'itasks.component.Tasklet',
	alias: 'widget.itasks_tasklet_placeholder',
		
	initComponent: function(c) {
		DB.loadTasklet(this.taskId, this);
		this.callParent(arguments);		
	},
	
	onRender: function() {
		this.callParent(arguments);
	},
	
	afterRender: function() {	
		this.callParent(arguments);
	}
});
