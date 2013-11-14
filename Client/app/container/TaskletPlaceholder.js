Ext.define('itwc.container.TaskletPlaceholder', {
	extend: 'itwc.container.Tasklet',
	alias: 'widget.itwc_tasklet_placeholder',
    mixins: ['itwc.Sizeable'],
				
	initComponent: function(c) {
        this.initSize();
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
