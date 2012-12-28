Ext.define('itwc.container.TaskletPlaceholder', {
	extend: 'itwc.container.Tasklet',
	alias: 'widget.itwc_tasklet_placeholder',
				
	initComponent: function(c) {
		DB.loadTasklet(this.iid, this);
		this.callParent(arguments);		
	},
	
	onRender: function() {
		this.callParent(arguments);
	},
	
	afterRender: function() {	
		this.callParent(arguments);
	}
});
