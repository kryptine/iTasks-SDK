Ext.define('itasks.component.ProgressBar',{
	extend: 'Ext.ProgressBar',
	alias: 'widget.itasks_progressbar',
	initComponent: function() {
		this.width = 200;
		this.height = 20;
		this.minHeight = 20;
		this.callParent(arguments);
	}
});
