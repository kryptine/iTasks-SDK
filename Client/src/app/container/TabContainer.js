Ext.define('itasks.container.TabContainer',{
	extend: 'Ext.tab.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks.tab.container',
	initComponent: function() {
		//If no specification of the container width or height is given, make it flexible
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hflex = 1;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vflex = 1;
		}
		this.callParent(arguments);
	},
	onAdd: function(item) {
		this.callParent(arguments);
		
		if(this.rendered) {
			this.setActiveTab(item);
		}
	}
});
