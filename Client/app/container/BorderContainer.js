Ext.define('itasks.container.BorderContainer',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.iborderc',
	layout: 'border',
	
	initComponent: function (config) {
	
		if(!this.height) {
			this.height = 10000;
			this.simulatedHeight = true;
		}
		if(!this.width) {
			this.width = 10000;
			this.simulatedWidth = true;
		}	
		if(this.direction == 'horizontal') {
			this.itemA.region = 'west';
			this.itemA.width = this.initSplit;
		} else {
			this.itemA.region = 'north';
			this.itemA.height = this.initSplit;
		}
		
		this.itemA.split = true;
		this.itemA.collapsible = this.collapsible;
		if(!this.itemA.title) {
			this.itemA.hideCollapseTool = true;
			this.itemA.collapseMode = 'mini';
			this.itemA.header = false;
		}
		this.itemB.region = 'center';
		
		this.items = [this.itemA,this.itemB];
		
		delete(this.itemA);
		delete(this.itemB);
		delete(this.collapsible);
		
		this.callParent(arguments);
	}
});
