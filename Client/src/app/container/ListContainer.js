Ext.define('itasks.container.ListContainer',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.ilistc',
	requires: ['itasks.layout.VHBox'],
	mixins: ['itasks.mixin.Editable'],
	layout: 'vhbox',
	frame: true,
	initComponent: function() {
	
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hflex = 1;
			this.hwrap = true;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}		
		if(!this.width) {
			this.width = 10000;
			this.simulatedWidth = true;
		}
		if(!this.height) {
			this.height = 10000;
			this.simulatedHeight = true;
		}
		this.callParent(arguments);
	},
	onRender: function() {
		this.callParent(arguments);
	},
	afterLayout: function() {
		this.callParent(arguments);
		this.items.each (function(item) { item.updateListTools(); });
	}
});	
