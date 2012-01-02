Ext.define('itasks.container.ListContainer',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks_list_container',
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
		if(!this.margins) {
			this.margins = '0 0 5 0';
		}
		this.callParent(arguments);
	},
	onBeforeAdd: function(item) {
		if(this.taskId) {
			item.taskId = this.taskId;
			item.name = this.name;
		}
		this.callParent(arguments);
	},
	afterLayout: function() {
		this.callParent(arguments);
		if(this.taskId) {
			this.items.each (function(item) { item.updateListTools(); });
		}
	}
});	
