Ext.define('itasks.container.Panel',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks_panel',
	requires: ['itasks.layout.VHBox'],

	initComponent: function() {
		Ext.applyIf(this,{
			layout: {type: 'vhbox', direction: this.direction, halign: this.halign, valign: this.valign}
		});	
		//If no specification of the container width or height is given, make it wrapping
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hwrap = true;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}
		//Ext cannot properly render panels that don't have initial size
		if(!this.height) {
			this.height = 10000;
			this.simulatedHeight = true;
		}
		if(!this.width) {
			this.width = 10000;
			this.simulatedWidth = true;
		}
		this.callParent(arguments);	
	}
});
