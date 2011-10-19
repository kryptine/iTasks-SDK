Ext.define('itasks.container.Container',{
	extend: 'Ext.container.Container',
	alias: 'widget.itasks_container',
	requires: ['itasks.layout.VHBox'],

	initComponent: function() {
		Ext.applyIf(this,{
			layout: {type: 'vhbox', direction: this.direction, valign: this.valign, halign: this.halign}
		});
		
		//If no specification of the container width or height is given, make it wrapping
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hwrap = true;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}
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
