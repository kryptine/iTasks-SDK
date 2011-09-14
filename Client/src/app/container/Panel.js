Ext.define('itasks.container.Panel',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.ipanel',
	requires: ['itasks.layout.VHBox'],
	mixins: {
		wrappable: 'itasks.mixin.Wrappable'
	},
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
		this.callParent(arguments);	
	}
});
