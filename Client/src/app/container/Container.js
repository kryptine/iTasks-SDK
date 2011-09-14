Ext.define('itasks.container.Container',{
	extend: 'Ext.container.Container',
	alias: 'widget.icontainer',
	requires: ['itasks.layout.VHBox'],
	mixins: {
		wrappable: 'itasks.mixin.Wrappable'
	},
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
		this.callParent(arguments);	
	}
});
