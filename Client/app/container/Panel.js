Ext.define('itasks.container.Panel',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks_panel',
	requires: ['itasks.layout.VHBox'],
	mixins: ['itasks.mixin.Managing'],

	initComponent: function() {
		Ext.applyIf(this,{
			layout: {type: 'vhbox', direction: this.direction, halign: this.halign, valign: this.valign}
		});
		if(this.menus && this.menus.length) {
			this.tbar = this.menus;
		}
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
		this.initManaged();
		this.callParent(arguments);	
	},
	add: function() {
		this.managedAdd.apply(this,arguments);
	},
	remove: function(comp,autoDestroy) {
		this.managedRemove.apply(this,arguments);
	},
	onAdd: function(cmp,index) {
		this.registerManaged(cmp);
		this.callParent(arguments);
	}
});
