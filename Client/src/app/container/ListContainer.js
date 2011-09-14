Ext.define('itasks.container.ListContainer',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.ilistc',
	requires: ['itasks.layout.VHBox'],
	mixins: {
		editable :'itasks.mixin.Editable',
		wrappable : 'itasks.mixin.Wrappable'
	},
	layout: 'vhbox',
	frame: true,
	initComponent: function() {
	
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hflex = true;
		}
		if(!this.height && !this.vwrap && !this.vflex) {
			this.vwrap = true;
		}		
		this.callParent(arguments);

		this.updateItemControls();
	},
	updateItemControls: function() {
		this.items.first().updateControls();	
		if(this.items.length > 1)
			this.items.last().updateControls();
		if(this.items.length > 2)
			this.items.get(this.items.length - 2).updateControls();
	},
	onAdd: function () {
		this.callParent(arguments);
		
		if(this.rendered) {
			this.wrap();
			this.ownerCt.doLayout();
			this.updateItemControls();
		}
	}
});	