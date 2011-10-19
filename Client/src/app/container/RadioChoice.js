Ext.define('itasks.container.RadioChoice',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks_radiochoice',
	requires: ['itasks.layout.VHBox'],
	mixins: ['itasks.mixin.Editable'],
	padding: 2,
	bodyBorder: false,
	initComponent: function() {
		Ext.applyIf(this,{
			layout: {type: 'vhbox', direction: this.direction, halign: this.halign, valign: this.valign}
		});	
		//Add radio control
		Ext.apply(this, {
			dockedItems: [{
				dock: 'left',
				xtype: 'radiofield',
				name: this.name,
				checked: this.checked,
				listeners: {change: {fn: this.handleSelect, scope: this}}
			}]
		});

		//If no specification of the container width or height is given, make it wrapping
		if(!this.width && !this.hwrap && !this.hflex) {
			this.hwrap = true;
			this.hflex = 1;
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

		this.addEvents('edit');
		this.enableBubble('edit');
	
		this.callParent(arguments);	
	},
	handleSelect: function(cmp) {
		if(cmp.getValue()) {
			this.fireEvent('edit',this.taskId,this.name,this.index);
		}
	}
});
