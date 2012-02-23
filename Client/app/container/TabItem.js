Ext.define('itasks.container.TabItem',{
	extend: 'Ext.panel.Panel',
	alias: 'widget.itasks_tab_item',
	requires: ['itasks.layout.VHBox'],
	layout: 'vhbox',
	
	initComponent: function() {
		if(Ext.isArray(this.closeAction)) {
			this.closable = true;
		}
		if(this.menus && this.menus.length) {
			this.tbar = this.menus;
		}
		this.addEvents('commit');
		this.enableBubble('commit');
		
		this.callParent(arguments);
		
		this.addListener('beforeclose',this.onClose,this);
	},
	onClose: function() {
		var ca = this.closeAction;
		this.fireEvent('commit', ca[1], ca[0]);
		return false;
	}
});
