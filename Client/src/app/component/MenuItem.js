Ext.define('itasks.component.MenuItem',{
	extend: 'Ext.menu.Item',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks.menu.item',
	floating: false,
	
	initComponent: function() {
		this.callParent(arguments);

		this.addEvents('commit');
		this.addListener('click',this.onClick,this);
	},
	onClick: function() {
		if(this.target) {
			this.fireEvent('commit',this.target,this.action);
		}
	}
});
