Ext.define('itasks.component.MenuButton',{
	extend: 'Ext.Button',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks_menu_button',
	initComponent: function() {
		this.callParent(arguments);

		this.addEvents('commit');
	},
	onClick: function() {
		if(this.target) {
			this.fireEvent('commit',this.target,this.action);
		} else {
			this.callParent(arguments);
		}
	}
});
