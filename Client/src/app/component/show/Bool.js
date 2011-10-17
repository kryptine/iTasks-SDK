Ext.define('itasks.component.show.Bool',{
	extend: 'Ext.Component',
	alias: 'widget.itasks.show.bool',
	width: 16,
	height: 16,
	initComponent: function() {
		this.addCls(this.value ? 'icon-bool-true': 'icon-bool-false');
		this.callParent(arguments);
	}
});
