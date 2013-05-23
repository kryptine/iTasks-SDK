Ext.define('itwc.container.TabSet',{
	extend: 'Ext.tab.Panel',
	alias: 'widget.itwc_tabset',
	mixins: ['itwc.component.edit.Editable'],

	width: 'flex',
	height: 'flex',
	border: false,
	
	initComponent: function() {
		var me = this;
		me.callParent(arguments);	
		me.addManagedListener(me,'tabchange',me.onTabChange, me);
	},
	onTabChange: function (set,ntab,otab) {
		var me = this;

		if(ntab.focusTaskId) {
			me.viewport = me.findViewport();
			me.viewport.fireEvent('focus',ntab.focusTaskId);
		}
	}
});
