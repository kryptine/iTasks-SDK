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
	setActiveTab: function (tab, noEvent) {
		var me = this;
		if(noEvent) {
			me.suspendEvent('tabchange');
			me.callParent([tab]);
			me.resumeEvent('tabchange');
		} else {
			me.callParent([tab]);
		}	
	},
	onTabChange: function (set,ntab,otab) {
		var me = this;

		if(ntab.focusTaskId) {
			itwc.global.controller.sendFocusEvent(ntab.focusTaskId);
		}
	}
});
