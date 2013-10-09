Ext.define('itwc.container.TabSet',{
	extend: 'Ext.tab.Panel',
	alias: 'widget.itwc_tabset',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],

	itwcWidth: 'flex',
	itwcHeight: 'flex',

	border: false,
	
	initComponent: function() {
		var me = this;
        me.initSize();
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
    remove: function () {
		var me = this;
		me.suspendEvent('tabchange');
		me.callParent(arguments)
		me.resumeEvent('tabchange');
	},
    getActiveTabIndex: function() {
        var me = this,
            activeId = me.getActiveTab().getId(),
            allTabs = me.items || [],
            numTabs = allTabs.length, i;
        for(i = 0; i < numTabs; i++) {
            if(allTabs.get(i).getId() == activeId) {
                return i;
            }
        }
    },
	onTabChange: function (set,ntab,otab) {
		var me = this;

		if(ntab.focusTaskId) {
			itwc.global.controller.sendFocusEvent(ntab.focusTaskId);
		}
	},
    replace: function (index,ntab) {
        var me = this,
            active = me.getActiveTabIndex();
        me.suspendEvent('tabchange');
        me.remove(index);
        me.insert(index,ntab);
        if(active == index) {
            me.setActiveTab(index);
        }
        me.resumeEvent('tabchange');
    }
});
