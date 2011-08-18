Ext.ns('itasks.tui');

itasks.tui.TabContainer = itasks.tui.extendContainer(Ext.TabPanel,{
	
	vflex: 1,
	vwrap: true,
	hflex: 1,
	hwrap: true,
	
	sumW: false,
	sumH: false,
	deferredRender: false,
	forceLayout: true,
	layoutOnTabChange: true,
	listeners: { add: function() {
		// activate tab if there is only a single one
		if (this.items.getCount() == 1) {
			this.setActiveTab(0);
		}
	}},
	
	doTUILayout: function(fillW,fillH) {
		var activeTab = this.getActiveTab();
		var myS = itasks.tui.base.doTUILayout.apply(this,arguments);
		
		var totalFillW	= myS.myW - this.getFrameWidthCached();
		var totalFillH	= myS.myH - this.getFrameHeightCached();
		
		this.items.each(function(i) {
			i.doTUILayout(totalFillW, totalFillH);
		});
		
		this.setActiveTab(activeTab);
	}
});

itasks.tui.Tab = Ext.extend(Ext.Panel, {
	layout: 'hbox',
	unstyled: true,
	
	listeners: { beforeclose: function() {
		var ca = this.closeAction;
		this.fireEvent('tuiaction',ca[1],ca[0]);
		return false;
	}},
	
	initComponent: function() {
		if (Ext.isArray(this.closeAction)) {
			this.closable = true;
		}
		if(this.menus.length)
			this.tbar = this.menus;
	
		itasks.tui.Tab.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuiaction');
		this.enableBubble('tuiaction');
	},
	
	doTUILayout: function(w,h) {
		return this.get(0).doTUILayout(w,h);
	},
	getTUISize: function() {
		return this.get(0).getTUISize();
	},
	getMinTUISize: function() {
		return this.get(0).getMinTUISize();
	}
});

Ext.reg('itasks.tui.TabContainer',itasks.tui.TabContainer);
Ext.reg('itasks.tui.Tab',itasks.tui.Tab);