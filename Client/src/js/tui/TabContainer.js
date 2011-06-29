Ext.ns('itasks.tui');

itasks.tui.TabContainer = itasks.tui.extendContainer(Ext.TabPanel,{
	defaultWidth: ['FillParent', 1, 'ContentSize'],
	defaultHeight: ['FillParent', 1, 'ContentSize'],
	sumW: false,
	sumH: false,
	deferredRender: false,
	forceLayout: true,
	activeTab: 0,
	layoutOnTabChange: true,
	
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