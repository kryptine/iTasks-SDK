Ext.ns('itasks.tui');

itasks.tui.MainContainer = itasks.tui.extendContainer(Ext.Panel,{
	defaultWidth: ['FillParent', 1, 'ContentSize'],
	defaultHeight: ['FillParent', 1, 'ContentSize'],
	initComponent: function(){
		Ext.apply(this, {
			layout : "border",
			items : [{
				xtype: "panel",
				region: "north",
				height: 25,
				baseCls: "worktab-header-normal-priority",
				html: String.format(
				'<div class="worktab-header {0}">'+
					'<div class="worktab-header-text">'+
						'<table><tr><th>Subject:</th><td>{1}</td><th>Deadline:</th><td>{2}</td></table>'+
					'</div>'+
				'</div>'
				,this.worktabBackground(this.properties.managerProperties.priority)
				,this.properties.taskProperties.taskDescription.title
				,itasks.util.formatDeadline(this.properties.managerProperties.deadline)
				)
			},{
				xtype: "panel",
				region: "center",
				ctCls: "worktab-container",
				bodyStyle: "background-color: #eee",
				layout: "hbox",
				autoScroll: true,
				items: this.items,
				tbar : this.menus
			}],
		});
		itasks.tui.base.initComponent.apply(this,arguments);
		this.setTabTitle(this.properties.taskProperties.taskDescription.title);
	},
	getChildSizes: function() {
		var cached = this.getCache(this.id,'childSizes');
		if (cached !== null) return cached;
	
		var item = this.items.get(1).items.get(0);
		var sizes = new Ext.util.MixedCollection();
		sizes.add({
			item:		item,
			tuiSize:	item.getTUISize(),
			minSize:	item.getMinTUISize()
		});
		
		this.setCache(this.id,'childSizes',sizes);
		return sizes;
	},
	worktabBackground: function(priority){
		switch(priority) {
			case null : return "";
			case "LowPriority":
			case "NormalPriority": return "worktab-header-normal-priority";
			case "HighPriority": return "worktab-header-high-priority";
		}		
	},
	setTabTitle: function(title) {
		this.findParentByType(itasks.WorkPanel).setTitle(Ext.util.Format.ellipsis(title,10));
	},
	
	getTUIFrameHeight: function() {
		headerHeight = this.items.get(0).getHeight();
		tbarHeight = this.items.get(1).getTopToolbar().getHeight();
		
		return itasks.tui.container.getTUIFrameHeight.call(this) + headerHeight + tbarHeight + 4;
	}
});

Ext.reg('itasks.tui.MainContainer',itasks.tui.MainContainer);