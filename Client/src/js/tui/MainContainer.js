Ext.ns('itasks.tui');

itasks.tui.MainContainer = itasks.tui.extendContainer(Ext.Panel,{
	
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
				tbar : [{text: 'Task Actions',
						 iconCls: 'icon-properties',
						 menu: {items: [{
							text: 'Refresh Task',
							iconCls: 'x-tbar-loading',
							scope: this,
							handler: function(item,evt){
								this.findParentByType(itasks.WorkPanel).refresh(false);
							}},{
							text: 'Task Properties'	,
							iconCls: 'icon-properties',
							scope: this,
							handler: function(item,evt){
								this.findParentByType(itasks.WorkPanel).showProperties();
							}},'-',{
							text: 'Cancel Task',
							iconCls: 'icon-trash',
							scope: this,
							handler: function(item,evt){
								this.findParentByType(itasks.WorkPanel).cancel();
							}}]
						 }}].concat(this.menus)
			}],
		});	
		itasks.tui.base.initComponent.apply(this,arguments);
	},
	doTUILayout: function(fillW,fillH) {
		this.setWidth(fillW);
		this.setHeight(fillH);
		
		headerHeight = this.items.get(0).getHeight();
		tbarHeight = this.items.get(1).getTopToolbar().getHeight();
		
		this.items.get(1).items.get(0).doTUILayout(fillW,fillH - headerHeight - tbarHeight);
	},
	worktabBackground: function(priority){
		switch(priority) {
			case null : return "";
			case "LowPriority":
			case "NormalPriority": return "worktab-header-normal-priority";
			case "HighPriority": return "worktab-header-high-priority";
		}		
	}
});

Ext.reg('itasks.tui.MainContainer',itasks.tui.MainContainer);