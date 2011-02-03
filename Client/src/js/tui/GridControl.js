Ext.ns("itasks.tui");

itasks.tui.GridControl = Ext.extend(Ext.grid.EditorGridPanel,{
	autoHeight: true,
	 viewConfig: {
        forceFit: true
	},
	
	initComponent: function() {
		var fields = [];
		var cols = this.columns;
		for(var i = 0; i < cols.length; i++){
			if (cols[i].editor) cols[i].editor = {xtype: cols[i].editor};
			fields[i] = cols[i].name;
		}
		
		var store = new Ext.data.JsonStore({
			autoDestroy: true,
			root: 'data',
			fields: fields,
			data: {data: this.gridData}
		});
		
		this.store = store;
	
		itasks.tui.GridControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
		
		this.on('afteredit',function(e) {
			this.fireEvent('tuichange',this.name + "-" + e.row + "-" + e.field,e.value.toString());
			e.record.commit();
		},this);
	}
});

Ext.reg("itasks.tui.Grid",itasks.tui.GridControl);