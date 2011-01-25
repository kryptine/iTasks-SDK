Ext.ns("itasks.tui");

itasks.tui.GridControl = Ext.extend(Ext.grid.EditorGridPanel,{
	autoHeight: true,
	 viewConfig: {
        forceFit: true
	},
	
	initComponent: function() {
		var fields = [];
		for(var i = 0; i < this.columns.length; i++){
			fields[i] = this.columns[i].dataIndex;
		}
		
		console.log(this.columns);
		console.log(this.gridData);
		
		var store = new Ext.data.JsonStore({
			// store configs
			autoDestroy: true,
			root: 'data',
			//idProperty: 'name',
			fields: fields,
			data: {data: this.gridData}
		});
		
		this.store = store;
	
		itasks.tui.GridControl.superclass.initComponent.apply(this,arguments);
		
		//this.addEvents('tuichange');
		//this.enableBubble('tuichange');
	},
});

Ext.reg("itasks.tui.Grid",itasks.tui.GridControl);