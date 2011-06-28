Ext.ns("itasks.tui");

itasks.tui.GridContainer = itasks.util.extend(Ext.grid.GridPanel, itasks.tui.control, {
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['FillParent',1,['FixedMinSize',150]],
	recordCache: {},
	stateful: false,
	listeners: {viewready: function() {
		if (Ext.isNumber(this.value)) {
			this.getSelectionModel().selectRow(this.value);
		}
	}},
	
	initComponent: function() {
		var cells = this.cells;
		var headers = this.headers;
		var fields = [];
		this.columns = [];
		for(var i = 0; i < headers.length; i++){
			this.columns[i] = {header: headers[i], dataIndex: i};
			fields[i] = i;
		}
		var MyRecord = Ext.data.Record.create(fields);
		var fieldsC = new Ext.util.MixedCollection();
		fieldsC.addAll(fields);
		
		var grid = this;
		var mkRecord = function(row,idx){
			var recConfig = {};
			for(var j = 0; j < row.length; j++){
				recConfig[j] = '<div>' + row[j] + '</div>';
			}
			
			var rec = new MyRecord(recConfig);
			rec.row = idx;
			
			grid.recordCache[rec.id] = rec;
			return rec;
		};
		
		this.store = new Ext.util.Observable();
		this.store.addEvents('update');
		Ext.apply(this.store,{
			getSortState: function(){},
			getCount: function(){return cells.length;},
			getRange: function(start,end){
				if(!Ext.isDefined(start)) start = 0;
				if(!Ext.isDefined(end))   end   = cells.length - 1;
				var records = [];
				for(var i = start; i <= end; i++) {
					records[records.length] = this.getAt(i);
				}
				return records;
			},
			getAt: function(i){
				return mkRecord(cells[i],i);
			},
			indexOf: function(rec){
				return rec.row;
			},
			indexOfId: function(id){
				var rec = grid.recordCache[id];
				return Ext.isDefined(rec) ? rec.row : -1;
			},
			fields: fieldsC
		});
		
		this.selModel = new Ext.grid.RowSelectionModel({
			singleSelect: true,
			listeners: {rowselect: function(selM,rowIdx,rec) {
				grid.fireEvent('tuichange',grid.taskId,grid.name,rec.row);
			}}
		});
		
		itasks.tui.control.initComponent.apply(this,arguments);
	},
	
	setSize: function(w,h) {
		this.extSuperclass.setSize.call(this,w,h);
	},
	
	setValue: function(v) {
		if (Ext.isNumber(v)) {
			this.getSelectionModel().selectRow(v);
		} else {
			this.getSelectionModel().clearSelections();
		}
	}
});

Ext.reg("itasks.tui.Grid",itasks.tui.GridContainer);