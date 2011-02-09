Ext.ns("itasks.tui");

itasks.tui.GridControl = Ext.extend(Ext.grid.GridPanel,{
	autoHeight: true,
	 viewConfig: {
		forceFit: true,
		// increase selector depths to allow complex html layouts like for lists inside grid
		rowSelectorDepth: 1000,
		cellSelectorDepth: 400,
		rowBodySelectorDepth: 1000
	},
	
	initComponent: function() {
		var data = this.gridHtml;
		var cols = this.columns;
		var fields = [];
		var cols = this.columns;
		for(var i = 0; i < cols.length; i++){
			this.columns[i].dataIndex = i;
			fields[i] = i;
		}
		var MyRecord = Ext.data.Record.create(fields);
		var fieldsC = new Ext.util.MixedCollection();
		fieldsC.addAll(fields);
		
		var mkRecord = function(row){
			var recConfig = {};
			for(var j = 0; j < row.length; j++){
				recConfig[j] = '<div>' + row[j] + '</div>';
			}
			
			return new MyRecord(recConfig);
		};
		
		this.store = {
			getSortState: function(){},
			getCount: function(){return data.length;},
			getRange: function(start,end){
				if(!Ext.isDefined(start)) start = 0;
				if(!Ext.isDefined(end))   end   = data.length - 1;
				var records = [];
				for(var i = start; i <= end; i++) {
					records[records.length] = this.getAt(i);
				}
				
				return records;
			},
			getAt: function(i){
				return mkRecord(data[i]);
			},
			fields: fieldsC
		};
	
		itasks.tui.GridControl.superclass.initComponent.apply(this,arguments);
		
		this.selModel = new Ext.grid.CellSelectionModel();
		this.activeEditor = null;
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	onRender : function(ct, position){
		Ext.grid.GridPanel.superclass.onRender.apply(this, arguments);

		var c = this.getGridEl();

		this.el.addClass('x-grid-panel');

		this.mon(c, {
			scope: this,
			mousedown: this.onMouseDown,
			click: this.onClick,
			dblclick: this.onDblClick,
			contextmenu: this.onContextMenu
		});

		this.relayEvents(c, ['mousedown','mouseup','mouseover','mouseout']);

		var view = this.getView();
		view.init(this);
		view.render();
		this.getSelectionModel().init(this);
	},

	initEvents: function(){
		Ext.grid.EditorGridPanel.superclass.initEvents.call(this);
		this.on('celldblclick', this.onCellDblClick, this);
		this.on('cellclick', this.onCellClick, this);
	},
	
	onCellClick: function(g, row, col){
		var activeEd = this.activeEditor;
		if(this.editing && (activeEd.row != row || activeEd.col != col)){
			this.stopEditing();
		}
	},
	onCellDblClick: function(g, row, col){
		this.startEditing(row, col);
	},
	
	startEditing: function(row, col){
		this.stopEditing();
		this.editing = true;
		var ed = this.gridEditors[row][col];
		
		if(!ed.rendered){
			ed = Ext.create(ed);
			var cellEl = this.getView().getCell(row,col);
			
			ed.displayEl = new Ext.Element(cellEl.firstChild);
			ed.displayEl.setVisibilityMode(Ext.Element.DISPLAY);
			
			var edPanel = new Ext.Panel({
				ownerCt: this,
				items: [ed],
				autoWidth: true,
				autoHeight: true,
				cls: 'table-editor',
				unstyled: true,
				renderTo: cellEl
			});
			
			ed.panel = edPanel;
			this.gridEditors[row][col] = ed;
		}
		
		this.activeEditor = ed;
		
		var cm = this.getColumnModel();
		Ext.apply(ed, {
			row     			: row,
			col     			: col,
			beforeEditColWidth	: cm.getColumnWidth(col)
		});
		
		ed.displayEl.hide();
		ed.panel.show();
		cm.setColumnWidth(col,ed.getWidth() + 20);
	},
	stopEditing: function(){
		if(this.editing){
			this.editing = false;
			
			var ae = this.activeEditor;
			if(ae){
				delete this.activeEditor;
				ae.displayEl.show();
				ae.panel.hide();
				this.getColumnModel().setColumnWidth(ae.col,ae.beforeEditColWidth);
			}
		}
	}
});

Ext.reg("itasks.tui.Grid",itasks.tui.GridControl);