Ext.ns("itasks.tui");

itasks.tui.GridContainer = itasks.util.extend(Ext.Button, itasks.tui.base, {
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['WrapContent',0],
	
	htmlDirty: false,
	stateful: false,
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
		
		var mkRecord = function(row,idx){
			var recConfig = {};
			for(var j = 0; j < row.length; j++){
				recConfig[j] = '<div>' + row[j] + '</div>';
			}
			
			var rec = new MyRecord(recConfig);
			rec.row = idx;
			return rec;
		};
		
		this.store = new Ext.util.Observable();
		this.store.addEvents('update');
		Ext.apply(this.store,{
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
				return mkRecord(data[i],i);
			},
			indexOf: function(rec){
				return rec.row;
			},
			fields: fieldsC
		});
	
		itasks.tui.base.initComponent.apply(this,arguments);
		
		this.selModel = new Ext.grid.CellSelectionModel();
		this.activeEditor = null;
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	onRender : function(ct, position){
		this.extSuperclass.onRender.apply(this, arguments);

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
		this.extSuperclass.initEvents.call(this);
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
		
		var cellEl = new Ext.Element(this.getView().getCell(row,col));
		if(!ed.rendered){
			ed = Ext.create(ed);
			
			ed.displayEl = new Ext.Element(cellEl.first());
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
		}else if(!cellEl.contains(ed.getEl())){
			// re-append editor after cell update
			cellEl.appendChild(ed.panel.getEl());
			ed.displayEl = new Ext.Element(cellEl.first());
			ed.displayEl.setVisibilityMode(Ext.Element.DISPLAY);
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
			
			if(ae.htmlDirty){
				var store = this.store;
				store.fireEvent('update',store,store.getAt(ae.row));
				ae.htmlDirty = false;
			}
		}
	},
	
	setValue: function(v){
		var	row = v[0],
			col = v[1],
			val = v[2];
			
		this.gridHtml[row][col] = val;
		
		var ae = this.activeEditor;
		if (ae && ae.row == row){
			ae.htmlDirty = true;
		}else{
			var store = this.store;
			store.fireEvent('update',store,store.getAt(row));
		}
	}
});

Ext.reg("itasks.tui.Grid",itasks.tui.GridContainer);