Ext.define('itasks.component.Grid',{
	extend: 'Ext.grid.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks_grid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	value: null,
	viewConfig: {loadMask: false},
	
	initComponent: function() {
		var headers = this.headers;
		var cells = this.cells;
		var fields = [];
		var i;

		delete(this.headers);
		delete(this.cells);
		
		//Default width & height
		if(!this.width && !this.hflex) {
			this.minWidth = 400;
			this.hflex = 1;
		}
		if(!this.height && !this.vflex) {
			this.height = 150;
			
		} else if(this.vwrap) {
			this.minHeight = 150;
		}
		
		//Setup columns
		this.columns = [];
		if(headers.length) {
			for(i = 0; i < headers.length; i++) {
				this.columns[i] = {text: headers[i], dataIndex: i};
				fields[i] = {name: i, type: 'string'};
			}
		} else {
			this.columns[0] = {text: "&nbsp;", dataIndex: 0};
			fields[0] = {name: 0, type: 'string'};
		}
				
		//Fill store with data
		this.store = Ext.create('Ext.data.Store',{
			fields: fields,
			data: {'items': cells},
			proxy: { type: 'memory', reader: {type: 'json', root: 'items'}}
		});
		
		this.callParent(arguments);
		this.addListener('itemclick',this.onItemClick,this);		
	},
	afterRender: function() {
		this.callParent(arguments);
		this.setValue(this.value);
	},
	onItemClick: function(view,rec) {
		this.value = rec.index;
		this.fireEvent('change');	
	},
	setValue: function(value) {
		if(Ext.isNumber(value) && value < this.store.count() && value >= 0) {
			this.getSelectionModel().select(value);
		} else {
			this.getSelectionModel().deselectAll();
		}
	},
	add: function() {
		this.callParent(arguments);
	},
	remove: function() {
		this.callParent(arguments);
	},
	getValue: function() {
		return this.value;
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
