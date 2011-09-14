Ext.define('itasks.component.Grid',{
	extend: 'Ext.grid.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.igrid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	value: null,
	
	initComponent: function() {
		var headers = this.headers;
		var cells = this.cells;
		var fields = [];
		var i;

		delete(this.headers);
		delete(this.cells);
		
		//Setup columns
		this.columns = [];
		for(i = 0; i < headers.length; i++) {
			this.columns[i] = {text: headers[i], dataIndex: i};
			fields[i] = {name: i, type: 'string'};
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
		if(Ext.isNumber(value)) {			
			this.getSelectionModel().select(value);
		} else {
			this.getSelectionModel().deselectAll();
		}
	},
	getValue: function() {
		return this.value;
	}
});