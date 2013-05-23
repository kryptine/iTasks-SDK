Ext.define('itwc.component.choice.Grid',{
	extend: 'Ext.grid.Panel',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_grid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	viewConfig: {loadMask: false},

	width: 'flex',
	height: 'flex',
	minWidth: 400, //'wrap',	
	minHeight: 100, //'wrap',

	editBufferTime: 0,

	options: [],
	columns: [],
	value: null,
	border: false,

	initComponent: function() {

		var me = this,
			fields = [], 
			columns = [], i;

		//Setup columns			
		for(i = 0; i < me.columns.length; i++) {
			columns[i] = {text: me.columns[i], dataIndex: i};
			fields[i] = {name: i, type: 'string'};
		}
		me.columns = columns;

		//Fill store with data
		this.store = Ext.create('Ext.data.Store',{
			fields: fields,
			data: {'options': me.options},
			proxy: { type: 'memory', reader: {type: 'json', root: 'options'}}
		});

		me.callParent(arguments);
		me.on('itemclick', me.onItemClick, me);		
		me.initEditable();
	},
	afterRender: function() {
		var me = this;
		me.callParent(arguments);
		
		if(Ext.isArray(me.value)) {
			me.setValue(me.value.length ? me.value[0] : -1);
		}
		if(Ext.isNumber(me.value) && me.value >= 0) {
			me.setValue(me.value);
		}
	
	},
	onItemClick: function(view,rec) {
		this.value = [this.store.indexOf(rec)];
		this.fireEvent('change');	
	},
	setValue: function(value) {
		if(Ext.isArray(value) && value.length) {
			value = value[0];
		}
		if(Ext.isNumber(value) && value < this.store.count() && value >= 0) {
			this.value = value;
			this.getSelectionModel().select(value);		
		} else {
			this.getSelectionModel().deselectAll();
		}
	},
	setOptions: function(options) {
		var me = this,
			numOptions = options.length;
			
		me.store.removeAll();
		me.store.insert(0,options);
		
		if(Ext.isNumber(me.value) && me.value >= 0 && me.value < numOptions) {
			me.setValue(me.value);
		}
	},
	getValue: function() {
		return this.value;
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
