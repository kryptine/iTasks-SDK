Ext.define('itwc.component.choice.Grid',{
	extend: 'Ext.grid.Panel',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_grid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	viewConfig: {loadMask: false},

    itwcWrapWidth: 400,
    itwcWrapHeight: 100,

	itwcWidth: 'flex',
	itwcHeight: 'flex',
	itwcMinWidth: 400,//'wrap',	
	itwcMinHeight: 100,//'wrap',

	editBufferTime: 0,

	options: [],
	columns: [],
	value: null,
	border: false,

	initComponent: function() {

		var me = this,
			fields = [], 
			columns = [], i;

        me.initSize();
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
		me.on('select', me.onSelect, me);		
        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            me.on('itemdblclick', me.onItemDblClick, me);
        }
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
	onSelect: function(view,rec) {
		this.value = [this.store.indexOf(rec)];
		this.fireEvent('change');	
	},
    onItemDblClick: function() {
        var me = this;

        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            itwc.global.controller.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);
        }
    },
	setValue: function(value) {
		var me = this;

		if(Ext.isArray(value) && value.length) {
			value = value[0];
		}
		if(Ext.isNumber(value) && value < me.store.count()) {
			me.value = value;
			me.getSelectionModel().select(value);		
		} else {
			me.getSelectionModel().deselectAll();
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
