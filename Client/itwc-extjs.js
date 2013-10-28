//#### EXT COMPONENTS ####//
//TREE COMPONENT
Ext.define('itwc.component.choice.Tree',{
	extend: 'Ext.tree.Panel',
	rootVisible: false,

	editBufferTime: 0,

	initComponent: function() {	
		var me = this,
            itwcDiv = Ext.get(me.itwcCmp.domId),
			store;	
		
		store = Ext.create('Ext.data.TreeStore',{
			root : {xtype: 'treenode', text: 'tree', children: me.options}
		});
		
		me.store = store;
		me.selectedNode = -1;
		
        me.setSize(itwcDiv.getSize());
		me.callParent(arguments);

		me.addManagedListener(me,'select',me.onSelect,me);
        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            me.addManagedListener(me,'itemdblclick', me.onItemDblClick, me);
        }
		me.addManagedListener(me,'beforeitemexpand',me.onItemExpand,me);
		me.addManagedListener(me,'beforeitemcollapse',me.onItemCollapse,me);
	},
    afterRender: function() {
		var me = this;
		me.callParent(arguments);
		
		if(Ext.isArray(me.value)) {
			me.setValue(me.value.length ? me.value[0] : -1);
		}
		if(Ext.isNumber(me.value) && me.value >= 0 && me.value < numOptions) {
			me.setValue(me.value);
		}
	},
	onItemExpand: function(record) {
		var val = ["exp",record.raw && record.raw.value,true];
		this.lastEditNo = itwc.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
		return false;
	},
	onItemCollapse: function(record) {
		var val = ["exp",record.raw && record.raw.value,false];
		this.lastEditNo = itwc.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
		return false;
	},
	onSelect: function(tree,record,item) {
		var value = record.raw && record.raw.value,
			val = ["sel",value,true];

		this.selectedNode = value;
		this.lastEditNo = itwc.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
	},
    onItemDblClick: function() {
        var me = this;

        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            itwc.controller.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);
        }
    },
	getValue: function() {
		return this.selectedNode;
	},
	setValue: function(value) {
		var me = this,
            node;
		
        me.suspendEvents();
		if(Ext.isArray(value) && value.length) {
			value = value[0];
		}
		if(Ext.isNumber(value)) {
			me.selectedNode = value;
			node = me.getRootNode().findChildBy(function(node) {return (node.raw.value == value);},me,true);
			if(node) {
				me.getSelectionModel().select(node);
			}
		} else {
			me.selectedNode = -1;
			me.getSelectionModel().deselectAll();
		}
        me.resumeEvents();
	},
	setOptions: function(options) { //STILL BUGGY
		var me = this,
			root = me.store.getRootNode(),
			numOptions = options.length,
			i;

        me.suspendEvents();
		root.removeAll();
		for(i = 0; i < numOptions; i++) {
			root.appendChild(options[i]);
		}
        me.resumeEvents();
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
//GRID COMPONENT
Ext.define('itwc.component.choice.Grid',{
	extend: 'Ext.grid.Panel',

	alias: 'widget.itwc_choice_grid',
	forceFit: true,
	sortableColumns: false,
	enableColumnHide: false,
	enableColumnMove: false,
	viewConfig: {loadMask: false},

    editBufferTime: 0,

	options: [],
	columns: [],
	value: null,
	border: false,

	initComponent: function() {

		var me = this,
            itwcDiv = Ext.get(me.itwcCmp.domId),
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

        me.setSize(itwcDiv.getSize());
		me.callParent(arguments);
		me.on('select', me.onSelect, me);		
        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            me.on('itemdblclick', me.onItemDblClick, me);
        }
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

        itwc.controller.sendEditEvent(this.taskId,this.editorId,this.value);
	},
    onItemDblClick: function() {
        var me = this;

        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            itwc.controller.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);
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
