Ext.define('itwc.component.choice.Tree',{
	extend: 'Ext.tree.Panel',
	mixins: ['itwc.Sizeable','itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_tree',
	rootVisible: false,

    itwcWrapWidth: 150,
    itwcWrapHeight: 100,

	itwcWidth: 'flex',
	itwcHeight: 'flex',
	itwcMinWidth: 150,//'wrap',
	itwcMinHeight: 100,//'wrap',

	editBufferTime: 0,

	initComponent: function() {	
		var me = this,
			store;	
		
        me.initSize();
		store = Ext.create('Ext.data.TreeStore',{
			root : {xtype: 'treenode', text: 'tree', children: me.options}
		});
		
		me.store = store;
		me.selectedNode = -1;
		
		me.callParent(arguments);
		
		me.addManagedListener(me,'select',me.onSelect,me);
        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            me.addManagedListener(me,'itemdblclick', me.onItemDblClick, me);
        }
		me.addManagedListener(me,'beforeitemexpand',me.onItemExpand,me);
		me.addManagedListener(me,'beforeitemcollapse',me.onItemCollapse,me);
		me.initEditable();
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
		this.lastEditNo = itwc.global.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
		return false;
	},
	onItemCollapse: function(record) {
		var val = ["exp",record.raw && record.raw.value,false];
		this.lastEditNo = itwc.global.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
		return false;
	},
	onSelect: function(tree,record,item) {
		var value = record.raw && record.raw.value,
			val = ["sel",value,true];

		this.selectedNode = value;
		this.lastEditNo = itwc.global.controller.sendEditEvent(this.taskId,this.editorId,val);
		this.lastEditVal = val;
	},
    onItemDblClick: function() {
        var me = this;

        if(me.doubleClickAction && me.doubleClickAction.length == 2) {
            itwc.global.controller.sendActionEvent(me.doubleClickAction[0],me.doubleClickAction[1]);
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
