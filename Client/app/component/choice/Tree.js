Ext.define('itwc.component.choice.Tree',{
	extend: 'Ext.tree.Panel',
	mixins: ['itwc.component.edit.Editable'],
	alias: 'widget.itwc_choice_tree',
	rootVisible: false,

	width: 150,
	height: 200,
	editBufferTime: 0,

	initComponent: function() {	
		var me = this,
			store;	
		
		store = Ext.create('Ext.data.TreeStore',{
			root : {xtype: 'treenode', text: 'tree', children: me.options}
		});
		
		me.store = store;
		me.selectedNode = -1;
		
		me.callParent(arguments);
		
		me.addManagedListener(me,'itemclick',me.onItemClick,me);
		me.initEditable();
	},
	afterRender: function() {
		this.callParent(arguments);
		
		if(Ext.isNumber(this.value)){
			this.setValue(this.value);
		}
	},
	onItemClick: function(tree,record,item) {
		this.selectedNode = record.raw.value;
		this.fireEvent('change');
	},
	getValue: function() {
		return this.selectedNode;
	},
	setValue: function(value) {
		var node;
		
		if(Ext.isNumber(value)) {
			this.selectedNode = value;
			node = this.getRootNode().findChildBy(function(node) {return (node.raw.value == value);},this,true);
			if(node) {
				this.getSelectionModel().select(node);
			}
		} else {
			this.selectedNode = -1;
			this.getSelectionModel().deselectAll();
		}
	},
	onDestroy: function() {
		this.store.destroy();
		this.callParent(arguments);
	}
});
