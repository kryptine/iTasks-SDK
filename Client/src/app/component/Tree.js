Ext.define('itasks.component.Tree',{
	extend: 'Ext.tree.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks_tree',
	rootVisible: false,

	initComponent: function() {	
		
		if(!this.width && !this.hflex) {
			this.hflex = 1;
			this.minWidth = 400;
		}
		
		var store = Ext.create('Ext.data.TreeStore',{
			root : {xtype: 'treenode', text: 'tree', children: this.tree}
		});
		
		
		this.store = store;
		this.selectedNode = -1;
		
		this.callParent(arguments);
		
		this.addManagedListener(this,'itemclick',this.onItemClick,this);
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
	getEditValue: function() {
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
