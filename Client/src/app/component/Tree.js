Ext.define('itasks.component.Tree',{
	extend: 'Ext.tree.Panel',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itree',
	rootVisible: false,
	selectedNode: -1,
	
	initComponent: function() {	
		this.root = {xtype: 'treenode',text: 'tree', children: this.tree};
		this.callParent(arguments);
		this.addListener('itemclick',this.onItemClick,this);
	},
	afterRender: function() {
		this.callParent(arguments);
		
		if(Ext.isNumber(this.value)){
			this.setValue(this.value);
		}
	},
	onItemClick: function(tree,record,item) {
		if(record.raw.leaf) {
			this.selectedNode = record.raw.value;
			this.fireEvent('change');
		}
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
	}
});