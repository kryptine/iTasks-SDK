Ext.ns('itasks.tui');

itasks.tui.TreeControl = itasks.tui.extendControl(Ext.tree.TreePanel,{
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['WrapContent',0],
	rootVisible: false,
	border: false,
	loader: new Ext.tree.TreeLoader({preloadChildren: true}),
	
	initComponent : function(){
		this.root = {
			xtype: 'treenode',
			text:'tree',
			children: this.tuiTree
		};
		
		this.on('click',function(node){
			if(node.leaf){
				this.fireEvent('tuichange',this.taskId,this.name,node.attributes.index);
			}
		},this);
		
		itasks.tui.control.initComponent.apply(this,arguments);
	},
	afterRender : function(){
		this.extSuperclass.afterRender.apply(this,arguments);
		if(Ext.isNumber(this.value)){
			this.setValue(this.value);
		}
	},
	setValue : function(sel){
		var selNode = function(node){
			if(node.attributes.index == sel){
				this.selectPath(node.getPath());
				return false;
			} else {
				node.eachChild(selNode,this);
			}
		};
		var unselNodes = function(node){
			node.unselect();
			node.eachChild(unselNodes,this);
		};
		if(Ext.isNumber(sel))
			this.getRootNode().eachChild(selNode,this);
		else
			this.getRootNode().eachChild(unselNodes,this);
	},
	getMessageHandler: function() {
		return null;
	}
});

Ext.reg('itasks.tui.Tree', itasks.tui.TreeControl);