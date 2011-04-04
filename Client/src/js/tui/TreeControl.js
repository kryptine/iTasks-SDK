Ext.ns('itasks.tui');

itasks.tui.TreeControl = Ext.extend(Ext.tree.TreePanel,{
	rootVisible: false,
	loader: new Ext.tree.TreeLoader({preloadChildren: true}),
	
	initComponent : function(){
		this.msgTarget = 'side';
		this.hideLabel = this.fieldLabel == null;
		this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		this.root = {
			xtype: 'treenode',
			text:'tree',
			children: this.tuiTree
		};
		
		itasks.tui.TreeControl.superclass.initComponent.apply(this,arguments);
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
		this.on('click',function(node){
			if(node.leaf){
				this.fireEvent('tuichange',this.name,node.attributes.index.toString());
			}
		},this);
	},
	afterRender : function(){
		itasks.tui.TreeControl.superclass.afterRender.call(this);
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
		if(sel != "")
			this.getRootNode().eachChild(selNode,this);
		else
			this.getRootNode().eachChild(unselNodes,this);
	},
	setError: function(msg){		
		if(msg == "")
			itasks.tui.common.clearError(this);
		else
			itasks.tui.common.markError(this,msg);
	},
	setHint: function(msg){
		if(msg == "")
			itasks.tui.common.clearHint(this);
		else
			itasks.tui.common.markHint(this,msg);
	}
});

Ext.reg('itasks.tui.Tree', itasks.tui.TreeControl);