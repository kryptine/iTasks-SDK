Ext.ns('itasks.tui');

itasks.tui.ListContainer = itasks.tui.extendContainer(Ext.Container,{
	defaultWidth: ['FillParent',1,'ContentSize'],
	defaultHeight: ['WrapContent',0],
	
	initComponent: function(){
		itasks.tui.container.initComponent.apply(this,arguments);
		
		this.on('afterlayout', function(ct,cmp,pos) { 
			if(this.staticDisplay) return;
		
			for(var i=0; i<this.items.length; i++){
				this.items.get(i).toggleLastItem(i == this.items.length-1);
			}
			
			if(this.sbExpanded) this.expandSidebar(); //redo opening of sidebar, so the new toolboxes are shown as well..
		});
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	afterRender : function(ct,position){
		this.extSuperclass.afterRender.call(this,ct,position);
		this.initSidebar();
	},
	
	getSidebarEl : function(){		
		if(!this.sidebarEl){
			this.sidebarEl = this.sidebarCt.createChild({cls: 'list-sidebar-bg', id: this.id+'-sidebar-bg', html: ' '});
		}	
		
		return this.sidebarEl;
	},
	
	getExpandedEl : function(){
		if(!this.expandedEl){
			this.expandedEl = this.sidebarCt.createChild({cls: 'list-sidebar-expanded x-layout-split-east', id: this.id+'-expanded', html: ' '});
			var mini = this.expandedEl.createChild({cls: 'x-layout-mini x-layout-mini-east list-sidebar-control', id: this.id+'-expanded-mini', html: ' '});
			
			this.expandedEl.addClassOnOver('x-layout-collapsed-over');
			this.expandedEl.on('click',this.collapseSidebar, this, {stopEvent:true});
			
			this.expandedEl.hide();
		}
		
		return this.expandedEl;
	},
	
	getCollapsedEl : function(){
		if(!this.collapsedEl){
			
			this.collapsedEl = this.sidebarCt.createChild({cls: 'list-sidebar-collapsed x-layout-cmini-east', id: this.id+'-collapsed', html: ' '});	
			var mini = this.collapsedEl.createChild({cls: 'x-layout-mini x-layout-mini-east list-sidebar-control', id: this.id+'-collapsed-mini', html: ' ',});
			
			this.collapsedEl.addClassOnOver('x-layout-collapsed-over');
			this.collapsedEl.on('click', this.expandSidebar, this, {stopEvent: true});	
		}
		
		return this.collapsedEl;
	},
	
	initSidebar : function(){		
		if(this.staticDisplay) return;
		
		this.sidebarCt = this.el.createChild({cls: 'list-sidebar', id: this.id+'-sidebar'});
		this.getCollapsedEl();	
	},
	
	collapseSidebar : function() {
		var ct = this;
		
		var x0 = ct.sidebarCt.getX();
		var sb = ct.getSidebarEl();
		var ce = ct.getCollapsedEl();
		var ee = ct.getExpandedEl();
		
		ee.hide();
		
		for(var i=0; i<ct.items.length; i++){
			var item = ct.items.get(i);
			item.collapseToolbox();
		}

		this.sidebarCt.shift({width: 5, callback: function() { ce.slideIn('r',{ duration: 0.20 }); }});
		this.sbExpanded = false;
	},
	
	expandSidebar : function() {
		var ct = this;
		
		var x0 = ct.sidebarCt.getX();
		var sb = ct.getSidebarEl();
		var ce = ct.getCollapsedEl();
		var ee = ct.getExpandedEl();
		
		ce.hide();
		this.sidebarCt.shift({ width: 85, 
		callback: function() 
			{	ee.show();	
				for(var i=0; i<ct.items.length; i++){					
					var item = ct.items.get(i);
					item.expandToolbox();
				}			
			}
		});	
		
		this.sbExpanded = true;
	}
});

Ext.ns('itasks.tui.list');

itasks.tui.list.ListItemControl = Ext.extend(Ext.Container,{
	layout: 'vbox',
	initComponent : function(){	
		this.cls = (this.index % 2) == 0 ? "list-item-light" : "list-item-dark";
	
		itasks.tui.list.ListItemControl.superclass.initComponent.apply(this,arguments);
		
		this.addEvents('tuichange');
		this.enableBubble('tuichange');
	},
	
	expandToolbox : function(){
		if(this.isLast || this.isExpanded) return;
		
		if(!this.toolbox){
			this.toolbox = this.el.createChild({ cls: 'list-toolbox' });
			
			this.upBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-up' });
			this.dnBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-down', style: 'margin-right: 4px' });
			this.addBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-add' });
			this.remBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-rem' });
			
			this.upBtn.on('click',this.handleClick.createDelegate(this,['mup']));
			this.dnBtn.on('click',this.handleClick.createDelegate(this,['mdn']));
			this.addBtn.on('click',this.handleClick.createDelegate(this,['add']));
			this.remBtn.on('click',this.handleClick.createDelegate(this,['rem']));
		}
			
		this.toolbox.fadeIn({ duration: 0.25 });	
		this.isExpanded = true;
	},
	
	collapseToolbox : function(){
		if(this.isLast || !this.toolbox || !this.isExpanded) return;
		
		this.toolbox.fadeOut({ duration: 0.25 });
		this.isExpanded = false;	
	},
	
	handleClick: function(action){
		var parent = this.findParentByType(itasks.tui.ListContainer);
		this.fireEvent('tuichange',parent.taskId,parent.name, action + "_" + this.index);
	},
	
	toggleLastItem : function(isLast){
		if(isLast){
			this.addClass('list-last-item');
			this.isLast = true;
		}else{
			this.removeClass('list-last-item');
			this.isLast = false;
		}
	},
	
	doTUILayout: function(fillW,fillH) {
		var minSize = this.getMinTUISize();
		var fillW = Ext.isDefined(fillW) ? fillW : minSize.width;
		var fillH = Ext.isDefined(fillH) ? fillH : minSize.height;
	
		this.suspendEvents();
		this.setSize(fillW,fillH);
		this.resumeEvents();
		
		this.get(0).doTUILayout(fillW,fillH);
	},
	getTUISize: function() {
		return this.get(0).getTUISize();
	},
	getMinTUISize: function() {
		return this.get(0).getMinTUISize();
	}
});

Ext.reg('itasks.tui.List', itasks.tui.ListContainer);
Ext.reg('itasks.tui.list.Item', itasks.tui.list.ListItemControl);