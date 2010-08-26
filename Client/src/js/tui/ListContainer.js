Ext.ns('itasks.tui');

itasks.tui.ListContainer = Ext.extend(Ext.Panel,{

	initComponent: function(){
		
		if(this.fieldLabel == null) delete this.fieldLabel;
		else this.fieldLabel = itasks.util.fieldLabel(this.optional,this.fieldLabel);
		
		Ext.apply(this,
		{ autoHeight: true
		, layout: 'anchor'
		, unstyled: true
		, cls: 'list'
		, autoWidth: true
		});	
		
		itasks.tui.ListContainer.superclass.initComponent.apply(this,arguments);
		
		this.on('afterlayout', function(ct,cmp,pos) { 
			if(this.staticDisplay) return;
		
			for(var i=0; i<this.items.length; i++){
				this.items.get(i).toggleLastItem(i == this.items.length-1);
			}
			
			if(this.sbExpanded) this.expandSidebar(); //redo opening of sidebar, so the new toolboxes are shown as well..
		});
	},
	
	afterRender : function(ct,position){
		itasks.tui.ListContainer.superclass.afterRender.call(this,ct,position);
		this.initSidebar();
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
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
	},
	
	setError: function(msg){
		if(this.staticDisplay) return;
		
		(function() {
			if(msg == "") this.clearError();
			else this.markError(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		if(msg == "") this.clearHint();
		else this.markHint(msg);
	},
	
	makeMessageField : function(){
		this.msgField = this.el.insertSibling({tag:'div'},'after');
		this.msgField.createChild({cls: 'x-constructor-panel-tc'});
		this.msgField.createChild({cls: 'list-msg-field'});
	},
	
	markHint : function (msg){
		if(this.rendered){
			if(!this.msgField) {
				this.makeMessageField();
			}else{
				this.msgField.show();
			}
			
			if(!this.hintIcon){
				var ct = this.msgField.child('[class = list-msg-field]');
				this.hintIcon = ct.createChild({cls: 'x-record-hint-icon'});
				this.hintIcon.setVisibilityMode(Ext.Element.DISPLAY);
			}
			
			this.hintIcon.dom.innerHTML = msg;
			this.hintIcon.setVisible(true);
		}
	},
	
	markError: function(msg){
		if(this.rendered){
			if(!this.msgField) {
				this.makeMessageField();
			}else{
				this.msgField.show();
			}
			
			if(this.hintIcon) this.hintIcon.hide();
			
			if(!this.errorIcon){
				var ct = this.msgField.child('[class = list-msg-field]');
				this.errorIcon = ct.createChild({cls: 'x-record-invalid-icon'});
				this.errorIcon.setVisibilityMode(Ext.Element.DISPLAY);
			}
			
			this.errorIcon.dom.innerHTML = msg;
			this.errorIcon.setVisible(true);
		}
	},
	
	clearHint : function(){
		if(this.hintIcon){
			this.hintIcon.setVisible(false);
			this.msgField.hide();
		}
	},
	
	clearError : function(){
		if(this.errorIcon){
			this.errorIcon.setVisible(false);
			this.msgField.hide();
		}
	}
});

Ext.ns('itasks.tui.list');

var T = itasks.tui.list;

T.ListItemControl = Ext.extend(Ext.Panel,{

	initComponent : function(){	
		Ext.apply(this,
		{ unstyled: true
		, autoHeight: true
		, autoWidth: true
		//, layout: 'form'
		, cls: ((this.index%2) == 0)?"list-item-light":"list-item-dark"
		});
			
		T.ListItemControl.superclass.initComponent.apply(this,arguments);
	},
	
	expandToolbox : function(){
		if(this.isLast || this.isExpanded) return;
		
		if(!this.toolbox){
			this.toolbox = this.el.createChild({ cls: 'list-toolbox' });
			
			this.upBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-up' });
			this.dnBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-down', style: 'margin-right: 4px' });
			this.addBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-add' });
			this.remBtn = this.toolbox.createChild({ cls: 'list-toolbox-button list-button-rem' });
			
			this.upBtn.on('click',this.handleClick.createDelegate(this,['mup',this.name,this.index]));
			this.dnBtn.on('click',this.handleClick.createDelegate(this,['mdn',this.name,this.index]));
			this.addBtn.on('click',this.handleClick.createDelegate(this,['add',this.name,this.index]));
			this.remBtn.on('click',this.handleClick.createDelegate(this,['rem',this.name,this.index]));
		}
			
		this.toolbox.fadeIn({ duration: 0.25 });	
		this.isExpanded = true;
	},
	
	collapseToolbox : function(){
		if(this.isLast || !this.toolbox || !this.isExpanded) return;
		
		this.toolbox.fadeOut({ duration: 0.25 });
		this.isExpanded = false;	
	},
	
	handleClick: function(action,name,index){
		var formCt = this.findParentByType(itasks.ttc.FormContainer);
		formCt.addUpdate(name,action+"_"+index);
		formCt.sendUpdates(false);	
	},
	
	toggleLastItem : function(isLast){
		if(isLast){
			this.addClass('list-last-item');
			this.isLast = true;
		}else{
			this.removeClass('list-last-item');
			this.isLast = false;
		}
	}
});

Ext.reg('itasks.tui.List', itasks.tui.ListContainer);
Ext.reg('itasks.tui.list.Item', itasks.tui.list.ListItemControl);