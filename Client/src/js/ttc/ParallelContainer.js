Ext.ns('itasks.ttc');

itasks.ttc.ParallelContainer = Ext.extend(Ext.TabPanel, {
	
	control: null,	
	taskId: null,
	id: null,
	
	activeSubtaskId: null,
		
	initComponent : function(){
		
		Ext.apply(this,
		{ activeTab: 0
		, items: []
		});
		
		if (Ext.isDefined(this.headerButton))
			this.tbar = [this.headerButton];
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
		
		this.initContent(this.content);
		this.on('tabchange',function(panel,tab){
			this.activeSubtaskId = tab.subtaskId;
		});
	},
	
	initContent: function(content){
		
		this.control = new itasks.ttc.parallel.Control({subtaskInfo: this.subtaskInfo, description: this.description, label: this.label});
		this.add(this.control);
		
		for(var i=0, len = content.length; i<len;i++){

			Ext.apply(content[i],{
				title : 'Subtask '+content[i].subtaskId,
				iconCls: 'icon-task'
			});
			
			this.add(content[i]);
		}	
	},
	
	update : function(data){
		this.updateSubtaskInfo(data.subtaskInfo);
		this.updateTabs(data.content);
		
	},
	
	updateSubtaskInfo : function(subtaskInfo){
		this.getComponent(0).update(subtaskInfo);
	},
	
	updateTabs : function(content){
		
		var astid = this.activeSubtaskId;
		
		content = content.filter(function (val) { 
			if(val == "done" || val == "redundant") return false;
			else return true;
		});
		
		for(var i=0; i < content.length; i++){
			
			Ext.apply(content[i],{
					title : 'Subtask '+content[i].subtaskId,
					iconCls: 'icon-task'
			});
			
			for(var j=i; j<(this.items.length-1); j++){
				if(content[i].taskId == this.items.get(j+1).taskId) break;
			}
			
			for(var k=0; k < (j-i); k++){
				this.remove(i+1,true);
			}
			
			if(i<(this.items.length-1)){
				if (this.items.get(i+1).getXType() == content[i].xtype){
					this.items.get(i+1).update(content[i]);
				}else{
					//if not same xtype - completely replace tab contents
					this.remove(i+1);
					this.insert(i+1,content[i]);
				}
			}else{			
				this.insert(j+1,content[i]);
			}
		}
		
		var trailing = this.items.length - content.length - 1;
		
		for(var i=0; i<trailing; i++){
			this.remove(this.items.length-1,true);
		}

		this.doLayout();
		this.setActiveSubtask(astid);
	},
	
	setActiveSubtask : function(id){				
		var t = 0;
		
		for(var i=0; i<this.items.length; i++){
			if(this.items.get(i).subtaskId == id){ t=i; break; }
		}		

		this.setActiveTab(t);
	}
});

/*--------------------------------------------------------------------------------------------------------------------------------------------------------
 * Subcomponents
 *--------------------------------------------------------------------------------------------------------------------------------------------------------*/
 
Ext.ns('itasks.ttc.parallel');

itasks.ttc.parallel.Control = Ext.extend(Ext.Panel,{

	initComponent : function(){
		
		this.initGrid();
		
		var ct = this;
		
		Ext.apply(this,{
			subtaskId: 0,
			unstyled: true,
			autoScroll: true,
			iconCls : 'icon-overview',
			title: 'Overview',	
			cls: 'ParallelControlContainer',
			layout: 'anchor',
			items: [
				{ xtype: 'panel'
				, cls: 'task-description ParallelControlDescription'
				, unstyled: true
				, html: this.label
				},
				{ xtype: 'panel'
				, bodyStyle: 'padding: 4px'
				, items: [
					{ xtype: 'panel'
					, unstyled: true
					, html: this.description
					, bodyStyle: 'padding-bottom: 4px'
					},
					this.grid
				]
				, cls: 'ParallelControlPanel'
				, unstyled: true
				, buttons: [
					{ xtype: 'button'
					, text: 'Manage selected subtask'
					, iconCls: 'icon-manage-process'
					, disabled: true
					, handler: function(){					
						var selected = ct.grid.getSelectionModel().getSelected();
						
						if(!selected){
							Ext.Msg.show(
								{ title: 'No selection'
								, msg: 'Please select the process you want to manage in the grid'
								, buttons: Ext.Msg.OK
								, icon: Ext.MessageBox.ERROR
								}
							);
							return;
						}
						
						var properties = selected.get('properties');
						if(properties == null){	return;	}
								
						var closeCB = function(){
							this.findParentByType(itasks.WorkPanel).refresh();
						}
								
						var window = new itasks.ttc.parallel.ManageWindow({properties: properties, parent: ct});
						
						window.on('close',closeCB);						
						window.show();
					  }
					}
				]
				}	
			]
		});
				
		itasks.ttc.parallel.Control.superclass.initComponent.apply(this,arguments);
		this.updateStore(this.subtaskInfo);
	},
	
	initGrid : function() {
		var store = new Ext.data.JsonStore({
			autoDestroy: true,
			root: 'subtasks',
			fields: [
				{name: 'finished', type: 'bool'},
				'properties',
				'taskId',
				'subject',
				'delegatedTo',
				'progress',
				'subtaskId',
				'description'
			]
		});
		
		var col = new Ext.grid.ColumnModel({
			defaults: {
				menuDisabled: true,
				sortable: true
			},
			columns: [
				{header: 'Done',     			dataIndex: 'finished', renderer: this.renderFinished, width: 36, resizable: false},
				{header: 'Nr.',			  			dataIndex: 'subtaskId', width: 34, renderer: this.renderId},
				{header: 'Priority',				dataIndex: 'properties', width: 70, renderer: this.renderPriority},
				{header: 'Subject',		   		dataIndex: 'subject', width: 130},
				{header: 'Delegated To', 		dataIndex: 'delegatedTo', renderer: Ext.util.Format.htmlEncode, width: 150},
				{header: 'Description',			dataIndex: 'description', width: 180},
				{header: 'Task Id', 				dataIndex: 'taskId', hidden: itasks.app.debug, width: 85}				
			]
		});
		
		this.grid = new Ext.grid.GridPanel({
			store : store,
			colModel: col,	
			width: '100%',
			height: 250
		});
		
		this.grid.on("rowdblclick",function(grid,row,e){
			var store = grid.store;
			var rec = grid.store.getAt(row);
			var staskId = rec.data.subtaskId;
			var ct = this.findParentByType(itasks.ttc.ParallelContainer);
	
			ct.setActiveSubtask(staskId);
		},this);
		
		this.grid.on('rowclick',function(grid,row,e){
			var selected = grid.getSelectionModel().getSelected();
			
			if(selected.get('properties')){
				grid.ownerCt.buttons[0].setDisabled(false);
			}else{
				grid.ownerCt.buttons[0].setDisabled(true);
			}
		},this);
	},
	
	update: function(subtaskinfo){
		this.updateStore(subtaskinfo);
	},
	
	updateStore: function(records){
		var store = this.grid.store;
		store.loadData({subtasks: records},false);
	},
	
	renderFinished: function(val,metadata,rec,row,col,store){
		if(val == false){
			return '<div style="text-align: center"><img src="skins/default/img/icons/hourglass.png" /></div>'
		}else{
			return '<div style="text-align: center"><img src="skins/default/img/icons/tick.png" /></div>'
		}
	},
	
	renderId: function(val,metadata,rec,row,col,store){
		var split = val.split(".");
		render = '<img style="width: '+(split.length-1)*5+'px" src="'+Ext.BLANK_IMAGE_URL+'"></span>';		
		render += val;
		return render;
	},
	
	renderPriority: function(val,metadata,rec,row,col,store){
		if(val != null){
			return itasks.util.formatPriority(val.managerProps.priority);
		}
	}

});


itasks.ttc.parallel.ManageWindow = Ext.extend(Ext.Window,{

	initComponent : function(){
		Ext.apply(this,
		{ width: 735
		, height: 300
		, layout: 'fit'
		, modal: true
		, closable: true
		, resizable: false
		, renderTo: this.parent.getEl()
		, constrain: true
		, items: [			
			{ xtype: 'itasks.ttc.proc-control'
			, taskId: this.properties.systemProps.processId
			, properties: this.properties
			}
		]
		//, title: 'Manage Process Properties'
		});
	
		//hack: make sure the ownerCt is set to reference the parent, so that findParentByType works.
		this.ownerCt = this.parent;
		
		itasks.ttc.parallel.ManageWindow.superclass.initComponent.apply(this,arguments);
	}

});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
Ext.reg('itasks.ttc.parallel.control',itasks.ttc.parallel.Control);