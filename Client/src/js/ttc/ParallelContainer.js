Ext.ns('itasks.ttc');

itasks.ttc.ParallelContainer = Ext.extend(Ext.Panel, {
	
	control: null,	
	taskId: null,
	id: null,
		
	initComponent : function(){
		
		this.initGrid();
		
		Ext.apply(this,
		{ unstyled: true 
		, autoScroll: true
		, cls: 'ParallelControlContainer'
		, items: [
			{ cls: 'task-description ParallelControlDescription'
			, unstyled: true
			, html: this.label
			, width: 720
			},
			{ bodyStyle: 'padding: 4px'
			, items : [
				{ unstyled: true
				, html: this.description
				, bodyStyle: 'padding-bottom: 4px'
				},
				this.grid
			]
			, cls: 'ParallelControlPanel'
			, width: 720
			, unstyled: true
			}
		]		
		});
		
		if (Ext.isDefined(this.headerButton))
			this.tbar = [this.headerButton];
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
		this.update(this);
	},
	
	initGrid : function() {
		var store = new Ext.data.JsonStore({
			autoDestroy : true,
			root: 'subtasks',
			fields: [
				{name: 'finished', type: 'bool'},
				//'properties',
				'taskId',
				'subject',
				'delegatedTo',
				'progress',
				//'subtaskId',
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
				//{header: 'Nr.',			  			dataIndex: 'subtaskId', width: 34, renderer: this.renderId},
				//{header: 'Priority',				dataIndex: 'properties', width: 70, renderer: this.renderPriority},
				{header: 'Subject',		   		dataIndex: 'subject', width: 250},
				{header: 'Delegated To', 		dataIndex: 'delegatedTo', editor: new itasks.tui.UsernameControl({preventMark: true}), width: 150},
				{header: 'Description',			dataIndex: 'description', width: 180},
				{header: 'Task Id', 				dataIndex: 'taskId', hidden: itasks.app.debug, width: 85}				
			]
		});
		
		this.grid = new Ext.grid.EditorGridPanel({
			store : store,
			colModel: col,	
			width: '100%',
			height: 250,
			clicksToEdit: 'auto'
		});
		
		this.grid.on('afteredit', this.handleChange, this);
	},
	
	handleChange : function(edit){
		console.log(edit);
		this.grid.store.commitChanges();
	},
	
	update : function(data){
		var store = this.grid.store;
		store.loadData({subtasks : data.subtaskInfo},false);
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
				, width: 720
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
				, width: 720
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
		{ width: 745
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