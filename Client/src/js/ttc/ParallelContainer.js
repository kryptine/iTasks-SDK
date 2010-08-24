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
		, width: 720
		, items: [
			{ cls: 'TTCSubject'
			, unstyled: true
			, html: this.subject
			},
			{ cls: 'TTCDescription'
			, unstyled: true
			, html: this.description
			},
			this.grid
		]		
		});
		
		if (Ext.isDefined(this.headerButton))
			this.tbar = [this.headerButton];
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
		this.update(this);
	},
	
	initGrid : function() {
		
		this.cellActions = new Ext.ux.grid.CellActions({
			callbacks: {
				'icon-edit' : function(grid, record, action, value){
					var win = new itasks.ttc.parallel.AssignWindow({initUser : value, taskId: record.data.taskId});
					win.show();
				}
			},
			align: 'right'
		});
		
		var store = new Ext.data.JsonStore({
			autoDestroy : true,
			root: 'subtasks',
			fields: [
				{name: 'finished', type: 'bool'},
				'taskId',
				'subject',
				'delegatedTo',
				'progress',
				'description'
			]
		});
		
		var col = new Ext.grid.ColumnModel({
			defaults: {
				menuDisabled: true,
				sortable: true,
				resizable: false
			},
			columns: [
				{header: 'Status', dataIndex: 'finished', renderer: this.renderFinished, width: 50},
				{header: 'Subject',	dataIndex: 'subject', width: 450},
				{header: 'Assigned to', dataIndex: 'delegatedTo', renderer: this.renderDelegated, cellActions:[{ iconCls: 'icon-edit', qtip: 'Re-asign the task to another user'}]}			
			]
		});
		
		this.grid = new Ext.grid.EditorGridPanel({
			cls: 'ParallelControlPanel',
			border: false,
			store : store,
			colModel: col,	
			width: 700,
			height: 250,
			plugins:[this.cellActions],
			autoExpandColumn: 2,
			clicksToEdit: 'auto'
		});
		
		this.grid.on('afteredit', this.handleChange, this);
		this.grid.on('rowdblclick', this.handleDblClick, this);
	},
	
	handleChange : function(edit){
		this.grid.store.commitChanges();
	},
	
	handleDblClick : function(grid,row,e){
		var rec = grid.getStore().getAt(row);
		var taskId = rec.data.taskId+'.0'; //add one shift as the result node is stored as a child of the process node
		var finished = rec.data.finished;
		
		if(finished){
			var worktabs = grid.findParentByType(itasks.WorkTabsPanel);
			var tab = worktabs.openResultTab(taskId);
		}
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
		
	renderPriority: function(val,metadata,rec,row,col,store){
		if(val != null){
			return itasks.util.formatPriority(val.managerProps.priority);
		}
	},
	
	renderDelegated: function(val,metadata,rec,row,col,store){
		if(rec.data.finished) metadata.css += 'hide-cell-action ';
		return Ext.util.Format.htmlEncode(val);
	}
});

Ext.ns('itasks.ttc.parallel');

itasks.ttc.parallel.AssignWindow = Ext.extend(Ext.Window,{
	
	initComponent: function(){
		
		this.ucontrol = new itasks.tui.UsernameControl({
			preventMark: true,
			value: this.initUser
		});
		
		this.progress = new Ext.ProgressBar({hidden: true});
		
		Ext.apply(this,{
			title: 'Re-assign task',
			modal: true,
			resizable: false,
			items: [{
				html: 'Please select a user to whom this task may be assigned.'
			},{
				xtype: 'form',
				items: [this.ucontrol],
				buttons: [{
					text: 'Cancel',
					iconCls: 'icon-cancel',
					handler: function(b,e) { b.findParentByType(itasks.ttc.parallel.AssignWindow).close(); }
				  },{
					text: 'Ok',
					iconCls: 'icon-ok',
					handler: this.handleAssignment
				  }
				]
			},this.progress],
			defaults: {unstyled: true, bodyStyle: 'padding: 4px'}
		});
	
		itasks.ttc.parallel.AssignWindow.superclass.initComponent.apply(this,arguments);
	},
	
	handleAssignment: function(button,evt){
		
		var win = this.findParentByType(itasks.ttc.parallel.AssignWindow);
		var val = win.ucontrol.getValue();
		var c = new Ext.data.Connection();
		
		c.on('beforerequest', function(conn,options){
			win.progress.show();
			win.progress.wait({text: 'Reassigning task...'});
		},this);
		
		c.on('requestcomplete', function(conn,response,options){
			var resp = Ext.decode(response.responseText);
			
			if(!resp.success){
				Ext.Msg.alert('Error',resp.error);
			}
		
			itasks.app.mainGui.refreshGUI();	
		},this);		
		
		c.on('requestexception', function(conn,response,options){
			Ext.Msg.alert('Error', 'Cannot reassign task. The task is either non existent or you don\'t have sufficient privileges');
		});
		
		c.request({
			url: itasks.config.serviceUrl+'/json/tasks/'+win.taskId+ '/managerProperties/worker',
			params: { session: itasks.app.session, update: Ext.encode(val)},
			callback: function(){
				win.close();
			}
		});		
	}
});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
Ext.reg('itasks.ttc.parallel.assign', itasks.ttc.parallel.AssignWindow);
