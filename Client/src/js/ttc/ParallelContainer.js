Ext.ns('itasks.ttc');

itasks.ttc.ParallelContainer = Ext.extend(itasks.ttc.TTCBase, {
	
	control: null,	
	taskId: null,
	id: null,
		
	initComponent : function(){
		this.cls = 'TTCParallelControlContainer';
		
		//Create a json store for holding the information of the parallel subtasks
		this.store = new Ext.data.JsonStore({
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
		this.store.loadData({subtasks: this.subtaskInfo},false);
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
	
		//Bubble the event to trigger viewing a task result
		this.addEvents('taskresult');
		this.enableBubble('taskresult');
		
		for(var i=0; i < this.content.length; i++) {
			return this.add(this.content[i]);
		}
	},
	buildComponents: function(data) {
		if(this.content.length == 0){
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
			
			var cellActions = new Ext.ux.grid.CellActions({
				callbacks: {
					'icon-edit' : function(grid, record, action, value){
						var win = new itasks.ttc.parallel.ReassignWindow({initUser : value, taskId: record.data.taskId});
						win.show();
					}
				},
				align: 'right'
			});
						
			this.interactionpanel = {
				xtype: 'editorgrid',
				cls: 'TTCParallelControlPanel',
				border: false,
				store : this.store,
				colModel: col,	
				height: 250,
				plugins:[cellActions],
				autoExpandColumn: 2,
				clicksToEdit: 'auto',
				listeners:	{afteredit: {fn: this.handleChange, scope: this}
							,rowdblclick: {fn: this.handleDblClick, scope: this}
							}
			};
		}
	},
	handleChange : function(edit){
		this.grid.store.commitChanges();
	},
	handleDblClick : function(grid,row,e){
		var rec = grid.getStore().getAt(row);
		if(rec.data.finished){
			this.fireEvent('taskresult',rec.data.taskId);
		}
	},	
	update : function(data){
		//update the store
		this.store.loadData({subtasks : data.subtaskInfo},false);
		
		for(var i=0; i < data.content.length; i++) {
			this.get(i+2).update(data.content[i]);
		}
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

itasks.ttc.parallel.ReassignWindow = Ext.extend(Ext.Window,{
	
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
					handler: function(b,e) { b.findParentByType(itasks.ttc.parallel.ReassignWindow).close(); }
				  },{
					text: 'Ok',
					iconCls: 'icon-ok',
					handler: this.handleAssignment
				  }
				]
			},this.progress],
			defaults: {unstyled: true, bodyStyle: 'padding: 4px'}
		});
	
		itasks.ttc.parallel.ReassignWindow.superclass.initComponent.apply(this,arguments);
	},
	
	handleAssignment: function(button,evt){
		
		var win = this.findParentByType(itasks.ttc.parallel.ReassignWindow);
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
Ext.reg('itasks.ttc.parallel.reassign', itasks.ttc.parallel.ReassignWindow);
