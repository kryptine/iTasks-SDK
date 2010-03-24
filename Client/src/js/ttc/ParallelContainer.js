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
		
		itasks.ttc.ParallelContainer.superclass.initComponent.apply(this,arguments);	
		
		this.initContent(this.content);
		this.on('tabchange',function(panel,tab){
			this.activeSubtaskId = tab.subtaskId;
		});
	},
	
	initContent: function(content){
		
		this.control = new itasks.ttc.parallel.Control({subtaskInfo: this.subtaskInfo});
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
		
		
		content = content.filter(function (val) { 
			if(val == "done" || val == "redundant") return false;
			else return true;
		});
		
		for(var i=0; i < content.length; i++){
			for(var j=i; j<(this.items.length-1); j++){
				if(content[i].taskId == this.items.get(j+1).taskId) break;
			}
			
			for(var k=0; k < (j-i); k++){
				this.remove(i+1,true);
			}
			
			if(i<(this.items.length-1)){
				this.items.get(i+1).update(content[i]);
			}else{
				
				Ext.apply(content[i],{
					title : 'Subtask '+content[i].subtaskId,
					iconCls: 'icon-task'
				});
				
				this.insert(j+1,content[i]);
			}
		}
		
		var trailing = this.items.length - content.length - 1;
		
		for(var i=0; i<trailing; i++){
			this.remove(this.items.length-1,true);
		}

		this.doLayout();
		this.setActiveSubtask(this.activeSubtaskId);
	},
	
	setActiveSubtask : function(id){				
		var t = 0;
		
		for(var i=0; i<this.items.length; i++){
			if(this.items.get(i).subtaskId == id){ t=i; break; }
		}		
		
		this.activeSubtaskId = id;
		this.setActiveTab(t);
	}
});

/*--------------------------------------------------------------------------------------------------------------------------------------------------------
 * Subcomponents
 *--------------------------------------------------------------------------------------------------------------------------------------------------------*/
 
Ext.ns('itasks.ttc.parallel');

itasks.ttc.parallel.Control = Ext.extend(Ext.Panel,{

	initComponent : function(){
		var store = new Ext.data.JsonStore({
			autoDestroy: true,
			root: 'subtasks',
			fields: [
				{name: 'finished', type: 'bool'},
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
				{header: 'Nr.',			  			dataIndex: 'subtaskId', width: 75, renderer: this.renderId},
				{header: 'Subject',		   		dataIndex: 'subject', width: 200},
				{header: 'Task Id', 				dataIndex: 'taskId', hidden: itasks.app.debug, width: 120},
				{header: 'Delegated To', 		dataIndex: 'delegatedTo', renderer: Ext.util.Format.htmlEncode, width: 120},
				{header: 'Description',			dataIndex: 'description', width: 400}
			]
		});
		var grid = new Ext.grid.GridPanel({
			store : store,
			disableSelection: true,
			colModel: col,	
			width: '100%',
			height: 250,
		});
		
		Ext.apply(this,{
			subtaskId: 0,
			unstyled: true,
			bodyStyle: 'padding: 10px',
			iconCls : 'icon-overview',
			title: 'Overview',	
			layout: 'form',
			items: [
				{ xtype: 'panel'
				, html: this.label
				, bodyCssClass: 'task-description'
				}
				, grid			
			]
		});
		
		grid.on("rowdblclick",function(grid,row,e){
			var store = grid.store;
			var rec = grid.store.getAt(row);
			var staskId = rec.data.subtaskId;
			var ct = this.findParentByType(itasks.ttc.ParallelContainer);
	
			ct.setActiveSubtask(staskId);
		},this);
		
		itasks.ttc.parallel.Control.superclass.initComponent.apply(this,arguments);
		this.updateStore(this.subtaskInfo);
	},
	
	update: function(subtaskinfo){
		this.updateStore(subtaskinfo);
	},
	
	updateStore: function(records){
		var grid = this.getComponent(1);
		var store = grid.store;
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
	}
});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
Ext.reg('itasks.ttc.parallel.control',itasks.ttc.parallel.Control);