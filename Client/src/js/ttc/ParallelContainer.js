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
		
		this.control = new itasks.ttc.parallel.Control({subtaskInfo: this.subtaskInfo, description: this.description, label: this.label});
		this.add(this.control);
		
		for(var i=0, len = content.length; i<len;i++){

			Ext.apply(content[i],{
				title : 'Sub task '+content[i].subtaskId,
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
		
		Ext.apply(this,{
			subtaskId: 0,
			unstyled: true,
			bodyStyle: 'padding: 10px',
			iconCls : 'icon-overview',
			title: 'Overview',	
			cls: 'ParallelControlContainer',
			layout: 'anchor',
			items: [
				{ xtype: 'panel'
				, cls: 'task-description ParallelControlDescription'
				, width: 700
				, unstyled: true
				, html: this.label
				},
				{ xtype: 'panel'
				, items: [
					{ xtype: 'panel'
					, unstyled: true
					, html: this.description
					, bodyStyle: 'padding: 4px'
					},
					this.grid
				]
				, cls: 'ParallelControlPanel'
				, width: 700
				, unstyled: true
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
				{header: 'Subject',		   		dataIndex: 'subject', width: 150},
				{header: 'Delegated To', 		dataIndex: 'delegatedTo', renderer: Ext.util.Format.htmlEncode, width: 100},
				{header: 'Description',			dataIndex: 'description', width: 294},
				{header: 'Task Id', 				dataIndex: 'taskId', hidden: itasks.app.debug, width: 80}
			]
		});
		this.grid = new Ext.grid.GridPanel({
			store : store,
			disableSelection: true,
			colModel: col,	
			width: '100%',
			height: 250,
			unstyled: true,
			bodyStyle: 'border-top: 1px solid  #99BBE8'
		});
		
		this.grid.on("rowdblclick",function(grid,row,e){
			var store = grid.store;
			var rec = grid.store.getAt(row);
			var staskId = rec.data.subtaskId;
			var ct = this.findParentByType(itasks.ttc.ParallelContainer);
	
			ct.setActiveSubtask(staskId);
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
	}
});

Ext.reg('itasks.ttc.parallel',itasks.ttc.ParallelContainer);
Ext.reg('itasks.ttc.parallel.control',itasks.ttc.parallel.Control);