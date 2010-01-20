/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.grid.GridPanel, {

	initComponent: function () {
	
		var treeRenderer = function (label, meta, record) {
			var html = "";
			var level = record.data.tree_path.length;
			
			//Disable margins
			meta['attr'] = 'style="margin: 0px; padding: 0px"';
			
			//Create path
			for(var i = 0; i < level; i++) {
				if(record.data.tree_path[i]) {
					html += '<div class="treegrid treegrid-line" style="left: ' + i * 16 + 'px"></div>';
				} else {
					html += '<div class="treegrid treegrid-empty" style="left: ' + i * 16 + 'px"></div>';
				}
			}
			//Add elbow
			if(record.data.tree_last) {
				html += '<div class="treegrid treegrid-last" style="left: ' + level * 16 + 'px"></div>';
			} else {
				html += '<div class="treegrid treegrid-middle" style="left: ' + level * 16 + 'px"></div>';
			}
			//Add icon
			html += '<div class="treegrid treegrid-icon icon-' + record.data.tree_icon +'" style="left: ' + (level + 1) * 16 + 'px"></div>';
			
			//Add label
			if(record.data.tree_new) {
				html += '<div class="treegrid-label" style="font-weight: bold; left: ' + (level + 2) * 16 + 'px">' + label + '</div>';
			} else {
				html += '<div class="treegrid-label" style="left: ' + (level + 2) * 16 + 'px">' + label + '</div>';
			}
			return html;
		};
	
	
		this.workStore = new Ext.data.Store({
			autoLoad: false,
			bufferSize: 300,
			url: itasks.config.serverUrl + "/work/list",
			reader: new Ext.data.JsonReader({
					root: 'worklist',
					totalProperty: 'total',
					successProperty: 'success',
					fields: [
						{name: 'subject'},
						{name: 'priority'},
						{name: 'progress'},
						{name: 'delegatorName'},
						{name: 'timestamp'},
						{name: 'latestExtEvent'},
						{name: 'deadline'},
						{name: 'tree_path'},
						{name: 'tree_last'},
						{name: 'tree_icon'},
						{name: 'tree_new'},
						{name: 'taskid'}
					]})
			});
		
		this.workView = new Ext.grid.GridView({
			deferEmptyText: true,
			emptyText: 'There is no unfinished work.',
			nearLimit: 100,
			loadMask: { msg: 'Please wait...'}
		});
	
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			view: this.workView,
			selModel: new Ext.grid.RowSelectionModel(),
			columns: [
				{id: 'subject', header: 'Subject', dataindex: 'taskid', renderer: treeRenderer, width: 200},		
				{id: 'priority', header: 'Priority', dataindex: 'priority', renderer: itasks.util.formatPriority, width: 100},
				{id: 'progress', header: 'Progress', dataindex: 'progress', renderer: itasks.util.formatProgress, width: 100},
				{id: 'delegatorName', header: 'Managed by', dataIndex: 'delegatorName', width: 100},
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', renderer: itasks.util.formatDate, width: 120},
				{id: 'latestExtEvent', header: 'Latest Ext Event', dataIndex: 'latestExtEvent', renderer: itasks.util.formatDate, width: 120},
				{id: 'deadline', header: 'Deadline', dataIndex: 'deadline', renderer: itasks.util.formatDeadline, width: 100}
			],
			autoExpandColumn: 'subject',
			enableColumnMove: false,
			enableHdMenu: false,
			stripeRows: true,
			tbar: [{
				id: 'refreshbutton',
				xtype: 'tbbutton',
				text: 'Refresh worklist',
				iconCls: 'x-tbar-loading',
				listeners: {
					click: { 
						scope: this,
						fn: function (btn) {
							this.refresh();
						}
					}
				}	
			},'->',{
				id: 'userdisplay',
				xtype: 'label',
				text: 'Welcome ' + itasks.app.displayName
			},{
				id: 'logoutbutton',
				xtype: 'tbbutton',
				text: 'Logout',
				iconCls: 'icon-logout',
				listeners: {
					click: {
						scope: this,
						fn: function() {
							this.findParentByType(itasks.ApplicationPanel).logout();
						}	
					}
				}
			}]
		});
		
		itasks.WorkListPanel.superclass.initComponent.apply(this, arguments);
		this.addEvents("workListRefreshed");
		
		//Check session error responses
		this.store.on('loadexception',function() {
			if(this.store.reader.jsonData.error) {
				itasks.app.restart(this.store.reader.jsonData.error);
			}
		},this);
	
		this.startAutoRefresh();
	},
	
	/*
	* Return the taskid of the selected row
	*/
	getTaskId: function (index) {
		return this.store.getAt(index).data.taskid;
	},
	/*
	* Refresh the list
	*/
	refresh: function () {
		if(this.store != null){
			this.store.load({
				params: {_session: itasks.app.session}
			});
			
			this.fireEvent("workListRefreshed", this);
		}
	},
	
	/* 
	* Start the timed task for auto-refreshing.
	*/
	startAutoRefresh: function(){
		if(itasks.config.autoRefresh){
		
			var parent = this;
		
			Ext.TaskMgr.start({
				run: function(){
					parent.refresh();
				},
				interval: itasks.config.refreshRate
			})
		}
	},
});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
