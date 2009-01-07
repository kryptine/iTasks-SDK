/**
* This panel shows a list with work that a user has to do.
*/
Ext.ns('itasks');

itasks.WorkListPanel = Ext.extend(Ext.ux.grid.livegrid.GridPanel, {

	applicationPanel: undefined,

	workStore: new Ext.ux.grid.livegrid.Store({
		autoLoad: true,
		bufferSize: 300,
		sortInfo: {
			field: 'taskid',
			direction: 'ASC'
		},
		proxy: new Ext.data.HttpProxy({
			url: 'handlers/work/list'
		}),
		reader: new Ext.ux.grid.livegrid.JsonReader({
				root: 'worklist',
				totalProperty: 'total',
				successProperty: 'success'
			},[
				{name: 'taskid'},
				{name: 'subject'},
				{name: 'processname'},
				{name: 'delegatorName'},
				{name: 'priority'},
				{name: 'timestamp'},
				{name: 'tree_path'},
				{name: 'tree_last'},
				{name: 'tree_icon'}
			])
	}),	
	workView: new Ext.ux.grid.livegrid.GridView({
		nearLimit: 100,
		loadMask: { msg: 'Please wait...'}
	}),
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
			html += '<div class="treegrid-label" style="left: ' + (level + 2) * 16 + 'px">' + label + '</div>';
			
			return html;
		};
	
		Ext.apply(this, {
			border: false,
			store: this.workStore,
			view: this.workView,
			selModel: new Ext.ux.grid.livegrid.RowSelectionModel(),
			columns: [
				{id: 'taskid', header: 'Task', dataindex: 'taskid', renderer: treeRenderer, width: 200 },
				{id: 'subject', header: 'Subject', dataIndex: 'subject', width: 100},
				{id: 'processname', header: 'Process', dataindex: 'processname', width: 100},
				{id: 'delegatorName', header: 'From', dataIndex: 'delegatorName', width: 100 },
				{id: 'priority', header: 'Priority', dataindex: 'priority', renderer: itasks.util.formatPriority, width: 100 },
				{id: 'timestamp', header: 'Date', dataIndex: 'timestamp', renderer: itasks.util.formatDate, width: 100}
			],
			viewConfig: {
				forceFit: true,
				emptyText: 'There is no unfinished work.',
				deferEmptyText: false
			},
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
					click : { 
						scope: this,
						fn: function (btn) {
							this.refresh();
						}
					}
				}
			}]
			/*,
			bbar: new Ext.ux.grid.livegrid.Toolbar({
				view: this.workView,
				displayInfo: true
			})*/
		});
		itasks.WorkListPanel.superclass.initComponent.apply(this, arguments);
		
		//Check session error responses
		this.store.on('loadexception',function() {
			this.applicationPanel.checkSessionResponse(this.store.reader.jsonData);
		},this);
	},
	setApplicationPanel: function(panel) {
		this.applicationPanel = panel;
	},
	/*
	* Return the taskid of the selected row
	*/
	getTaskId: function (index) {
		return this.store.getAt(index).data.taskid;
	},
	/*
	* Return all task information of the selected row
	*/
	getTaskInfo: function (index) {
		return this.store.getAt(index).data;
	},
	/*
	* Refresh the list
	*/
	refresh: function () {
		this.store.load({
			params: this.applicationPanel.addSessionParam({})
		});
	}
});

Ext.reg('itasks.worklist',itasks.WorkListPanel);
