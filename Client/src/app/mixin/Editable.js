/*
* 'Editable' components are components that represent an editable task from an iTasks perspective.
* Changes in their client-side state, are synced with the server as edit events. The component itself
* has to define the granularity of what constitutes an edit event.
*/

Ext.define('itasks.mixin.Editable',{
	
	editable: true,
	editBufferTime: 200,

	syncEditsEnabled: false,
	
	startSyncEdits: function () {
		var me = this;
	
		if(me.syncEditsEnabled)
			return;
		
		me.addEvents('edit');
		me.enableBubble('edit');
		
		me.addManagedListener(me,'change',function () {
			this.fireEvent('edit',this.taskId,this.getEditName(),this.getEditValue());
		},me,{buffer: me.editBufferTime});
		me.syncEditsEnabled = true;
	},
	getEditName: function() {
		return this.name;
	},
	getEditValue: function() {
		var v = this.getValue();
		if(v === "")
			return null;
		else
			return v;
	},
	setTaskId: function(taskId) {
		this.taskId = taskId;
	},
	setName: function(name) {
		this.name = name;
	}
});
