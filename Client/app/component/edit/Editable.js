/*
* 'Editable' components are components that represent an editable task from an iTasks perspective.
* Changes in their client-side state, are synced with the server as edit events. The component itself
* has to define the granularity of what constitutes an edit event.
*/
Ext.define('itwc.component.edit.Editable',{
	
	editable: true,
	editBufferTime: 50,

	syncEditsEnabled: false,
	
	initEditable: function() {
		var me = this;
	
		if(me.syncEditsEnabled)
			return;

		me.addEvents('edit');

		me.addManagedListener(me,'change',function () {
			var val = me.getEditorValue();
			me.lastEditNo = itwc.global.controller.sendEditEvent(me.taskId,me.getEditorId(),val);
			me.lastEditVal = val;
		},me,{buffer: me.editBufferTime});

		me.syncEditsEnabled = true;
	},
	getTaskId: function() {
		return this.taskId;
	},
	getEditorId: function() {
		return this.editorId;
	},
	getEditorValue: function() {
		var v = this.getValue();
		if(v === "")
			return null;
		else
			return v;
	},
	setTaskId: function(taskId) {
		this.taskId = taskId;
	},
	setEditorId: function(editorId) {
		this.editorId = editorId;
	},
	setEditorValue: function(value) {
		var receivedNo = itwc.global.controller.lastReceivedEventNo,	
			sentNo = this.lastEditNo || 0;

		//Don't consider outdated values
		if(receivedNo >= sentNo) {
			if(receivedNo == sentNo && value == this.lastEditVal) {
				//Ignore confirmations of changed values that we just told the server about
				return;
			}
			this.suspendEvents();
			this.setValue(value);
			this.resumeEvents();
		}
	}
});
