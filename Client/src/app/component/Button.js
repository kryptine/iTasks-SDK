Ext.define('itasks.component.Button',{
	extend: 'Ext.Button',
	mixins: ['itasks.mixin.Editable'],
	alias: 'widget.itasks.button',
	
	initComponent: function() {
		this.callParent(arguments);
		
		if(this.actionButton) {
			this.addEvents('commit');
			this.enableBubble('commit');
		} else {
			this.addEvents('change');
		}
	},
	onClick: function() {
		if(this.actionButton) {
			this.fireEvent('commit',this.taskId,this.name);
		} else {
			this.fireEvent('change');
		}
	},
	getEditValue: function() {
		return 'clicked';
	}
});
