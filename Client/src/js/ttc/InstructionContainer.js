Ext.ns('itasks.ttc');

itasks.ttc.InstructionContainer = Ext.extend(itasks.ttc.TTCBase,{
	
	initComponent: function() {
		this.cls = 'TTCInstructionContainer';
		itasks.ttc.InstructionContainer.superclass.initComponent.apply(this,arguments);
	},
	buildComponents: function(data) {
		this.interactionpanel = {
			xtype: 'panel',
			unstyled: true,
			html: this.context,
			cls: 'TTCInstructionContainer-Context',
			buttons: [
				{ xtype: 'itasks.ttc.Button'
				, id: 'tf-'+this.taskId+'-action-0'
				, action: 'ok'
				, text: 'Done'
				, disabled: false
				, iconCls: 'icon-ok'
				}]
		};
	}
});

Ext.reg('itasks.ttc.instruction',itasks.ttc.InstructionContainer);