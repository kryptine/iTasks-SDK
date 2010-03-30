Ext.ns('itasks.ttc');

itasks.ttc.InstructionContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		
		Ext.apply(this,
		{ cls: 'InstructionContainer'
		, unstyled: true
		, taskUpdates : {}
		, items: [
			{ xtype: 'panel'
			, unstyled: true
			, html: this.label
			, width: 700	
			, cls: 'InstructionContainer-Description task-description'
			},
			{ xtype: 'panel'
			, unstyled: true
			, width: 700
			, html: this.instruction
			, cls: 'InstructionContainer-Text'
			, buttons: [
				{ xtype: 'button'
				, name: 'action'
				, id: 'tf-'+this.taskId+'-action-0'
				, text: 'Done'
				, value: 1
				, disabled: false
				, iconCls: 'icon-ok'
				}	
			]
			}
		]});
		
		itasks.ttc.InstructionContainer.superclass.initComponent.apply(this,arguments);
		
		if(this.context != null){
			this.insert(1,{
				xtype: 'panel',
				width: 700,
				//html: '<div class="task-description">Context Information:</div>'+this.context,
				html: this.context,
				unstyled: true,
				cls: 'InstructionContainer-Context'
			});
		}
	},
	
	afterRender: function(){
		itasks.ttc.InstructionContainer.superclass.afterRender.call(this,arguments);
		//attachTaskHandlers is moved to file 'TTCCommon.js'		
		itasks.ttc.common.attachTaskHandlers(this);
	},
	
	addUpdate: function(name, value) {
		this.taskUpdates[name] = value;
	},
	
	sendUpdates: function(delay) {
		if(delay) {
			new Ext.util.DelayedTask().delay(250,this.sendUpdates,this);
		} else {
			var wt = this.findParentByType(itasks.WorkPanel);
			if(!wt) return;
			
			wt.sendTaskUpdates(this.taskId,this.taskUpdates);
		
			this.taskUpdates = {};
		}
	},
		
	update: function(data){}
});

Ext.reg('itasks.ttc.instruction',itasks.ttc.InstructionContainer);