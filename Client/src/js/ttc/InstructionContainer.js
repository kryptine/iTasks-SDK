Ext.ns('itasks.ttc');

itasks.ttc.InstructionContainer = Ext.extend(Ext.Panel,{
	initComponent: function() {
		
		Ext.apply(this,
		{ cls: 'InstructionContainer'
		, unstyled: true
		, taskUpdates : {}
		, items: [
			{ xtype: 'itasks.ttc.common.description'
			, cls: 'InstructionContainer-Description'
			, description: this.label
			, headerButton: this.headerButton
			},
			{ xtype: 'panel'
			, unstyled: true
			, html: this.instruction
			, cls: 'InstructionContainer-Text'
			/*
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
			*/
			}
		]});
		if(this.context != null){
			this.items[2] = {
				xtype: 'panel',
				html: this.context,
				unstyled: true,
				cls: 'InstructionContainer-Context'
			};
		}
		Ext.apply(this.items[this.items.length - 1],{
			buttons: [{ xtype: 'button'
				, name: 'action'
				, id: 'tf-'+this.taskId+'-action-0'
				, text: 'Done'
				, value: 1
				, disabled: false
				, iconCls: 'icon-ok'
				}]
		});
		
		itasks.ttc.InstructionContainer.superclass.initComponent.apply(this,arguments);
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