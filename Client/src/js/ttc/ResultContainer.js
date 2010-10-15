Ext.ns('itasks.ttc');

itasks.ttc.ResultContainer = Ext.extend(itasks.ttc.TTCBase,{
	initComponent: function() {
		
		this.cls = 'TTCResultContainer';
		this.description = this.result;
		
		itasks.ttc.ResultContainer.superclass.initComponent.apply(this,arguments);
	}
});

Ext.reg('itasks.ttc.result',itasks.ttc.ResultContainer);