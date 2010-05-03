Ext.ns('itasks.tui');

itasks.tui.AppletContainer = Ext.extend(Ext.Panel,{
	
	initCompent : function(){
	
	
		itasks.tui.AppletContainer.superclass.initComponent.apply(this,arguments);
		
		this.bubble(function(ct){ if(this.xtype == 'itasks.ttc.form') this.fullscreen = true });
	}
});

Ext.reg('itasks.tui.Applet', itasks.tui.AppletContainer);