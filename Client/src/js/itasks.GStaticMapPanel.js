Ext.ns("itasks");

itasks.GStaticMapPanel = Ext.extend( Ext.Panel, {

	initComponent : function(){
		
		Ext.apply(this,
			{ border: false
			, autoHeight: false
			, html: '<img src="'+this.url+'" width="'+this.width+'" height="'+this.height+'">'
			});

		itasks.GMapPanel.superclass.initComponent.apply(this,arguments);
		
		this.show();
	}
});

Ext.reg('itasks.gstaticmappanel', itasks.GStaticMapPanel);