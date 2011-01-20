Ext.ns('itasks.tui');

itasks.tui.AppletControl = Ext.extend(Ext.Panel,{
	initComponent : function(){
		this.appletId = Ext.id(null, 'applet');

		Ext.apply(this,
			{ url: "/handlers/work/tab"
			, border: false
			, autoHeight: false
			, bodyCfg: {
				id: this.appletId,
				tag: 'applet',
				code: this.appletcode,
				archive: this.archives.join(','),
				width: this.width,
				height: this.height,
				mayscript: 'true',
				children: [ { tag: 'param',
							  name: 'id',
							  value: this.id },
							{ tag: 'param',
						      name: 'name',
							  value: this.name },
							{ tag: 'param',
							  name: 'value',
							  value: Ext.util.Format.htmlEncode(this.value) } ]
				}
			});			
		
		itasks.tui.AppletControl.superclass.initComponent.apply(this,arguments);

		this.addEvents('tuichange');
		this.enableBubble('tuichange');

		var appletControl = this;
		this.bubble(function(ct){ 
			if (this.xtype == 'itasks.ttc.form') {
				this.on ('resize', 
					function(panel, w, h) {
						appletControl.setHeight(this.getHeight() - 90);
					});
			}
		});
	},
	
	setValue : function(_data){
		var applet = document.getElementById(this.appletId);
		applet.setValue(_data);
	},
	
	afterRender : function(){
		itasks.tui.AppletControl.superclass.afterRender.call(this);
		
		(function(){
			this.setError(this.errorMsg);
			this.setHint(this.hintMsg);
		}).defer(50,this);
	},
	
	setError: function(msg){		
		(function() {
            var applet = document.getElementById(this.appletId);
            applet.setError(msg);
		}).defer(50,this);
	},
	
	setHint: function(msg){
		(function() {
            var applet = document.getElementById(this.appletId);
            applet.setHint(msg);
		}).defer(50,this);
	}
});

Ext.reg('itasks.tui.Applet', itasks.tui.AppletControl);
